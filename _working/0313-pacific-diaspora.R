# This script draws some charts of the diaspora of Pacific island countries and territories.
# It's pretty rough and certainly incomplete. The approach was to use the census figures
# for resident population of Pacific islander ancestry currently living in USA, Australia
# and New Zealand; and compare that to populations resideing in the countries themselves.
#
# All sorts of known limitations which we are prepared to live with for these crude comparisons:
# - different reference years (2025 for populations, and census years are 2018, 2020 and 2021)
# - populations residing in the Pacific islands themselves are all ethnicities (e.g. will include
#   Australian-descent people rsideing in those countries), haven't bothered to limit to just "true" Tongans, Samoans, etc
# - not comprehensive e.g. I know there are some Pitcairn-descended people in UK but haven't included them. And of course
#   there must be many others of these people in countries other than Australia, NZ and USA
# - France not included at all. No ancestry data in French censuses so this would be tricky.
#
# Peter Ellis 2025-11

#---------------------Data prep-------------------------

library(tidyverse)
library(rsdmx)
library(ISOcodes)

# Current populations of PICTs:
pops <- rsdmx::readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A..MIDYEARPOPEST._T._T?startPeriod=2025&endPeriod=2025&dimensionAtObservation=AllDimensions") |> 
  as_tibble() |> 
  left_join(select(ISOcodes::ISO_3166_1, Alpha_2, pict = Name), by = c("GEO_PICT" = "Alpha_2")) |> 
  select(pict, pop = obsValue) |> 
  drop_na()

# out of interest what is the total population of all PICTs, Austrlaia and NZ together? (about 47m):
picts_and_anz <- c(sum(pops$pop), 28.1e6, 5.3e6)
sum(picts_and_anz)

# https://tools.summaries.stats.govt.nz/ethnic-group/tongan and similar for 2023 NZ figure
# table builder for Australian 2021 figures - see `https://raw.githubusercontent.com/ellisp/blog-source/refs/heads/master/data/total%20by%20pacific.csv`
# Wikipedia for US figures, from 2020 census. Search for e.g. "Palauans in USA wikipedia"
diaspora <- tribble(~pict, ~dest, ~people,
                    "Tonga",            "New Zealand", 97824,
                    "Niue",             "New Zealand", 34944,
                    "Tokelau",          "New Zealand", 9822,
                    "Cook Islands",     "New Zealand", 94176,
                    "Samoa",            "New Zealand", 213069,
                    "Tuvalu",           "New Zealand", 6585,
                    "Fiji",             "New Zealand", 25038 + 23808, # includes Fijian Indian
                    "Papua New Guinea", "Australia", 22668,
                    "Vanuatu",          "Australia", 2380,
                    "Solomon Islands",  "Australia", 2704,
                    "Kiribati",         "Australia", 1263,
                    "Fiji",             "Australia", 48354,
                    "Nauru",            "Australia", 571,
                    "Cook Islands",     "Australia", 27494,
                    "Tokelau",          "Australia", 2544,                    
                    "Tonga",            "Australia", 43469,
                    "Niue",             "Australia", 6225,
                    "Samoa",            "Australia", 98022,
                    "Tuvalu",           "Australia", 995,
                    "Pitcairn",         "Australia", 1123,
                    "Marshall Islands", "USA", 52624, # 47300 if just 'alone' 
                    "Palau",            "USA", 12202,
                    "Micronesia, Federated States of", "USA", 21596)

# Australia checked
# New Zealand checked
# USA checked


#--------------------------Bar chart------------------------
pops_with_prop <- pops |> 
  inner_join(diaspora) |> 
  mutate(pict = gsub("Federated States of", "Fed. St.", pict)) |> 
  group_by(pict, pop) |> 
  summarise(Overseas = sum(people)) |>
  ungroup() |> 
  mutate(prop = Overseas / (pop + Overseas)) |> 
  mutate(pict = fct_reorder(pict, prop))

p2 <- pops_with_prop |> 
  select(-prop) |> 
  rename(`Origin country` = pop) |> 
  gather(variable, value, -pict) |> 
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_col(width = 0.8) +
  facet_wrap(~pict, scales = "free_y") +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c("steelblue", "brown")) +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(colour = "grey50")) +
  labs(x = "", y ="Number of people",
        title = "Pacific Islander diaspora, arranged from lowest proportion overseas to highest",
        subtitle = "Diaspora is a lower bound of full figure as it is based on just Australia, USA and New Zealand censuses.",
       caption = "Source: PDH.Stat for populations; Australian, USA and New Zealand Censuses for diaspora.")

svg_png(p2, "../img/0313-diaspora-bar", w = 10, h = 7)





#-----------------------Scatter plot------------------------

p1 <- pops |> 
  inner_join(diaspora) |> 
  ggplot(aes(x = pop, y = people, label = pict, colour = dest)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey") +
  geom_point() +
  geom_text_repel(size = 2.5) +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  scale_colour_manual(values = c("blue", "black", "darkred")) +
  labs(x = "People living in origin country in 2025",
       y = "Diaspora in another country, recent census",
       colour = "Disapora country",
      title = "Pacific Island home population and diaspora in various countries")

svg_png(p1, "../img/0313-diaspora-scatter", w = 8, h = 6)
