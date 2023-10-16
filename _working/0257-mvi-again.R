# This is a comparison of membership of the V20 with the MVI and GDP per capita

#------------Data prep-----------
library(WDI)
library(kableExtra)
library(clipr)

# run the script from the previous blog, to load in the MVI data in particular,
# and to load in all the standard R packages (tidyverse in particular)
source("0255-mvi-pacific.R")

# get the world Banks' GDP data

# 	"GDP per capita, PPP (constant 2017 international $)":
gdp_ppp_pc <- WDI(indicator = "NY.GDP.PCAP.PP.KD", start = 2000) |>
  drop_na() |>
  group_by(iso3c) |>
  # just get the latest year for each country:
  arrange(desc(year)) |>
  slice(1) |>
  ungroup() |>
  select(iso3c,
          gdp_year = year,
         gdp_pc = NY.GDP.PCAP.PP.KD)

# List of members of the V20 taken from
# https://www.v-20.org/v20-ministerial-dialogue-xi-communique#_ftnref5
v20 <- c("Afghanistan", "Bangladesh", "Barbados", "Benin", "Bhutan", "Burkina Faso",
  "Cambodia", "Chad", "Colombia", "Comoros", "Costa Rica", "Côte D'Ivoire", 
  "Democratic Republic of the Congo", "Dominica", "Dominican Republic",
  "Eswatini", "Ethiopia", "Fiji", "Gambia", "Ghana", "Grenada", "Guatemala",
  "Guinea", "Guyana", "Haiti", "Honduras", "Jordan", "Kenya", "Kiribati",
  "Kyrgyzstan", "Lebanon", "Liberia", "Madagascar", "Malawi", "Maldives",
  "Marshall Islands", "Mongolia", "Morocco", "Mozambique", "Namibia", "Nepal",
  "Nicaragua", "Niger", "Pakistan", "Palau", "Palestine**", "Papua New Guinea",
  "Paraguay", "Philippines", "Rwanda", "Saint Lucia", "Samoa", "Senegal",
  "Sierra Leone", "South Sudan", "Sri Lanka", "Sudan", "United Republic of Tanzania",
  "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Tuvalu",
  "Uganda", "Vanuatu", "Viet Nam", "Yemen")
# A few hand edits were made so country names match those in the MVI (eg
# D'Ivoire rather than d'Ivoire, etc)

# any non-matches? Yes, Palestine:
v20[!v20 %in% mvi$Country]
# Palestine not in the mvi.

# Mark which countries are in the V20, and join to the GDP data:
mvi2 <- mvi |>
  mutate(type = ifelse(Country %in% v20, "Member of V20", "Not a member of V20")) |>
  left_join(gdp_ppp_pc, by = c("ISO" = "iso3c"))

# note there are seven countries with no GDP PPP per capita values:
filter(mvi2, is.na(gdp_pc))

#===============comparisons============


#---------Table------------
# Average values by v20 membership or not:
mvi2 |>
  group_by(type) |>
  summarise(`Structural vulnerability` = median(svi),
            `Lack of structural resilience` = median(lsri),
            `Multidimensional vulnerability` = median(`MVI - Score`),
            `GDP per capita, PPP` = dollar(median(gdp_pc, na.rm = TRUE), accuracy = 100)) |>
  rename(`  ` = type) |>
  kable(digits = 1, format.args = list(big.mark = ","), align = c("l", "r", "r", "r", "r")) |>
  kable_styling() |>
  write_clip()

# V20 members are very slightly more vulnerable, and notably poorer,
# than non-members


#-------------Plot-------------------
ff = "Roboto"
pal <- c("Member of V20" = "darkred","Not a member of V20" = "darkblue")

p3 <- mvi2 |>
  ggplot(aes(x = gdp_pc, y = `MVI - Score`, colour = type)) +
  geom_point() +
  geom_text_repel(data = filter(mvi2, (`MVI - Score` < 50 & type == "Member of V20")|
                                  (`MVI - Score` > 60 & type != "Member of V20")),
                  aes(label = Country), size = 2.8, seed = 123) +
  annotate("label", x = 800, y = 37, colour = pal[1], fontface = "italic",
           label.size = 0, fill = "grey90", hjust = 0,
           label = "Member of V20 but lower than average vulnerability according to MVI") +
  annotate("label", x = 800, y = 73, colour = pal[2], fontface = "italic",
           label.size = 0, fill = "grey90", hjust = 0,
           label = "Not a member of V20 but high vulnerability according to MVI") +
  scale_x_log10(label = dollar) +
  scale_colour_manual(values = pal) +
  labs(x = "GDP per capita, PPP, 2017 prices",
       y = "Multidimensional Vulnerability Index",
       colour = "",
       title = "Membership of the 'V20' group of vulnerable coutries", 
       subtitle = "Compared to scores on the UN's Multidimensional Vulnerability Index (MVI)",
       caption = "Source: UN https://www.un.org/ohrlls/mvi (MVI), World Bank World Development Indicators (GDP), analysis by freerangestats.info
Palestine is a member of V20 but does not have a MVI. Seven countries have an MVI but are missing GDP data.")

svg_png(p3, "../img/0257-discrepancies", w = 9, h = 7)


#---------------not used------------------
# Decided this wasn't that interesting:
# p1 <- mvi2 |>
#   ggplot(aes(x = svi, y = lsri, colour = type)) +
#   geom_point() +
#   scale_colour_manual(values = pal)


# Decided this plot not very interesting, doesn't label interesting countries so
# have not polished it.
# p2 <- mvi2 |>
#   ggplot(aes(x = gdp_pc, y = `MVI - Score`, colour = type)) +
# #  geom_smooth(method = "lm", se = TRUE) +
#   geom_point() +
#   geom_text_repel(data = filter(mvi2, `MVI - Score` < 40 |
#                                   `MVI - Score` > 65 |
#                                   gdp_pc > 50000 |
#                                   gdp_pc < 2000),
#                   aes(label = Country)) +
#   scale_x_log10(label = dollar) +
#   scale_colour_manual(values = pal)


