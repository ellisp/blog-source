library(tidyverse)
library(glue)
library(janitor)
library(readxl)
library(countrycode)
library(rsdmx)


#----------Pacific population data for use later--------------------
pict_pops <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A.FJ+NC+PG+SB+VU+GU+KI+MH+FM+NR+MP+PW+AS+CK+PF+NU+PN+WS+TK+TO+TV+WF.MIDYEARPOPEST._T._T?dimensionAtObservation=AllDimensions") |> 
  as_tibble() |> 
  clean_names() |> 
  filter(time_period == 2025) |> 
  mutate(iso3c = countrycode(geo_pict, origin = "iso2c", destination = "iso3c")) |> 
  select(population = obs_value, iso3c) 
#-------------SDGs data------------

# download from the Global Database screen of the SDGs database,
# https://unstats.un.org/sdgs/dataportal/database
# select all indicators, all countries, all periods. Size of zip file is about 250MB
# and it contains an Excel workbook (with one 'data' sheet) for each of the 17 SDGs.

if(!exists("Goal17.xlsx")){
  unzip("../data/20250925043752329_petere@spc.int.zip")
}

fns <- paste0("Goal", 1:17, ".xlsx")

# A lot of the columns are nearly all empty and have critical info on dimensions (eg Sex, Severity of price levels)
# but we don't need them if we just want to count number of distinct Indicator-TimePeriod observations that have a 
# non-NA Value

if(file.exists("raw_sdgs.rda")){
  load(raw_sdgs)
} else {
  # this takes a long time:
  raw_sdgs <- fns |> 
    lapply(read_excel, sheet = "data", range = cell_cols("A:I")) |> 
    bind_rows()
  save(raw_sdgs, file = "raw_sdgs.rda")
}


goals <- tibble(Goal = 1:17, goal_text = c("No poverty", "Zero hunger", "Good health and well-being", "Quality education",
            "Gender equality", "Clean water and sanitation", "Affordable and clean energy",
          "Decent work and economic growth", "Industry, innovation and infrastructure", "Reduced inequalities",
          "Sustainable cities and communities", "Responsible consumption and production", "Climate action",
          "Life below water", "Life on land", "Peace, justice and strong institutions", "Partnerships for the goals"))

indicator_lookup <- distinct(raw_sdgs, Indicator, SeriesDescription)

picts <- c("Papua New Guinea", "Solomon Islands", "Vanuatu", "Fiji", "New Caledonia",
            "Tonga", "Samoa", "Cook Islands", "French Polynesia", "Tuvalu", "Niue", "Tokelau", "Pitcairn", "American Samoa", "Wallis and Futuna Islands",
            "Marshall Islands", "Palau", "Guam", "Micronesia (Federated States of)", "Northern Mariana Islands", "Nauru", "Kiribati")

stopifnot(length(picts) == 22)
stopifnot(length(picts[!picts %in% unique(raw_sdgs$GeoAreaName)]) == 0)

shared_sovereignty <- c("New Caledonia", "Cook Islands", "French Polynesia", "Niue", "Tokelau", 
            "Pitcairn", "American Samoa", "Wallis and Futuna Islands",
            "Guam", "Northern Mariana Islands")


# Indicators that were chosen as priorities for the Pacific
pict_priorities <- c("
  1.1.1 1.2.1 1.2.2 1.3.1 1.4.1
  2.1.1 2.2.1 2.2.2 2.3.2 2.3.1 2.5.1 2.a.1
  3.1.1 3.1.2 3.2.1 3.2.2 3.3.2 3.3.3 3.3.5 3.4.1 3.5.2 
    3.7.1 3.7.2 3.8.1 3.9.2 3.a.1 3.c.1 3.d.1
  4.1.1 4.2.2 4.3.1 4.5.1 4.6.1 4.7.1 4.a.1 4.c.1
  5.1.1 5.2.1 5.2.2 5.3.1 5.4.1 5.5.1 5.5.2 5.6.1 5.6.2 
    5.a.2 5.b.1 5.c.1
  6.1.1 6.2.1 6.3.1
  7.1.1 7.2.1 7.a.1 7.b.1
  8.1.1 8.3.1 8.5.1 8.5.2 8.6.1 8.9.1 8.9.2 8.10.2 8.a.1
  9.2.2 9.a.1 9.c.1
  10.1.1 10.2.1 10.4.1 10.6.1 10.7.2 10.b.1 10.c.1
  11.1.1 11.4.1 11.5.1 11.5.2 11.6.1 11.b.2
  12.4.1 12.4.2 12.5.1 12.b.1
  13.1.2 13.2.1 13.3.1 13.a.1 13.b.1
  14.1.1 14.2.1 14.3.1 14.4.1 14.5.1 14.6.1 14.7.1 14.a.1 14.b.1
  15.1.1 15.1.2 15.5.1 15.6.1 15.7.1 15.8.1
  16.1.3 16.3.1 16.6.1 16.7.1 16.7.2 16.9.1 16.10.2 16.a.1
  17.1.1 17.1.2 17.2.1 17.3.1 17.3.2 17.4.1 17.6.1 17.7.1 17.8.1 
    17.9.1 17.11.1 17.14.1 17.15.1 17.16.1 17.17.1 17.18.1 17.18.2 
    17.18.3 17.19.1 17.19.2
") |> 
  str_squish() |> 
  str_split(pattern = " ") |> 
  unlist()

# Note: changed 17.6.2 to 17.6.1. 17.6.2 not in the official metadata, and 17.6.1 seems to 
# be the right one anyway: "Fixed broadband subscriptions per 100 inhabitants, by speed"

# there should be one missing indicator, 5.2.2 which seems to have no data globally
# "Proportion of owmen and girls aged 15 years and older subjected to sexual violence by 
# persons other than an intimate partner..."
stopifnot(length(pict_priorities[!pict_priorities %in% unique(raw_sdgs$Indicator)]) <= 1)


d2 <- raw_sdgs |> 
  filter(!is.na(Value)) |> 
  distinct(Goal, Target, Indicator, TimePeriod, GeoAreaCode, GeoAreaName) |> 
  drop_na()

d_countries <- d2 |> 
  # filter out some geo areas that tend to get given codes they shouldn't 
  # (because eg 'Australia' is part of the area name):
  filter(!grepl("Large Marine", GeoAreaName)) |> 
  filter(!grepl("FAO Major Fishing", GeoAreaName)) |> 
  # some other exclusions of multiple entries:
  filter(!grepl("Eastern Asia", GeoAreaName)) |> 
  filter(!grepl("Southern Asia", GeoAreaName)) |> 
  filter(!grepl("Northern Africa", GeoAreaName)) |> 
  filter(!grepl("Sudan [former]", GeoAreaName, fixed = TRUE)) |> 
  # next one is individual entries for England, Wales, Scotland:
  filter(!grepl("United Kingdom (", GeoAreaName, fixed = TRUE)) |>
  filter(!grepl("Iraq (", GeoAreaName, fixed = TRUE)) |> 
  filter(!grepl("Tanzania (Zanzibar", GeoAreaName, fixed = TRUE)) |> 
  # returns lots of warnings, for things like "Africa", "Oceania", "Yugoslavia [former]", etc - 
  # all ok as we only want countries:
  mutate(iso3c = countrycode(sourcevar = GeoAreaName, origin = "country.name", destination = "iso3c")) |> 
  mutate(is_pict = GeoAreaName %in% picts,
         is_pict_priority = Indicator %in% pict_priorities) |>
  filter(!is.na(iso3c)) 


# Check if we have an ISO codes that have been assigned to more than one GeoAreaName:
bad_iso <- distinct(d_countries, GeoAreaName, is_pict, iso3c) |> count(iso3c, sort = TRUE) |> 
  filter(n > 1) |> 
  pull(iso3c)

distinct(d_countries, GeoAreaName, iso3c) |> 
  filter(iso3c %in% bad_iso)

stopifnot(length( bad_iso) == 0)

# Country summary, proprotion of indicators with data against them
country_summary <- d_countries |> 
  filter(is_pict_priority) |>
  # after this next operation, n will be the number of rows in the data with non-NA 
  # values for each indicators (counting one for each year):
  count(iso3c, Indicator) |>
  # Add in rows with n = 0 for indicator-country combinations that don't exist
  complete(iso3c, Indicator, fill = list(n = 0)) |> 
  group_by(iso3c) |> 
  # for each country, count the indicators and how many have values:
  summarise(indicators = n(), 
           with_zero = sum(n == 0),
           at_least_one = sum(n >= 1),
           more_than_one = sum(n > 1)) |> 
  ungroup() |> 
  # convert to proportions:
  mutate(prop_at_least_one = at_least_one / indicators,
         prop_at_least_two = more_than_one / indicators) |> 
  left_join(distinct(d_countries, GeoAreaName, is_pict, iso3c), by = "iso3c") |> 
   arrange(prop_at_least_one) |> 
  select(GeoAreaName, iso3c, prop_at_least_one, prop_at_least_two, is_pict) |> 
   mutate(GeoAreaName = fct_reorder(GeoAreaName, prop_at_least_one))


pict_summary <- country_summary |> 
  filter(is_pict) |> 
  left_join(pict_pops, by = "iso3c") |> 
  mutate(status = ifelse(GeoAreaName %in% shared_sovereignty, "Shared sovereignty", "UN member"),
         status = fct_relevel(status, "UN member"))

pal <- spcstyle::spc_cols(1:2)

p1 <- pict_summary |> 
  select(GeoAreaName, 
         `At least one` = prop_at_least_one,
        `At least two` = prop_at_least_two) |> 
  gather(variable, value, -GeoAreaName) |> 
  mutate(variable = fct_relevel(variable, "At least one")) |> 
  ggplot(aes(y = GeoAreaName)) +
  geom_segment(data = pict_summary, aes(x = prop_at_least_one, xend = prop_at_least_two), colour = "steelblue") +
  geom_point(aes(x = value, colour = variable, shape = variable), size = 4) +
  scale_x_continuous(label = percent, limits = c(0, 1)) +
  scale_colour_manual(values = pal) +
  theme(legend.position = c(0.2, 0.8)) +
  labs(colour = "", shape = "", y = "",
        x = "Proportion of priority SDG indicators",
      title = "Sustainable Development Goal data availability for island members of the Pacific Community",
      subtitle = "Proportion of the 132 indicators selected by the Pacific SDG Taskforce that have at least one or 
at least two years' of data.")

svg_png(p1, "../img/0306-pict-dumbell", w = 9, h = 6)


country_summary |> 
  group_by(is_pict) |> 
  summarise(mean(prop_at_least_one),
             mean(prop_at_least_two))


country_inds <- d_countries |> 
  filter(is_pict_priority) |> 
  count(Goal, iso3c, Indicator) |> 
  group_by(Goal) |> 
  complete(iso3c, Indicator, fill = list(n = 0)) |> 
  group_by(Goal, iso3c) |> 
  summarise(indicators = n(), 
           with_zero = sum(n == 0),
           at_least_one = sum(n >= 1),
           more_than_one = sum(n > 1)) |> 
  ungroup() |> 
  mutate(prop_at_least_one = at_least_one / indicators,
         prop_at_least_two = more_than_one / indicators) |> 
  left_join(distinct(d_countries, GeoAreaName, is_pict, iso3c), by = "iso3c") |> 
   arrange(prop_at_least_one) |> 
  select(Goal, indicators, GeoAreaName, iso3c, prop_at_least_one, prop_at_least_two, is_pict) |> 
   mutate(GeoAreaName = fct_reorder(GeoAreaName, prop_at_least_one))


p2 <- country_inds |> 
  group_by(Goal, is_pict) |> 
  summarise(p1 = mean(prop_at_least_one)) |> 
  spread(is_pict, p1) |>
  rename('Pacific island' = `TRUE`,
        Other = `FALSE`) |>
  left_join(goals, by = "Goal") |> 
  ggplot(aes(x = `Pacific island`, y = Other, label = Goal)) +
  geom_abline(slope = 1, intercept = 0, colour = "steelblue") +
  coord_equal() +
  theme(axis.line = element_line(colour = "grey70")) +
  labs(x = "Pacific Island countries and territories (SPC members)",
       y = "Non-Pacific island countries",
       title = "Sustainable Development Goal data by Goal",
       subtitle = "Proportion of the 132 indicators selected by the Pacific SDG Taskforce that have at least one observation.")

p2a <-  p2 +
  geom_text(size = 4) +
  scale_x_continuous(label = percent, limits = 0:1) +
  scale_y_continuous(label = percent, limits = 0:1) +
   annotate("text", x = c(0.63, 0.00), y = c(0.54, 0.54), label = c("Pacific has more data", "Pacific has less data"), 
               hjust = 0, colour = "steelblue", size = 4, fontface = "italic")

p2b <-  p2 +
  geom_label(size = 5, fontface = "bold", fill = "grey95", label.size = 0) +
  geom_text_repel(aes(label = goal_text), colour = "darkgreen", size = 3, alpha = 0.5) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
   annotate("text", x = c(0.83, 0.61), y = c(0.78, 0.78), label = c("Pacific has more data", "Pacific has less data"), 
               hjust = 0, colour = "steelblue", size = 4, fontface = "italic")


svg_png(p2a, "../img/0306-goal-comparison", w = 9, h = 6)
svg_png(p2b, "../img/0306-goal-comparison-zoom", w = 9, h = 6)


p3 <- pict_summary |> 
  ggplot(aes(x = population, y = prop_at_least_one)) +
  geom_point(aes(colour = status), size = 2.5) +
  geom_text_repel(aes(label = GeoAreaName), family = "Roboto", size = 3, colour = "grey50") +
  scale_x_log10(label = comma) +
  scale_y_continuous(label = percent) +
  scale_colour_manual(values = spcstyle::spc_cols(c(2,3))) +
  labs(x = "Population in 2025",
       y = "Proportion of SDG indicators with at least one observation for each country",
      colour = "",
    title = "Larger countries generally have more SDG data") +
  theme(axis.line = element_line(colour = "grey70"))

svg_png(p3, "../img/0306-pop-v-sdg-data", w = 9, h = 6)
