library(tidyverse)
library(glue)
library(janitor)
library(readxl)
library(countrycode)

# download from the Global Database screen of the SDGs database,
# https://unstats.un.org/sdgs/dataportal/database
# select all indicators, all countries, all periods. Size of zip file is about 250MB
# and it contains an Excel workbook (with one 'data' sheet) for each of the 17 SDGs.
unzip("../data/20250925043752329_petere@spc.int.zip")

fns <- paste0("Goal", 1:17, ".xlsx")

# A lot of the columns are nearly all empty and have critical info on dimensions (eg Sex, Severity of price levels)
# but we don't need them if we just want to count number of distinct Indicator-TimePeriod observations that have a 
# non-NA Value

# this takes a long time:
d <- fns |> 
  lapply(read_excel, sheet = "data", range = cell_cols("A:I")) |> 
  bind_rows()


picts <- c("Papua New Guinea", "Solomon Islands", "Vanuatu", "Fiji", "New Caledonia",
            "Tonga", "Samoa", "Cook Islands", "French Polynesia", "Tuvalu", "Niue", "Tokelau", "Pitcairn", "American Samoa", "Wallis and Futuna Islands",
            "Marshall Islands", "Palau", "Guam", "Micronesia (Federated States of)", "Northern Mariana Islands", "Nauru", "Kiribati")

stopifnot(length(picts) == 22)
stopifnot(length(picts[!picts %in% unique(d2$GeoAreaName)]) == 0)

# Indicators that were chosen as priorities for the Pacific
pict_priorities <- c("1.1.1", "1.2.1")

d2 <- d |> 
  filter(!is.na(Value)) |> 
  distinct(Goal, Target, Indicator, TimePeriod, GeoAreaCode, GeoAreaName) |> 
  drop_na()

d2 |> 
  distinct(GeoAreaName) |> 
  filter(!grepl("Large Marine", GeoAreaName)) |> 
  filter(!grepl("FAO Major Fishing", GeoAreaName)) |> 
  mutate(iso3c = countrycode(sourcevar = GeoAreaName, origin = "country.name", destination = "iso3c")) |> 
  mutate(is_pict = GeoAreaName %in% picts) |>
  drop_na() |> 
  View()


d_countries <- d2 |> 
  filter(!grepl("Large Marine", GeoAreaName)) |> 
  filter(!grepl("FAO Major Fishing", GeoAreaName)) |> 
  mutate(iso3c = countrycode(sourcevar = GeoAreaName, origin = "country.name", destination = "iso3c")) |> 
  mutate(is_pict = GeoAreaName %in% picts) |>
  filter(!is.na(iso3c)) 

country_summary <- d_countries |> 
  count(iso3c, Indicator) |> 
  complete(iso3c, Indicator, fill = list(n = 0)) |> 
  group_by(iso3c) |> 
  summarise(indicators = n(), 
           with_zero = sum(n == 0),
           at_least_one = sum(n >= 1),
           more_than_one = sum(n > 1)) |> 
  ungroup() |> 
  mutate(prop_at_least_one = at_least_one / indicators,
         prop_at_least_two = more_than_one / indicators) |> 
  left_join(distinct(d_countries, GeoAreaName, is_pict, iso3c), by = "iso3c") |> 
   arrange(prop_at_least_one)

country_summary |> 
  filter(is_pict)

country_summary |> 
  filter(is_pict) |> 
  summarise(mean(prop_at_least_one),
             mean(prop_at_least_two))
