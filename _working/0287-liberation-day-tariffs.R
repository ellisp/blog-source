
library(tidyverse)
library(readxl)
library(janitor)
library(WDI)

#-------------------GDP-----------------------

# WDIsearch("gdp") |> View()


gdp <- WDI(indicator = c(gdp = "NY.GDP.MKTP.CD"), start = 2020) |> 
  as_tibble() |>
  # get the latest value for each country
  filter(!is.na(gdp)) |> 
  group_by(country) |> 
  arrange(desc(year)) |> 
  slice(1) |> 
  ungroup()

# note these will normally be 2023 or 2022, so when we compare later to 2024
# trade it is not like for like, but ok for a rough comparison.

count(gdp, year)

#---------------this data is too old---------------
# download USA 2022 trade data from World Bank  World Integrated Trade Solution
# https://wits.worldbank.org/CountryProfile/en/Country/USA/Year/2022/TradeFlow/EXPIMP/Partner/by-country

wits <- read_excel("WITS-Partner.xlsx", sheet = "Partner") |> 
  clean_names()

# export is US exports to partner
# import is US imports from partner
wits |> 
  mutate(deficit = (import_us_thousand - export_us_thousand) / 1000,
         total_trade = (export_us_thousand + import_us_thousand) / 1000) |> 
  mutate(imbalance_wh_method = deficit / total_trade) |> 
  select(partner_name, deficit, total_trade, imbalance_wh_method, export_us_thousand, import_us_thousand) |> 
  arrange(desc(imbalance_wh_method)) |> 
  filter(partner_name %in% c("China", "New Zealand", "Australia"))

# ahs is the applied tariff rate
actual_tariffs <- wits |> 
  select(partner_name, ahs_weighted_average_percent) |> 
  bind_rows(tibble(
    partner_name = "European Union",
    # seems disupted what this number should be, but it is low
    ahs_weighted_average_percent = 2.31
  )) |> 
  mutate(partner_name = case_when(
    partner_name == "Russian Federation" ~ "Russia",
    partner_name == "Korea, Dem. Rep." ~ "Korea, North",
    partner_name == "Korea, Rep." ~ "Korea, South",
    grepl("Anguil", partner_name) ~ "Anguilla",
    grepl("Bahamas", partner_name) ~ "Bahamas",
    grepl("British Indian Ocean Ter", partner_name) ~ "British Indian Ocean Territories",
    grepl("Cape Verde", partner_name) ~ "Cabo Verde",
    grepl("Curaçao", partner_name) ~ "Curacao",
    grepl("East Timor", partner_name) ~ "Timor-Leste",
    grepl("Egypt", partner_name) ~ "Egypt",
    grepl("Ethiopia", partner_name) ~ "Ethiopia",
    grepl("Faeroe Islands", partner_name) ~ "Faroe Islands",
    grepl("Falkland Island", partner_name) ~ "Falkland Islands (Islas Malvinas)",
    grepl("Gambia", partner_name) ~ "Gambia",
    grepl("Heard Island", partner_name) ~ "Heard and McDonald Islands",
    grepl("Holy See", partner_name) ~ "Vatican City",
    grepl("Hong Kong", partner_name) ~ "Hong Kong",
    grepl("^Iran", partner_name) ~ "Iran",
    grepl("Kyrgyz", partner_name) ~ "Kyrgyzstan",
    grepl("Lao PDR", partner_name) ~ "Laos",
    grepl("Macao", partner_name) ~ "Macau",
    grepl("Micronesia", partner_name) ~ "Micronesia",
    grepl("Myanmar", partner_name) ~ "Burma",
    grepl("North Macedonia", partner_name) ~ "Macedonia",
    # this treats all occupied territories as Gaza, leaves West Bank out
    grepl("Occ.Pal.Terr", partner_name) ~ "Gaza Strip Administered by Israel",
    grepl("Pitcairn", partner_name) ~ "Pitcairn Islands",
    grepl("Saint Helena", partner_name) ~ "St Helena",
    grepl("Saint Pierre", partner_name) ~ "St Pierre and Miquelon",
    grepl("Serbia", partner_name) ~ "Serbia",
    grepl("Slovak", partner_name) ~ "Slovakia",
    grepl("St. Kitts", partner_name) ~ "St Kitts and Nevis",
    grepl("St. Lucia", partner_name) ~ "St Lucia",
    grepl("St. Vincent", partner_name) ~ "St Vincent and the Grenadines",
    grepl("Turks and Caicos", partner_name) ~ "Turks and Caicos Islands",
    grepl("Wallis and Fut", partner_name) ~ "Wallis and Futuna",
    grepl("Syrian", partner_name) ~ "Syria",
    grepl("Saint Maarten", partner_name) ~ "Sint Maarten",
    grepl("Fr. So. Ant. Tr", partner_name) ~ "French Southern and Antarctic Lands",
    TRUE ~ partner_name
  )) |> 
  rename(Country = partner_name)

filter(actual_tariffs, grepl("China", Country))
filter(actual_tariffs, grepl("Russia", Country))
filter(actual_tariffs, grepl("Korea", Country))

#--------more definitive 2024 data from US itself------


uscb <- read_csv("../data/Exports & Imports by NAICS Commodities by country 2024.csv", skip = 5) |> 
  mutate(exports_m = `Total Exports Value ($US)` / 1e6,
         imports_m = `Customs Import Value (Gen) ($US)` / 1e6,
         balance_m = `Balance ($US)` / 1e6,
         check = exports_m -imports_m - balance_m) |> 
  filter(Country != "World Total") |> 
  # The White House's imbalance measure or 'tariff against us' (isn't really a tariff):
  mutate(imbalance_measure_wh = balance_m / imports_m,
         # US additional Liberation Day tariff:
         new_tariff = pmax(0.1, -imbalance_measure_wh / 2) * 100,
         total_m = exports_m + imports_m)

# check I have correct columns used for calculating balance:
stopifnot(nrow(filter(uscb, check > 0.001)) == 0)

combined <- uscb |> 
  left_join(actual_tariffs, by = "Country")


uscb |> 
  anti_join(actual_tariffs, by = "Country") |> 
  distinct(Country) |> 
  pull(Country)


actual_tariffs |> 
  anti_join(uscb, by = "Country") |> 
  distinct(Country) |> 
  pull(Country)


interesting_countries <- c("European Union", "Indonesia", "China", "Australia", "New Zealand",
                           "Brazil", "Vietnam", "Taiwan", "Japan", "India", "Canada", "Mexico",
                           unique(filter(combined, ahs_weighted_average_percent > 7)$Country),
                           unique(filter(combined, new_tariff > 45)$Country)) 

skipped_countries <- c("Russia", "Belarus", "Korea, North")

uscb |> 
  select(Country, exports_m:new_tariff, -check) |> 
  filter(Country %in% interesting_countries) |> 
  arrange(balance_m)

combined |> 
  select(Country, exports_m:new_tariff, -check, ahs_weighted_average_percent) |> 
  filter(Country %in% skipped_countries) |> 
  arrange(balance_m)

combined |> arrange(desc(ahs_weighted_average_percent))

combined |> 
  arrange(desc(imbalance_measure_wh)) |> 
  select(imbalance_measure_wh, new_tariff, everything())

ic <- filter(combined, Country %in% interesting_countries)
sc <- filter(combined, Country %in% skipped_countries)


#---------------------presentation----------------

combined |> 
  ggplot(aes(x = ahs_weighted_average_percent,
             y = new_tariff)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(colour = "steelblue", aes(size = total_m)) +
  geom_point(data = ic, colour = "grey30", aes(size = total_m)) +
  geom_text_repel(data = ic, aes(label = Country)) +
  geom_point(data = sc, colour = "red", aes(size = total_m)) +
  geom_text_repel(data = sc, aes(label = Country), colour = "red") +
  scale_x_continuous(label = percent_format(scale = 1)) +
  scale_y_continuous(label = percent_format(scale = 1), limits = c(0, 60)) +
  scale_size_area(label = dollar_format(suffix = "m")) +
  labs(x = "Actual weighted tariff by this country",
       y = "New tariff implied by White House trade deficit method",
       size = "Total two-way trade") +
  theme(legend.position = "right")

filter(combined, Country %in% c("Russia", "Belarus", "North Korea")) |> 
  glimpse()


combined |> 
  ggplot(aes(x = balance_m, y = new_tariff)) +
  geom_point() +
  geom_text(aes(label =Country))


combined |> 
  filter((balance_m / imports_m) < 2 & (balance_m / imports_m) > -5) |> 
  ggplot(aes(x = -balance_m / imports_m, y = new_tariff)) +
  # annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, 
  #          fill = "blue", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.5) +
  geom_point(aes(size = total_m), colour = "grey") +
  geom_point(data = ic, aes(size = total_m), colour = "black") +
  geom_text_repel(data = ic, aes(label = Country)) +
  labs(x = "US trade deficit with this partner as a proportion of its exports to it
(Negative numbers mean US has a trade surplus)",
       y = "New 'Liberation Day' tariff") +
  scale_colour_gradient2() +
  scale_x_continuous(label = percent) +
  scale_size_area(label = dollar_format(suffix = "m")) +
  theme(legend.position = c(0.1, 0.6))



picts <- c("Cook Islands", "Niue", "Tokelau",
           "New Caledonia", "French Polynesia", "Wallis and Futuna",
           "Marshall Islands", "Palau", "Micronesia",
           "Fiji", "Papua New Guinea", "Vanuatu", "Solomon Islands")

stopifnot(all(picts %in% combined$Country))

combined |> 
  filter(Country %in% picts) |> 
  ggplot(aes(x = imports_m, y = new_tariff)) +
  geom_point(aes(size = imports_m), colour = "steelblue") +
  geom_text_repel(aes(label = Country)) +
  scale_x_log10(label = dollar_format(suffix = "m")) +
  labs(x = "This country's exports to USA",
       y = "Additional 'liberation day' tariffs by USA")

  