library(readxl)
library(tidyverse)

# newsarticles like this compare the forecasts for growth made in April to those
# made in June:
# https://www.bbc.com/news/articles/czx415erwkwo
# eg US 1.8% v 2.7%; Canada 1.4% v 2.0%; France 0.6% v 0.8%
# Q1 - Are these current or constant prices or PPP; and per capita or not?
# Q2 - how is the Pacific going


#----------------download sdmx version-------------------------
options(timeout=600)

dluz <- function(url, destfile){
  if(!file.exists(destfile)){
    download.file(url, destfile = destfile, mode = "wb")
    unzip(destfile)
  }
}

# to see the outlooks where the whole database is available, all countries, see
# https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending

dluz("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOAPR2025-SDMXData.ashx",
              destfile = "weo2025-apr.zip")


dluz("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2024/October/WEOOCT2024-SDMXData.ashx",
              destfile = "weo2024-oct.zip")

#---------------processing---------------

library(rsdmx)
d2025 <- readSDMX("WEOAPR2025/xmlfile_APR2025.xml", isURL = FALSE) |> 
  # this parsing takes a long time:
  as_tibble()

d2024 <- readSDMX("WEOOCT2024/WEO_PUB_OCT2024.xml", isURL = FALSE) |> 
  # this parsing takes a long time:
  as_tibble()

count(d2025, SCALE)    # 1, 1000000 and 1000000000
count(d2025, CONCEPT)  # BCA, BCA_NGDP, BF, BFD, etc
count(d2025, UNIT)     # B to U
count(d2024, UNIT)     # B to U
count(d2025, REF_AREA)     # 001, 110, etc
count(d2025, FREQ)                # everying is A (annual)
count(d2024, FREQ)            # same
count(d2025, LASTACTUALDATE)      # varies, from 2003 and higher

concept <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "CONCEPT", skip = 7)[, 1:2] |> 
  rename(CONCEPT = Code,
         concept = Description)

unit <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "UNITS", skip = 8, 
                  col_names = c("UNIT", "unit"))

ref_areas <-   read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "REF_AREA", skip = 7)[, 1:2] |> 
  rename(REF_AREA = Code,
         country = Description)

weo2025 <- d2025 |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything()) |> 
  mutate(type = if_else(year > as.numeric(LASTACTUALDATE), "Actual", "Forecast"),
         type = fct_relevel(type, "Forecast"),
         edition = "WEO April 2025")

filter(weo2025, is.na(value)) |> count(OBS_VALUE)
# just -- and n/a, so OK


weo2024 <- d2024 |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything()) |> 
  mutate(type = if_else(year > as.numeric(LASTACTUALDATE), "Actual", "Forecast"),
         type = fct_relevel(type, "Forecast"),
         edition = "WEO October 2024")

weo_both <- rbind(weo2024, weo2025)



regions <- c(
  "World",
  "Advanced Economies",
  "G7",
  "Other Advanced Economies (Advanced Economies excluding G7 and Euro Area countries)",
  "Euro area",
  "Emerging Market and Developing Economies",
  "Latin America and the Caribbean",
  "Middle East and Central Asia (MECA)",
  "Emerging and Developing Asia",
  "ASEAN-5",
  "Sub-Sahara Africa",
  "Emerging and Developing Europe",
  "European Union"
)

pacific <- c(
  "Solomon Islands",
  "Fiji",
  "Kiribati",
  "Nauru",
  "Vanuatu",
  "Papua New Guinea",
  "Samoa",
  "Tonga",
  "Marshall Islands",
  "Micronesia",
  "Tuvalu"
)

stopifnot(all(pacific %in% ref_areas$country))
stopifnot(all(regions %in% ref_areas$country))




#----------------charts----------------------------

weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita, constant prices",
       subtitle = "Purchasing power parity; 2021 international dollar")



weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita in the Pacific",
       subtitle = "Purchasing power parity, constant prices; 2021 international dollar") +
  scale_y_continuous(label = dollar)
# interesting here that many of the countries did not have the big covid-related
# dip in GDP that Fiji did (or at least, it doesn't show up in their stats)


weo_both |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  facet_grid(edition~country) +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita in the Pacific",
       subtitle = "Purchasing power parity, constant prices; 2021 international dollars") +
  scale_y_continuous(label = dollar)
# interesting here that many of the countries did not have the big covid-related
# dip in GDP that Fiji did (or at least, it doesn't show up in their stats)


weo_both |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = fct_reorder(country, ratio, .fun = last, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  labs(title = "Revisions in economic expectations in the Pacific",
       subtitle = "GDP per capita, PPP constant prices, 2021 international dollars
Ratio of IMF estimates in April 2025 to those October 2024 (higher than 1.0 means the estimate was revised upwards)",
       x = "",
       y = "Ratio")
  

weo2025 |> 
  filter(CONCEPT == "NGDP") |> 
  filter(!country %in% regions) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product, current prices",
       subtitle = "local currency")

# export price of manufacturers is the only thing as % change:
weo2025 |> 
  filter(unit == "U.S. dollars; annual percent change") |> 
  count(concept)

# lots of things in USD:
# weo2025 |> 
#   filter(unit == "U.S. dollars") |> 
#   count(concept, CONCEPT) |> 
#   View()



ctxt <- "Gross domestic product per capita, current prices"
utxt <- "U.S. dollars"

weo2025 |> 
  filter(concept == ctxt) |> 
  filter(unit == utxt) |> 
  filter(!country %in% regions) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = ctxt,
       subtitle = utxt)


