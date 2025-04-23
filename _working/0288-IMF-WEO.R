library(readxl)
library(tidyverse)

download.file("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOApr2025all.ashx",
              destfile = "weo-2025.txt", mode = "wb")

d <- read_tsv("weo-2025.txt", n_max =5,  locale = locale('se', encoding = 'ISO8859-1'))
d <- read.delim("weo-2025.txt", encoding="Windows-1252")
glimpse(d)
dim(d)

library(data.table)
?fread
# thanks to Robert Krzanowski for how to deal with the encoding problem
# https://stackoverflow.com/questions/22643372/embedded-nul-in-string-error-when-importing-csv-with-fread

d <- fread("weo-2025.txt", sep = "\t", skip = 1894)
d <- fread("sed 's/\\0//g' weo-2025.txt", sep = "\t")

d <- read_tsv("weo-2025.txt", locale = readr::locale(encoding = "latin1"))
dim(d)
readLines("weo-2025.txt", n = 2)
glimpse(d)
names(d) <- as.character(d[1, ])

# for some reasons zeroes went missing in titles
names(d)[10:60] <- 1980:2030



#----------------sdmx-------------------------
# couldn't get the tab delimited version to work - encoding problems - so went
# with the SDMX version
download.file("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOAPR2025-SDMXData.ashx",
              destfile = "weo2025.zip", mode = "wb")

unzip("weo2025.zip")

library(rsdmx)
d <- readSDMX("WEOAPR2025/xmlfile_APR2025.xml", isURL = FALSE) |> 
  # this parsing takes a long time:
  as_tibble()

count(d, SCALE)    # 1, 1000000 and 1000000000
count(d, CONCEPT)  # BCA, BCA_NGDP, BF, BFD, etc
count(d, UNIT)     # B to U
count(d, REF_AREA)     # 001, 110, etc


concept <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "CONCEPT", skip = 7)[, 1:2] |> 
  rename(CONCEPT = Code,
         concept = Description)

unit <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "UNITS", skip = 8, 
                  col_names = c("UNIT", "unit"))

ref_areas <-   read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "REF_AREA", skip = 7)[, 1:2] |> 
  rename(REF_AREA = Code,
         country = Description)

weo2025 <- d |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything())

filter(weo2025, is.na(value)) |> count(OBS_VALUE)
# just -- and n/a, so OK


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

stopifnot(all(regions %in% ref_areas$country))

weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita, constant prices",
       subtitle = "Purchasing power parity; 2021 international dollar")


weo2025 |> 
  filter(CONCEPT == "NGDP") |> 
  filter(!country %in% regions) |> 
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product, current prices",
       subtitle = "local currency")

# export price of manufacturers is the only thing as % change:
weo2025 |> 
  filter(unit == "U.S. dollars; annual percent change") |> 
  count(concept)

# lots of things in USD:
weo2025 |> 
  filter(unit == "U.S. dollars") |> 
  count(concept, CONCEPT) |> 
  View()



ctxt <- "Gross domestic product per capita, current prices"
utxt <- "U.S. dollars"

weo2025 |> 
  filter(concept == txt) |> 
  filter(unit == utxt) |> 
  filter(!country %in% regions) |> 
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = txt,
       subtitle = utxt)


