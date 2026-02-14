
# this is a crude exploration of the question:
# "What are the largest Pacific islander cities in the world?"
# It is doubtless incomplete and there are a bunch of more detailed
# issues to go into if we wanted to do this definitively.
#
# Peter Ellis 2025-11

library(tidyverse)
library(scales)

#--------------------New Zealand census data----------
# Large file of Stats NZ census data to download. apparently the Census 2023
# equivalent is not yet availalbe, so we just use the 2018 version:
dir.create("raw-data")
fn <- "raw-data/nz_census_2018.zip"
if(!file.exists(fn)){
  download.file("https://www3.stats.govt.nz/2018census/8317_Age%20and%20sex%20by%20ethnic%20group%20(grouped%20total%20responses),%20for%20census%20night%20population%20counts,%202006,%202013,%20and%202018%20Censuses%20(RC,%20TA,%20SA2,%20DHB).zip",
              destfile = fn, mode = "wb")
} 

# the file is a zipped collection of long thin coded data table and 
# dimension lookup tables explaining what each of the codes mean:
unzip(fn, exdir = "raw-data")

ethnic <- read_csv("raw-data/DimenLookupEthnic8317.csv")
area <- read_csv("raw-data/DimenLookupArea8317.csv")

# we are going to use the Territorial Authority level so we can pick up
# Christchurch, Wellington which are TAs. Note this means we are 
# not counting eg Lower Hutt as part of Wellington. An interpretation of 'greater Wellington'
# probably would include this. But this is an ok compromise for our purposes, I think?

# Takes a while because there is a mass of very detailed data here
# but we are only using a tiny bit of it - second biggest regional groups
# and just a small subset of the ethnic groups
nz2018 <- read_csv("raw-data/Data8317.csv") |> 
  filter(Year == 2018) |> 
  left_join(ethnic, by = c("Ethnic" = "Code")) |>
  rename(ethnic_name = Description) |> 
  left_join(area, by = c("Area" = "Code")) |> 
  rename(area_name = Description) |> 
  filter(ethnic_name %in% c("Maori", "Pacific Peoples")) |> 
  # only Territorial Authority level:
  filter(str_length(Area) %in% 3) |> 
  filter(!area_name %in% c("Total - Territorial Authority areas")) |> 
  # total all people:
  filter(Age == "999999") |> 
  # total all sexes:
  filter(Sex == 9) |> 
  # just cities (not districts) |> 
  filter(grepl("City", area_name)  | area_name == "Auckland") |> 
  mutate(value = as.numeric(count)) |> 
  select(ethnic_name, area_name, value) |> 
  mutate(country = "New Zealand")

# quick reality check - print to console the biggest TAs with Pacific peoples:
nz2018 |> 
  group_by(area_name) |> 
  summarise(value = sum(value)) |> 
  arrange(desc(value))

nz2018 |> 
  select(ethnic_name, value, area_name) |> 
  spread(ethnic_name, value) |> 
  arrange(desc(`Pacific Peoples`))

#--------------Australian census data--------------
# Originally downloaded from australian tablebuilder,
# file is small so is committed to this repo:
# `/raw-data/ancestry pacific by greater city 2021 australia census.csv`


aus2021 <- read_csv("raw-data/ancestry pacific by greater city 2021 australia census.csv",
                    skip = 9, n_max = 26) |> 
  select(-Total, -...11) |> 
  rename(ethnic_name = `GCCSA (UR)`) |> 
  filter(!is.na(`Greater Sydney`)) |> 
  gather(area_name, value, -ethnic_name) |> 
  filter(!grepl("Total", ethnic_name)) |> 
  mutate(value = as.numeric(value)) |> 
  mutate(ethnic_name = if_else(
    ethnic_name == "Maori", "Maori", "Pacific Peoples"
  )) |> 
  group_by(ethnic_name, area_name) |> 
  summarise(value = sum(value)) |> 
  mutate(country = "Australia")

#--------------Other--------------
# these estimates from various ad hoc sources, mostly
# Wikipedia. Remembering we want number of pacific islanders,
# not total ppulation. Which means we have two difficult numbers
# to get hold of.
other <- tribble(~area_name, ~value, ~country,
                 "Port Moresby", 400000, "PNG",
                 "Lae",           100000, "PNG",
                 "Mount Hagen", 50000, "PNG",
                 # pop is 400k+ but what proportion is pacific? - generally west papua about 75% papuans
                 "Jayapura", 320000, "Indonesia",
                 "Sorong", .75 * 300000, "Indonesia",
                 "Greater Suva", 185000, "Fiji", # counting nausori
                 "Lautoka", 75000, "Fiji",
                "Nasinu", 74000, "Fiji",
              "Honolulu urban area", 0.09 * 1e6, "USA",
            "Greater Noumea", 0.26 * 200000, "New Caledonia",
          "Papeete", 137000, "French Polynesia",
          "Honiara", 80000, "Solomon Islands",
          "South Tarawa", 70000, "Kiribati",
          "Majuro", 20000, "Marshall Islands",
          "Apia", 30000, "Samoa",
          "Port Vila", 50000, "Vanuatu"
        ) |> 
  mutate(ethnic_name = "Pacific Peoples")

#----------------analysis--------------

p1 <- nz2018 |> 
  rbind(aus2021) |> 
  rbind(other) |> 
  group_by(area_name) |> 
  mutate(total = sum(value)) |> 
  ungroup() |> 
  arrange(desc(total)) |> 
  slice(1:24) |> 
  mutate(area_name = fct_reorder(area_name, -value, .fun = sum)) |> 
  mutate(country_type = case_when(
    country %in% c("Australia", "New Zealand", "France", "USA") ~ "Large SPC member",
    country %in% c("Indonesia")  ~ "Non-SPC member" ,
    TRUE ~ "Pacific island SPC member")) |> 
  ggplot(aes(y = value, x = area_name, fill = country_type)) +
  geom_col(position = "stack") +
  scale_y_continuous(label = comma) +
  theme_spc() +
  scale_fill_manual(values = spc_cols(c(2,3,1))) +
  labs(fill = "", x = "", y = "Number of Pacific Islanders
(including Māori, Papuans and Hawaiians)",
       title = "The world's largest Pacific Islander cities",
      subtitle = "Treat these estimates with some caution...",
       caption = "Source: Australia Census 2021, New Zealand Census 2018, author estimates") +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.7),
        plot.caption = element_text(colour = "grey50"))

Cairo::CairoSVG("output/pop-mobility-wellington-2025-11-03/pacific-cities.svg", w = 10, h = 5)
print(p1)
dev.off()
