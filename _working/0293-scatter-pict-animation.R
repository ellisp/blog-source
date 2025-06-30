library(gganimate)
library(tidyverse)
library(rsdmx)
library(readxl)
library(countrycode)

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
  "Tuvalu",
  "Palau"
)

ref_areas <-   read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "REF_AREA", skip = 7)[, 1:2] |> 
  rename(REF_AREA = Code,
         country = Description) |> 
  mutate(ISO3_code = countrycode(country, origin = "country.name",
              destination = "iso3c"))

d2025 <- readSDMX("WEOAPR2025/xmlfile_APR2025.xml", isURL = FALSE) |> 
  # this parsing takes a long time:
  as_tibble()


weo2025 <- d2025 |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(Time = as.numeric(TIME_PERIOD)) |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(gdp_pc_ppp = as.numeric(OBS_VALUE))


indicators <- read_csv("wpp2024.csv") |> 
  filter(Variant == "Medium" & !is.na(ISO3_code)) |> 
  select(ISO3_code, Time, IMR, TPopulation1July) |> 
  inner_join(weo2025, by = c("ISO3_code", "Time"))


p <- indicators |> 
  ggplot(aes(x = gdp_pc_ppp, y = IMR, colour = country)) +
  geom_point(aes(size = TPopulation1July)) +
  geom_text(aes(label = country), hjust = 0, nudge_x = 0.02) +
  scale_x_log10() +
  scale_y_log10()  +
  scale_colour_manual(values= spcstyle::spc_cols()) +
  theme(legend.position = "none") + 
  # Here comes the gganimate code
  labs(title = 'Year: {round(frame_time)}', x = 'GDP per capita', y = 'Infant mortality') +
  transition_time(Time) +
  ease_aes('linear')

animate(p, renderer = gifski_renderer(), nframes = 300)

