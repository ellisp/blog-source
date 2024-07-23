library(tidyverse)
library(scales)
library(readxl)
library(janitor)
library(ggrepel)
library(rsdmx)


gdp <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POCKET,3.0/A..GDPCPCUSD?startPeriod=2023&endPeriod=2023&dimensionAtObservation=AllDimensions") |>
  as_tibble() |>
  clean_names() |>
  select(geo_pict, gdp_per_capita_2023 = obs_value)

options(timeout=600)
df1 <- "pp24_agegrp.csv"
if(!file.exists(df1)){
  download.file("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryByAge5GroupSex_Medium.csv.gz",
                destfile = df1, mode = "wb")
}


df2 <- "pp24_standard.xlsx"
if(!file.exists(df2)){
  unlink(df2)
  download.file("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
                destfile = df2, mode = "wb")
}


standard <- read_excel(df2, skip = 16) |>
  rename(pop = `Total Population, as of 1 July (thousands)`)

agegrp <- read_csv(df1) |>
  clean_names()

picts <- tibble(location = c(
  "Fiji",
  "New Caledonia",
  "Papua New Guinea",
  "Solomon Islands",
  "Vanuatu",
  
  "Guam",
  "Kiribati",
  "Marshall Islands",
  "Micronesia (Fed. States of)",
  "Nauru",
  "Northern Mariana Islands",
  "Palau",
  
  "American Samoa",
  "Cook Islands",
  "French Polynesia",
  "Niue",
  "Samoa",
  "Tokelau",
  "Tonga",
  "Tuvalu",
  "Wallis and Futuna Islands"),
  subregion = rep(c("Melanesia", "Micronesia", "Polynesia"), times = c(5, 7, 9)))

stopifnot(nrow(picts) == 21)         
stopifnot(all(picts$location %in% agegrp$location))

old_grps <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") 
stopifnot(all(old_grps %in% unique(agegrp$age_grp)))

cagr <- function(x1, x2, year){
  y <- (x2 / x1) ^ (1 / year) - 1
  return(y)
}

cagr(100, 110, 5)

pict_sum <- agegrp |>
  inner_join(picts, by = "location") |>
  left_join(gdp, by = c("iso2_code" = "geo_pict")) |>
  mutate(old = age_grp %in% old_grps) |>
  group_by(location, time, subregion, gdp_per_capita_2023) |>
  summarise(pop = sum(pop_total),
            prop_old = sum(pop_total[old]) / pop) |>
  ungroup() |>
  group_by(location) |>
  arrange(time) |>
  mutate(pop2024 = pop[time == 2024],
         yoy_growth = pop / dplyr::lag(pop) - 1,
         coming_growth = cagr(pop2024, pop[time == 2050], 26)) |>
  ungroup() |>
  mutate(pop2024_rank = as.numeric(as.factor(rank(-pop2024)))) |>
  filter(time %in% 1950:2050)
  # filter(time %in% seq(from = 1950, to = 2050, by = 5))

library(RColorBrewer)

p <- pict_sum |>
  ggplot(aes(x = time, y = prop_old, colour = coming_growth, group = location)) +
  annotate("rect", xmin = 2023.5, xmax = 2050.5, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  geom_line() +
 # geom_vline(xintercept = 2050, colour = "black") +
  geom_point(data = filter(pict_sum, time %in% c(1950, 2024, 2050)), 
             aes(size = pop), alpha = 0.5) +
  geom_text_repel(data = filter(pict_sum, time == 2050), direction = "y",
                  aes(label = location), hjust = 0, nudge_x = 3, seed = 123,
                  min.segment.length = 5, family = "Calibri",
                  alpha = 1, force = 0.001, force_pull = 1) +
  scale_colour_viridis_c(option = "D", label = percent) +
#  scale_color_gradientn(colours = brewer.pal(7, "RdYlBu")[7:1], label = percent) +
  scale_y_continuous(label = percent) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2050, by = 25), limits = c(1950, 2070)) +
  scale_size_area(label = comma_format(suffix = "m", scale = 1/1000), 
                  breaks = c(0.1, 1, 2, 5, 10, 20) * 1000, max_size = 12) +
  # theme_dark(base_family = "Calibri") +
  # theme(legend.position = c(0.2, 0.6),
  #       legend.background = element_rect(fill = "grey20"),
  #       legend.text = element_text(colour = "grey80"),
  #       legend.title = element_text(colour = "white")) +
  theme(legend.position = c(0.2, 0.6)) +
  labs(x = "", y = "Proportion of population that is 65 or older",
       colour = "Population growth rate\n2024-2050",
       size= " Population",
       title = "Aging populations in the Pacific",
       caption = "Source: UN World Population Prospects 2024")

svg_png(p, file = "../img/0271-aging-linechart", w = 10, h = 6)
                  
                  

