
library(tidyverse)
library(rsdmx)
library(janitor)
library(ISOcodes)
library(ggrepel)
library(scales)

pocket <- readSDMX(providerId = "PDH", 
                   resource = "data", 
                   flowRef = "DF_POCKET")  |>
  as_tibble() |>
  clean_names() 



filter(ISO_3166_1, Alpha_2 %in% unique(pocket$geo_pict)) |>
  select(Alpha_2, Name)

vaccination <- readSDMX(providerId = "PDH",
                        resource = "data",
                        flowRef = "DF_COVID_VACCINATION") |>
  as_tibble() |>
  clean_names()

count(vaccination, indicator)
count(vaccination, unit_measure)

v2 <- vaccination |>
  # rate of sectond booster administered
  filter(indicator == "COVIDVACAD2RT") |>
  mutate(obs_time = as.Date(obs_time))
  
v2 |>
  filter(obs_value > 99 & obs_time < as.Date("2022-01-01")) |>
  write_csv("anomalous-vaccination-rates.csv")


v2 |>
  # remove some anomalies
  filter(!(obs_value > 99 & obs_time < as.Date("2022-01-01"))) |>
  ggplot(aes(x = obs_time, y = obs_value)) +
  geom_line() +
  facet_wrap(~geo_pict)

latest_vaccination <- v2 |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(geo_pict,
         covid_2shot_rate = obs_value) |>
  mutate(covid_2shot_rate = covid_2shot_rate / 100) |>
  ungroup()

pocket |>
  distinct(indicator) |>
  pull(indicator)

gdp <- pocket |>
  # GDP per capita, current prices, US dollars
  filter(indicator == "GDPCPCUSD") |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(gdp_pc = obs_value, geo_pict) |>
  ungroup() 

pop <- pocket |>
  # GDP per capita, current prices, US dollars
  filter(indicator == "MIDYEARPOPEST") |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(population = obs_value, geo_pict) |>
  ungroup()


gdp |>
  left_join(pop, by = "geo_pict") |>
  left_join(latest_vaccination, by = "geo_pict") |>
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  ggplot(aes(x = gdp_pc, y = covid_2shot_rate)) +
  # returns a misleading warning that can be ignored, see https://github.com/tidyverse/ggplot2/issues/5053:
  geom_smooth(aes(weight = population), method = "glm", colour = "grey70", 
              formula = y ~ x, method.args = list(family = "binomial")) +
  geom_point(aes(size = population)) +
  geom_text_repel(colour = "steelblue", aes(label = Name), seed = 123) +
  scale_size_area(label = comma) +
  scale_x_log10(label = dollar_format(accuracy = 1)) +
  scale_y_continuous(label = percent, limits = c(0, 1)) +
  labs(x = "GDP per capita, US dollars",
       y = "Proportion of eligible population with 2 or more Covid vaccination shots",
       size = "Population:",
       title = "Covid vaccination and GDP per capita in the Pacific",
       subtitle = "Grey line is from population-weighted logistic regression")

  
