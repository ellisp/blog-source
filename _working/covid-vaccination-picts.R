
library(tidyverse)
library(rsdmx)
library(janitor)
library(ISOcodes)
library(ggrepel)
library(scales)
library(boot)

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
  mutate(obs_time = as.Date(obs_time)) |>
  mutate(covid_2shot_rate = obs_value / 100)


the_caption = "Analysis by freerangestats.info; source: Pacific Data Hub, https://stats.pacificdata.org/"

p1 <- v2 |>
  inner_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  mutate(Name = fct_reorder(Name, covid_2shot_rate)) |>
  ggplot(aes(x = obs_time, y = covid_2shot_rate)) +
  geom_line() +
  facet_wrap(~Name) +
  scale_y_continuous(label = percent) +
  labs(x = "", 
       y = "Proportion of eligible population with 2 or more Covid vaccination shots",
       title = "Covid vaccination rates over time in the Pacific",
       caption = the_caption) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

svg_png(p1, "../img/0245-vaccination-time", w = 12, h = 6)

latest_vaccination <- v2 |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(geo_pict,
         covid_2shot_rate) |>
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


data <- gdp |>
  left_join(pop, by = "geo_pict") |>
  left_join(latest_vaccination, by = "geo_pict") |>
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) 

model <- glm(covid_2shot_rate ~ log(gdp_pc), data = data,
             family = "quasibinomial", weights = population)

# why is the t test not significant but F  test is? (and Chi squarte too)
summary(model)
anova(model, test = "F")
# well all of these are various approximations to things... and the sample size is small

# Definitely not significant by conventional tests if we drop Papua New Guinea
anova(glm(covid_2shot_rate ~ log(gdp_pc), data = filter(data, Name != "Papua New Guinea"),
          family = "quasibinomial", weights = population),
      test = "F")

# Let's use bootstrap to resolve this propblem
mf <- function(d, w){
  m <- glm(covid_2shot_rate ~ log(gdp_pc), data = d[w,],
      family = "quasibinomial", weights = population)
  return(coef(m)[2])
}

set.seed(42)
booted <- boot(data, mf, R = 999)
boot.ci(booted)


p2 <- data |>
  ggplot(aes(x = gdp_pc, y = covid_2shot_rate)) +
  # returns a misleading warning that can be ignored, see https://github.com/tidyverse/ggplot2/issues/5053:
  geom_smooth(aes(weight = population), method = "glm", 
              formula = y ~ x, method.args = list(family = "quasibinomial"),
              colour = "grey70", fill = "grey85") +
  geom_point(aes(size = population)) +
  geom_text_repel(colour = "steelblue", aes(label = Name), seed = 123) +
  scale_size_area(label = comma) +
  scale_x_log10(label = dollar_format(accuracy = 1)) +
  scale_y_continuous(label = percent, limits = c(0, 1)) +
  labs(x = "GDP per capita, US dollars (logarithmic scale)",
       y = "Proportion of eligible population with 2 or more Covid vaccination shots",
       size = "Population:",
       title = "Covid vaccination and GDP per capita in the Pacific",
       subtitle = "Grey line is from population-weighted logistic regression.",
       caption = the_caption)

  
svg_png(p2, "../img/0245-vaccination-v-gdp", w = 8, h = 7)
