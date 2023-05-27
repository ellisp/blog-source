
#------------------data prep-------------------
library(tidyverse)
library(rsdmx)
library(janitor)
library(ISOcodes)
library(ggrepel)
library(scales)
library(boot)

# Download the vaccination rates
vaccination <- readSDMX(providerId = "PDH",
                        resource = "data",
                        flowRef = "DF_COVID_VACCINATION") |>
  as_tibble() |>
  clean_names()

# have a look at all the various indicators available. There are lots...
count(vaccination, indicator)
count(vaccination, unit_measure)

# we will choose just one:
v2 <- vaccination |>
  # rate of sectond booster administered
  filter(indicator == "COVIDVACAD2RT") |>
  mutate(obs_time = as.Date(obs_time)) |>
  mutate(covid_2shot_rate = obs_value / 100)

#--------------------draw time series chart----------------
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

svg_png(p1, "../img/0246-vaccination-time", w = 12, h = 8)

#==================comparison to GDP==================

#-----------------Data prep---------------------
# Download some summary information on each country and territory from
# the "pocket summary"
pocket <- readSDMX(providerId = "PDH", 
                   resource = "data", 
                   flowRef = "DF_POCKET")  |>
  as_tibble() |>
  clean_names() 

# check, what indicators do we have? (lots):
pocket |>
  distinct(indicator) |>
  pull(indicator)

# extract just the most recent observed vaccination rate for each country
# from our vaccination time series:
latest_vaccination <- v2 |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(geo_pict,
         covid_2shot_rate) |>
  ungroup()

# extract just the GDP per capita from the pocket summary:
gdp <- pocket |>
  # GDP per capita, current prices, US dollars
  filter(indicator == "GDPCPCUSD") |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(gdp_pc = obs_value, geo_pict) |>
  ungroup() 

# extract the latest population:
pop <- pocket |>
  # GDP per capita, current prices, US dollars
  filter(indicator == "MIDYEARPOPEST") |>
  arrange(desc(obs_time)) |>
  group_by(geo_pict) |>
  slice(1) |>
  select(population = obs_value, geo_pict) |>
  ungroup()

# combine the GDP, population, vaccination and country names:
data <- gdp |>
  left_join(pop, by = "geo_pict") |>
  left_join(latest_vaccination, by = "geo_pict") |>
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) 


#------------------------------draw chart-----------------
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
       subtitle = "Grey line is from population-weighted logistic regression. Relationship is not statistically significant.",
       caption = the_caption)


svg_png(p2, "../img/0246-vaccination-v-gdp", w = 8, h = 7)

#--------------modelling---------------

model <- glm(covid_2shot_rate ~ log(gdp_pc), data = data,
             family = "quasibinomial", weights = population)

# why is the t test not significant but F  test is? (and Chi square too)
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

