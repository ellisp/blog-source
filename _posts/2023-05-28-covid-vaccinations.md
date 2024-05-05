---
layout: post
title: Covid-19 vaccination rates in the Pacific
date: 2023-05-28
tag: 
   - Health
   - ModellingStrategy
   - WorkRelated
   - Pacific
description: I compare vaccination rates in the Pacific to GDP per capita and find the evidence isn't strong enough to say that there is a relationship between the two.
image: /img/0246-vaccination-v-gdp.svg
socialimage: https:/freerangestats.info/img/0246-vaccination-v-gdp.png
category: R
---

During the Covid-19 global health crisis, the organisation where I work - the Pacific Community, or SPC - compiled and published weekly updates on Covid-19 incidence, mortality and vaccination rates. This only recently stopped with the WHO determining in May that the global health emergency was ended, after more than three years.

We published [weekly narrative updates](https://php.spc.int/covid-19) as well as a "data flow" on the Pacific Data Hub "dot Stat" implementation at [https://stats.pacificdata.org/](https://stats.pacificdata.org/). 

For some reason I can't now recall, a few months back (it's just taken me that long to write it up) I was interested in the roll-out of vaccination rates. Here's how they look over time:

<object type="image/svg+xml" data='/img/0246-vaccination-time.svg' width='100%'><img src='/img/0246-vaccination-time.png' width='100%'></object>

Some countries had a slow start, but they mostly ended up with quite high rates of getting at least two Covid vaccination shots. The global proportion of people that are fully vaccinated [is 67%](https://www.nytimes.com/interactive/2021/world/covid-vaccinations-tracker.html), according to New York Times with data from Our World in Data. Only two Pacific Island countries and territories seem to be lower than that.

Here's the code to download that from the Pacific Data Hub and draw the chart:

*Post continues after R code*
{% highlight R lineanchors %}
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

print(p1)
{% endhighlight %}

The striking thing here is that the slowest roll-out was in Papua New Guinea and Solomon Islands, the two poorest of the larger Pacific Island countries. This made me wonder, is there a general relationship between economic production (say GDP per capita) and vaccination rate in the Pacific? Are the poorer countries the least vaccinated?

That led to this chart:

<object type="image/svg+xml" data='/img/0246-vaccination-v-gdp.svg' width='100%'><img src='/img/0246-vaccination-v-gdp.png' width='100%'></object>

Papua New Guinea and Solomons do indeed stand out in the bottom left of the chart, but not all countries with low GDP per capita have low vaccination rates. Notably Kiribati, the poorest Pacific Island country and territory of them all (in GDP per capita terms - which might be a bit misleading as they don't include some fishing license revenue, but it is certainly true that Kiribati is poor by Pacific standards) has a high vaccination rate.

In fact, having done this, I think I would say that vaccination rates in the Pacific are low in countries that are poor AND have population dispersed not only over a wide area (few populations are as dispersed over as many square kilometres as Kiribati's) but in very numerous inland locations. But I'm not sure that really gets us any further than just saying "Papua New Guinea and Solomon Islands have the lowest vaccination rates".

*Post continues after R code*
{% highlight R lineanchors %}
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


print(p2)
{% endhighlight %}

How do I conclude that the relationship isn't "statistically significant"? Well, in the first case, we can see that the shaded area for the curved line from the model that's been fit in the plot could have a horizontal line that fits within the shaded area - imagine a horizontal line drawn at the 25% level. That model is a generalized linear model with a quasibinomial family response. The curve comes not from a smoothing term but just the logit link function that is default for a quasibinomial family and works with the response constrained to be within 0 and 1.

I wanted to check so I also explicitly fit the model myself (rather than relying on ggplot doing it on the fly), which is done in the code below. The conventional t statistic for the coefficient in front of `log(GDP)`  is 0.07, which is narrowly "not significant" and matches the visual. However the F statistic from an Analysis of Deviance table *is* significant, 0.02. Is it ok to use an F statistic to test a generalized linear model with a quasibiomial family, with a response that is proportions of a population? To be honest I'm not sure. Most sources seem to say it is, but perhaps I am missing something about the context here.

I'm using the wrong "population" figure for weights in this model - I've just used the whole population for each country and territory, rather than the population of people eligible for the vaccine for reporting purpose (which varies by country). But I am almost certain that's not material, either for the size of dots on the chart or for the model. Anyway, I can't be bothered to go back and fix that.

I *was* worried about the small sample size and possible influence of a few points. Knock out Kiribati and Pitcairn and perhaps the model would look significant. Knock out PNG and it certainly wouldn't be. So I decided to use brute force i.e. the bootstrap to generate a confidence interval for the slope of `log(GDP)`. This will give us a better sense of the robustness of evidence here. Some of the bootstrap examples will in fact knock out PNG, Kiribati and Pitcairn; or include them multiple times. Let's see on average what that does to the estimates.

So the confidence intervals for that slope, from all four methods of calculating bootstrap confidence intervals ("Normal", "Basic", "Percentile" and "BCa") included zero. Case closed, we can't say there's statistically significant evidence of a relationship between GDP per capita and Covid vaccination rates, based just on these Pacific countries and territories. Something else more complicated might be happening instead. Or in other words, it might just be a PNG thing.

All this modelling is based on a thought experiment that the 22 countries and territories in the data are a random sample from a hypothetical meta-population. That's the only way "statistically significant" can mean anything in this sort of situation, when you have observations on the entire real population.

Here's the code for the modelling part:

*Post continues after R code*
{% highlight R lineanchors %}
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
{% endhighlight %}

That's all for today. Take care out there.
