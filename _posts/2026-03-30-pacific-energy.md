---
layout: post
title: Pacific island energy supply
date: 2026-03-30
tag: 
   - Pacific
   - Economics
   - Energy
   - WorkRelated
description: Pacific island countries are heavily dependent on imported diesel, gas and kerosene for electricity generation and for cooking.
image: /img/0320-pict-electricity-mix.svg
socialimage: https:/freerangestats.info/img/0320-pict-electricity-mix.png
category: R
---

With the conflict in Iran causing worldwide disruption to energy markets, I have both a work and personal interest in energy supply in Pacific islands, which led me to this blog post. Here I look at just two aspects of energy: electricity generation, and household cooking. Nothing fancy here, just accessing some data and drawing a couple of plots.

## Electricity generation

Here is the *source* of electricity for Pacific island countries, plus Australia and New Zealand, collated by Our World In Data from Energy Institute data that ultimately comes from government estimates:

<object type="image/svg+xml" data='/img/0320-pict-electricity-mix.svg' width='100%'><img src='/img/0320-pict-electricity-mix.png' width='100%'></object>

There's a pretty obvious story here: most of the Pacific is **very** dependent on "oil" (in the form of diesel) for generation of most of its electricity. There are some small steps towards renewables happening in recent years, but the vulnerability to a price or availability shock for diesel is pretty obvious.

Here's the code for producing that, using the valuable `owidapi` R package to access the Our World in Data API.

{% highlight R lineanchors %}
#---------------Set up-----------------
library(owidapi)
library(tidyverse)
library(countrycode)
library(WDI)
library(jsonlite)
library(janitor)
library(httr2)


pic_codes <- 
  c(
    "ASM", "COK", "FSM", "FJI", "PYF", "GUM", "KIR", "MHL", "NRU", "NCL",
    "NIU", "MNP", "PLW", "PNG", "PCN", "WSM", "SLB", "TKL", "TON", "TUV",
    "VUT", "WLF", "AUS", "NZL"
  )
stopifnot(length(pic_codes) == 24)

# visual check we've got the right country codes for the Pacific:
countrycode::countrycode(pic_codes, origin = "iso3c", destination = "country.name.en")

#=======================electricity source===================

palette <- c(
  coal = "brown",
  gas = "magenta",
  oil = "red",
  kerosene = "red",
  electricity = "purple",
  solar = "yellow",
  wind = "steelblue",
  hydro = "darkblue",
  bioenergy = "lightgreen",
  charcoal = "grey",
  biomass = "darkgreen",
  'other renewables' = "darkgreen"
)

#-------------------electricity mix-----------------
elec_mix <- owid_get(
  chart_id = "share-elec-by-source",
  entities  = pic_codes
)

elec_data <- elec_mix |> 
   rename(country = entity_name) |> 
   select(-entity_id) |> 
   gather(variable, value, -country, -year) |>
   filter(value != 0) |> 
   filter(year > 2001) |> 
   mutate(variable = gsub("_share_of_electricity__pct", "", variable, fixed = TRUE),
          variable =gsub("_", " ", variable),
          variable =gsub(" excluding bioenergy", "", variable),
          variable = fct_drop(variable)) |> 
   mutate(variable = fct_relevel(variable, c("bioenergy", "hydro", "other renewables"), after = Inf)) |> 
   mutate(country = fct_relevel(country, c("Australia", "New Zealand"), after = Inf)) |> 
  group_by(country) |> 
  mutate(prop_pc= sum(value[variable %in% c("oil", "gas") & year == max(year)]) 
         / sum(value[year == max(year)])) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, prop_pc))

# Draw chart
elec_data |> 
  ggplot(aes(x = year, y = value, fill = variable)) +
 facet_wrap(~country, ncol = 5) +
  geom_col() +
  scale_fill_manual(values = palette) +
  scale_y_continuous(label = percent_format(scale = 1)) +
   labs(y = "Percentage of electricity",
        fill = "Source:",
        title = "Share of electricity by source",
        subtitle = "Countries shown in increasing order of vulnerability of electricity to a petrochemicals price or availability crisis.",
        x = "",
        caption = "Source: Ember (2026); Energy Institute - Statistical Review of World Energy (2025). Data processed by Our World In Data.") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
{% endhighlight %}


## Cooking fuel

OK, so electricity generation could be threatened by a lack of diesel. What about household cooking? This next chart draws on the definitive World Health Organization Household Energy Database, which models (based on what household survey data that is available) what households are using to cook:

<object type="image/svg+xml" data='/img/0320-pict-cooking-fuel.svg' width='100%'><img src='/img/0320-pict-cooking-fuel.png' width='100%'></object>

Again, we see a lot of reliance on petrochemical products, particularly kerosene and liquid natural gas. The latter has been promoted as a relatively clean and healthy fuel to cook with compared to burning biomass (e.g. wood, coconuts, etc).

The larger Melanesian countries, with high rural populations, are those with the greatest use still of biomass for cooking. Most Pacific island countries do most of their cooking with oil or gas derived energy (remembering from the first chart, that 'electricity' often means diesel, ultimately).

Here's the code to produce that chart. I used an LLM (I forget which) for the code to access the API itself, but I tested it and tweaked it to match my style, and the chart of course is all my own code. 

{% highlight R lineanchors %}
#--------------------cooking-------------------
# The definitive source is the WHO  WHO Household Energy Database 
# which draws on various household surveys
# See https://www.who.int/data/gho/data/themes/air-pollution/cooking-fuel-and-technology-database-by-fuel-category

# next half dozen lines of code were supplied by Co-pilot and minimally
# tweaked by me for my style
indicator_code <- "PHE_HHAIR_PROP_POP_CATEGORY_FUELS"  # % by fuel type [3](https://millenniumindicators.un.org/wiki/spaces/SDGeHandbook/pages/35291272/Indicator+7.1.2)
url <- paste0("https://ghoapi.azureedge.net/api/", indicator_code)

resp <- request(url) |> 
  req_headers(`Accept` = "application/json")  |> 
  req_perform()

cooking_data <- fromJSON(resp_body_string(resp), flatten = TRUE)$value |>
  as_tibble() |> 
  clean_names()


pic_cooking_data <-cooking_data |> 
  filter(spatial_dim %in% pic_codes) |> 
  filter(dim1 == "RESIDENCEAREATYPE_TOTL") |> 
  mutate(fuel_type = tolower(gsub("HOUSEHOLDCOOKINGFUEL_FUEL_", "", dim2))) |> 
  mutate(year = as.numeric(time_dimension_value)) |> 
   select(value = numeric_value, 
          iso3_code = spatial_dim,
          value = numeric_value,
          year,
          fuel_type) |> 
  mutate(country = countrycode(iso3_code, origin = "iso3c", destination = "country.name.en"),
         country = gsub("Federated States", "Fed St", country)) |>
  group_by(country) |> 
  mutate(prop_gke = sum(value[fuel_type %in% c("gas", "kerosene", "electricity") & year == max(year)]) 
         / sum(value[year == max(year)])) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, prop_gke))

# Draw chart
pic_cooking_data |> 
  ggplot(aes(y = value, x = year, fill = fuel_type)) +
  facet_wrap(~country, ncol = 5) +
  # the numbers don't add up to 100 always, due to being modelled estimates
  #, not fully MECE, not counting dual fuels, etc. Good practice advice
  # is to not force them to add to 100%
  geom_area() +
  scale_fill_manual(values = palette) +
  scale_y_continuous(label = percent_format(scale = 1)) +
  labs(title = "Household primary fuel used for cooking",
       subtitle = "Estimates are modelled by WHO, and not adding up to 100% is a known limitation.
Countries shown in increasing order of vulnerability of cooking to a petrochemicals price or availability crisis.",
       x = "",
       fill = "Fuel type:",
       y ="Proportion of households",
       caption = "Source: WHO Household Energy Database")
{% endhighlight %}

That's all, just a quick one today. 