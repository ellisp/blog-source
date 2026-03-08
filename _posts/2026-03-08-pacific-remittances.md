---
layout: post
title: Pacific island remittances
date: 2026-03-08
tag: 
   - Pacific
   - Economics
   - Demography
   - WorkRelated
description: Pacific island countries have some of the highest dependencies in the world on remittance payments from overseas. 
image: /img/0315-remittances-bar.svg
socialimage: https:/freerangestats.info/img/0315-remittances-bar.png
category: R
---

This post is the sixth of a series of seven on population issues in the Pacific, re-generating the charts I used in a keynote speech before the November 2025 meeting of the Pacific Heads of Planning and Statistics in Wellington, New Zealand. The seven pieces of the puzzle are:
* [Visual summaries of population size and growth](/blog/2025/11/30/pacific-population)
* [Net migration](/blog/2025/12/04/pacific-net-migration)
* [World cities with the most Pacific Islanders](/blog/2026/02/16/pacific-cities) 
* [Pacific diaspora](/blog/2026/02/18/pacific-diaspora) 
* [Population pyramids](/blog/2026/03/01/pacific-pyramids)
* Remittances (This post today)
* Tying it all together (to come)

Remittances are payments from family or other contacts overseas, typically in a higher income country. The source of remittances can be people on relatively short trips overseas&mdash;in the Pacific, examples include people in the Pacific Australia Labour Mobility scheme or the New Zealand Recognised Seasonal Employer scheme&mdash;or from long term migrants who have made the other country their indefinite home. 

The distinction between the two types of duration is important for where these funds appear in the National Accounts, but unfortunately is difficult to measure statistically. Banks can keep track of how much money is being transferred and give this information to a central bank or national statistical office, but generally will not be able to classify the sources as short term or long term residents.

The implications of all this, in the context of how many Pacific islanders reside overseas and where (the subject of previous posts in this series) will all be discussed later. But for now, here is the chart of Pacific remittances:
<object type="image/svg+xml" data='/img/0315-remittances-bar.svg' width='100%'><img src='/img/0315-remittances-bar.png' width='100%'></object>

This is designed mostly to a) show how a number of Pacific countries have very high levels of remittances, relative to their national economy (more than 40% of GDP for Tonga) compared to world averages and b) highlight a few of the Pacific island countries in particular that are most extreme in this respect. Sometimes a simple bar chart is all you need to make the point. Although this bar chart isn't as simple as it might seem at first look; there's quite a bit of thought gone into the sequencing of the country categories at the bottom to maximise the impact, and of course colour-coding the bars to distinguish the Pacific countries from the global comparators.

Here's the code to produce this chart. Super simple today, just pulling the data from the World Bank's World Development Indicators and turning it into a single chart:

{% highlight R lineanchors %}

# This script draws a simple bar chart of the latest year of remittances data
#
# Peter Ellis November 2025

library(WDI)
library(tidyverse)
library(glue)

picts <- c(
  "Fiji", "New Caledonia", "Papua New Guinea", "Solomon Islands",                                             
  "Guam", "Kiribati", "Marshall Islands", "Micronesia, Fed. Sts.", "Nauru",
  "Vanuatu", "Northern Mariana Islands","Palau", "American Samoa", "Cook Islands",
  "French Polynesia", "Niue", "Samoa", "Tokelau", "Tonga", "Tuvalu", "Wallis and Futuna Islands" 
)
length(picts)
sort(picts) # all 22 SPC PICT members except for Pitcairn

# Used this to see what series are available:
# WDIsearch("remittance") |>  View()
#
# Download data from World Bank's World Development Indicators.
# Apparently worker remittances is a subset of personal. But
# the worker remittances are all NA anyway:

remit <- WDI(indicator = c(personal = "BX.TRF.PWKR.DT.GD.ZS",
                           worker = "BX.TRF.PWKR.GD.ZS"), start = 2000) |> 
  as_tibble()

# which countries have we got?
sort(unique(remit$country))

# check who missing, just the 3 NZ Realm countries plus Wallis and futuna:
picts[!picts %in% unique(remit$country)]

# data for bar chart:
pac_data <- remit |> 
  group_by(country) |> 
  filter(!is.na(personal)) |> 
  arrange(desc(year)) |> 
  slice(1) |> 
  ungroup() |> 
  filter(country %in% c(picts, "Middle income", "Low income", "Small states", "World", "Australia", "New Zealand")) |> 
  mutate(is_pict = ifelse(country %in% picts, "Pacific island", "Comparison")) |> 
  mutate(country_order = ifelse(country %in% picts, personal, 1000 - personal),
         country = fct_reorder(country, country_order)) 

# draw bar chart
pac_data|> 
  ggplot(aes(x = country, y = personal, fill = is_pict)) +
  geom_col() +
  scale_y_continuous(label = percent_format(scale = 1)) +
  scale_fill_manual(values = c("brown", "steelblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position  = "none",
        plot.caption = element_text(colour = "grey50")) +
  labs(x = "", fill = "",
      subtitle = glue('{attr(remit$personal, "label")}, {min(pac_data$year)} to {max(pac_data$year)}'),
        y = "",
       title = "High dependency on remittances for many Pacific Island countries and territories",
       caption = "Source: World Bank World Development Indicators, series BX.TRF.PWKR.DT.GD.ZS")
{% endhighlight %}

That's all for today. Coming soon (I hope), a more narrative blog tying all this Pacific population stuff together, more or less as a written version of the talk this is all based on.