---
layout: post
title: New Caledonia's nickel exports
date: 2026-01-01
tag: 
   - Economics
   - WorkRelated
   - Pacific
   - Timeseries
description: Accessing data and drawing charts for nickel exports from New Caledonia, and world nickel prices, from 2008 to 2025. 
image: /img/0310-both.svg
socialimage: https:/freerangestats.info/img/0310-both.png
category: R
---
Just a brief blog post that effectively is jotting down some thoughts and sources on New Caledonian nickel exports. 

This post was motivated by my (still incomplete) reading of this 2017 book on [Large scale mines and local level politics&mdash;between New Caledonia and Papua New Guinea](https://press.anu.edu.au/publications/series/asia-pacific-environment-monographs/large-scale-mines-and-local-level-politics), edited by Colin Filer and Pierre-Yves Le Meur. It's fascinating, although obviously a bit dated now. 

At the time of writing of that book, the depiction of nickel mining in New Caledonia was more optimistic, both in pure economic terms and as an instrument of reconciliation and Kanak independence, than would be the case today in 2025. Without going into all the details, several mines have been mothballed or shut down in recent years. An already shaky economic trend was exacerbated by the 2024 New Caledonia civil unrest. I wanted to do a quick check "is any nickel being exported at all? how much is it worth?"&mdash;and hence had a quick look at the data.

## Exports by value

My main data source for this is New Caledonia's ISEE (Institute de la statistique et des études économiques Nouvelle-Calédonie), which maintains [a page on nickel in the economy](https://www.isee.nc/economie-entreprises/entreprises-secteurs-d-activites/secteur-du-nickel). This looks like a 2019 page, but the data downloads in Excel format at the bottom are being updated monthly and are current right up to October 2025.

New Caledonia exports both ore and processed metal&mdash;there's quite a story behind this&mdash;and the export markets differ somewhat. For example Australia used to import ore but the processing plant in Queensland that was taking it was closed down. Here is the data on ore exports, by value and disaggregating by destination:

<object type="image/svg+xml" data='/img/0310-ore.svg' width='100%'><img src='/img/0310-ore.png' width='100%'></object>

... and here is the comparable data on processed metal:

<object type="image/svg+xml" data='/img/0310-metals.svg' width='100%'><img src='/img/0310-metals.png' width='100%'></object>

The next chart shows them both, now paying no attention to destination: 

<object type="image/svg+xml" data='/img/0310-both.svg' width='100%'><img src='/img/0310-both.png' width='100%'></object>

None of these charts are adjusted for inflation. Putting that aside, what we see in recent years from all of these of course is pretty obvious&mdash;a big increase in exports by value in 2021 and 2022, decline in 2023 and collapse in 2024 and 2025.

Here's the code to download this data from the ISEE website and draw those charts:

{% highlight R lineanchors %}
library(tidyverse)
library(readxl)
library(fredr)

# Nickel sector info from ISEE available at
# https://www.isee.nc/economie-entreprises/entreprises-secteurs-d-activites/secteur-du-nickel

download.file("https://www.isee.nc/component/phocadownload/category/147-consultez-les-donnees-historiques?download=676:les-exportations-de-nickel",
              destfile = "nc-nickel-exports.xls", mode = "wb")

#--------------------minerai---------------------

ore <- read_excel("nc-nickel-exports.xls", sheet = "Minerai mensuel", range = "A29:HG32",
                      col_names = c("destination", 
                                     as.character(seq.Date(from = "2008-01-15", to = "2025-10-20", by = "month")))) |> 
  gather(date, value, -destination) |> 
  mutate(value = as.numeric(value),
         date = as.Date(date),
         datem = format(date, "%b %Y"),
         datem = fct_reorder(datem, date),
         type = "Ore")

labs <- levels(ore$datem)
labs[!grepl("Oct", labs)] <- ""

ore |> 
  ggplot(aes(x = datem, y = value, fill = destination)) +
          geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = labs) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "XPF (m)", fill = "Destination:",
       title = "Nickel ore exports from New Caledonia, 2008-2025")

#--------------------metallurgie------------------

metal <- read_excel("nc-nickel-exports.xls", sheet = "Métallurgie mensuel", range = "A134:HG138",
                      col_names = c("destination", 
                                     as.character(seq.Date(from = "2008-01-15", to = "2025-10-20", by = "month")))) |> 
  gather(date, value, -destination) |> 
  mutate(value = as.numeric(value),
         date = as.Date(date),
         datem = format(date, "%b %Y"),
         datem = fct_reorder(datem, date),
         type = "Processed metal")

metal |> 
  ggplot(aes(x = datem, y = value, fill = destination)) +
          geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = labs) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "XPF (m)", fill = "Destination:",
       title = "Nickel metal exports from New Caledonia, 2008-2025")

       
#----------------------combined------------------

both <- rbind(metal, ore) |> 
  group_by(datem, type) |> 
  summarise(value = sum(value, na.rm = TRUE))

both |> 
  ggplot(aes(x = datem, y = value, fill = type)) +
          geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = labs) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "XPF (m)", fill = "Type of export:",
       title = "Nickel exports from New Caledonia 2008-2025")
{% endhighlight %}

## Seasonality

I was interested in some of the variability in these patterns, and turned the total value of these exports into a single time series. I decomposed this into trend, seasonal and random components and was interested to see that there is in fact a noticeable seasonal component:

<object type="image/svg+xml" data='/img/0310-decomposed.svg' width='100%'><img src='/img/0310-decomposed.png' width='100%'></object>

The low months in the seasonal pattern tend to be January, February, and sometimes April. This is plausibly related to the heavier rain in those months making production and transport a bit more difficult. But this is just me guessing, I am open to information on what else might be driving this seasonality.

Here's the code for that time series decomposition:

{% highlight R lineanchors %}
nickel_ts <- both |> 
  group_by(datem) |> 
  summarise(value = sum(value)) |> 
  pull(value) |> 
  ts(frequency = 12, start = c(2008, 10))


par(bty = "l")
plot(stl(nickel_ts, s.window = 7), col = "steelblue",
      main = "Nickel exports (ore + metals, in millions of XPF) from New Caledonia")
{% endhighlight %}

## Causes

Why have New Caledonia nickel exports collapsed so much? I'm not an expert on this but think that it's largely driven by global nickel prices; with a secondary factor being difficulties specific to New Caledonia (local area politics such as those that are the subject of the book I started this post with a link to; security particularly since the 2024 civil unrest but actually dating earlier; and high labour costs).

I won't go into those New Caledonia-specific reasons here. But here's the world prices, showing an obvious rise from 2016 to a spike in 2022 and a quick collapse from 2023 to 2025:

<object type="image/svg+xml" data='/img/0310-nickel-prices.svg' width='100%'><img src='/img/0310-nickel-prices.png' width='100%'></object>

The structural increase in price was associated with the demand for nickel in batteries, including in electric vehicles. The 2022 spike came about from a genuine squeeze in supply when Russia invaded Ukraine, and fear about Russian nickel going off the market. Prices then fell because of Indonesian supply coming on line, and the rise of nickel-free electric vehicle batteries.

When prices go down, of course the value of exports goes down even if volumes stay the same. But when the price is going down because of more competititon, volume goes down as well. And the impact can be strong enough to lead whole mines to close (as has been the case in New Caledonia).

Code for the world nickel prices data, which comes from the USA Federal Reserve data system (FRED):

{% highlight R lineanchors %}
#------------------nickel prices---
fredr_set_key(Sys.getenv("FRED_API_KEY")) # assumes was previously set with Sys.setenv(FRED_API_KEY = "XXXXXX")


nickel_prices <- fredr(
  series_id = "PNICKUSDM",
  observation_start = as.Date("2008-01-01"),
  observation_end   = Sys.Date()   # or a chosen end date
)

nickel_prices |> 
  ggplot(aes(x = date, y = value)) +
  geom_line(colour = "brown") +
  scale_y_continuous(label = dollar) +
  labs(x = "", y = "US dollars (m)",
        title = "World nickel prices",
        subtitle = "(Not adjusted for inflation)")
{% endhighlight %}

That's all for today. There's a lot more to this nickel story and how critical it is for New Caledonia, but I don't have time to go into it today and probably I'm not the right person to do it anyway.

Happy new year!
