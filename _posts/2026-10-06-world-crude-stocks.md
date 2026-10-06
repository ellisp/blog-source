---
layout: post
title: World crude oil stocks
date: 2026-10-06
tag: 
   - Energy
   - DataFromTheWeb
description: How much crude oil does the world have in its ready-to-use stocks? Data is collected by the Joint Organizations Data Initiative (JODI) but rarely presented as a total due to missingness and other issues. I do my best in showing at least the stocks of consistently reporting countries, which includes most but not all of the major countries we're be interested in.
image: /img/0334-world-crude-stocks.svg
socialimage: https:/freerangestats.info/img/0334-world-crude-stocks.png
category: R
---

How much crude oil does the world have available? Inquiring minds want to know but it is surprisingly difficult to get a single number. We hear more about relative levels "less than at any point since XXXX" and changes "has dropped X million barrels in the last month" than estimates of the absolute level. The International Energy Agency (IEA) publishes a lot of information on inventory levels and changes, but consolidated global stock estimates are, as far as I can see, not publicly available.

However, the Joint Organizations Data Initiative (JODI) does publish monthly country level estimates, reported to it voluntarily by countries. JODI partners are the APEC Energy Working Group, Eurostat, GECF, IEA, the Latin American and Caribbean Energy Organization (OLACDE), OPEC and the UN Statistical Division. It's a very reputable set of partners! And JODI's data are available to download.

Only 38 countries have data for every month JODI collects data, but this includes the USA, Japan, UK, the key European countries, and a few other big holders like Saudi Arabia, South Korea, Taiwan and Nigeria. The key omissions include, unfortunately but unsurprisingly, China, Russia, and Venezuela.. The data are also a bit old&mdash;latest data relating to July 2026 as at the time of writing, 6 October 2026, so a lag of 2-3 months. But it's much better than nothing.

The point of today's post is to get me the chart below, showing the best picture we can of the stocks of crude oil of the countries that do report those stocks, compared to a rough estimate of 85 million barrels per day of global crude refinery throughput:

<object type="image/svg+xml" data='/img/0334-world-crude-stocks.svg' width='100%'><img src='/img/0334-world-crude-stocks.png' width='100%'></object>

That chart is now part of my [fuel crisis monitoring page](/fuel-crisis/index.html) which gets updated at least weekly.

The overall story is that we indeed have less crude oil in storage than at any time since these records began in the early 2000s; but it's still around 20 days of refinery cover. And at the rate that it's going down, it will take nearly two years to get down to just 10 days of refinery cover. Actually, I'd expect market panic to set in at around 15 days of cover, but I'm being conservative here because I'm interested in the point where the world really has run out of any buffer against supply disruptions.

OK, the rest of the blog is mostly about just how I got that data, made some choices about excluding some countries, and drew the final chart.

First, downloading the data. There's a separate CSV for each year, going back to 2002. Here's how I download all those and import them into R

{% highlight R lineanchors %}
library(tidyverse)
library(glue)
library(janitor)
library(countrycode)

# Set to FALSE if you don't want to download the latest 2026 data. Will need to
# update all this workflow once we get to 2027 data.
update_2026 <- TRUE

#-----------------Downloads----------------

if (update_2026) {
  dir.create("fuel-crisis", showWarnings = FALSE)
  # Download current year. This will get more complete month by month.
  df <- here("fuel-crisis/jodi-2026.csv")
  url <- "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/primaryyear2026.csv"
  download.file(url, destfile = df)
}


# Download historical data; only needed to be done once
if (!file.exists(here("fuel-crisis/jodi-2025.csv"))) {
  for (y in 2002:2025) {
    df <- here(glue("fuel-crisis/jodi-{y}.csv"))
    url <- glue(
      "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/{y}.csv"
    )
    download.file(url, destfile = df)
  }
}

#----------------Import data------------------

# Import all the data to R:
jodi_hist_l <- list()
for (i in 1:25) {
  df <- here(glue("fuel-crisis/jodi-{i+2001}.csv"))
  jodi_hist_l[[i]] <- read.csv(df) |> as_tibble()
}

jodi_hist <- bind_rows(jodi_hist_l) |>
  clean_names()

# see https://www.jodidata.org/_resources/files/downloads/oil-data/jodi-oil-wdb-item-names-ver2017.pdf
# for guide on what everything is
# CLOSTLV is closing stocks
# CRUDEOIL is just crude
# TOTCRUDE also includes NGL and refinery feedstocks, additives and other hydrocarbons and has a lot more missing data.
{% endhighlight %}

Now that I've got the data, I want to understand the missingness. At one point I contemplated imputing the missing values of various countries, and hence I made a big wide version of the data with the idea that I might model crude stocks based on movement in some of the other numerous variables in the data. I eventually abandoned this when I realised how little meaningful information there was on eg China and Russia to do this meaningfully. But I can still use the `jodi_wide` data set created below for most of my plots:

{% highlight R lineanchors %}
#-----------explore which countries missing---------------------
jodi_wide <- jodi_hist |>
  #  filter(energy_product == "CRUDEOIL") |>
  mutate(
    dimensions = paste(energy_product, flow_breakdown, unit_measure, sep = "|")
  ) |>
  mutate(obs_value = as.numeric(obs_value)) |>
  select(ref_area, time_period, dimensions, obs_value) |>
  drop_na() |>
  spread(dimensions, obs_value, fill = NA) |>
  rename(crude = `CRUDEOIL|CLOSTLV|KBBL`) |>
  complete(ref_area, time_period, fill = list(crude = NA)) |>
  mutate(
    country = countrycode(
      ref_area,
      origin = "iso2c",
      destination = "country.name.en"
    )
  ) |>
  mutate(
    ref_area = fct_relevel(ref_area, "US"),
    date = ym(time_period),
    date_n = as.numeric(date)
  )

last_date <- jodi_wide |>
  filter(!is.na(crude)) |>
  summarise(ld = max(date)) |>
  mutate(ld = format(ld, "%B %Y")) |>
  pull(ld)

the_caption <- glue(
  "Source: JODI-OIL World Database. Most recent data is for {last_date}."
)


jodi_wide |>
  ggplot(aes(x = date, y = crude / 1000, colour = ref_area)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma) +
  labs(
    x = "",
    y = "Millions of barrels",
    caption = the_caption,
    title = "Stocks of crude oil",
    subtitle = "Coloured by country; legend not shown."
  )
{% endhighlight %}

Here's that first exploratory plot, a line chart of all the countries. The big one here is the USA; I've not shown the legend because it would take up too much space.

<object type="image/svg+xml" data='/img/0334-line-all-countries.svg' width='100%'><img src='/img/0334-line-all-countries.png' width='100%'></object>

Next I want to identify which countries have missing data, and in particular those that are potentially big contributors to world crude oil stocks but are just missing a few observations that perhaps I could impute. I start by making a `country_sum` summary dataset:

{% highlight R lineanchors %}
possible_obs <- length(unique(jodi_wide$time_period))

country_sum <- jodi_wide |>
  group_by(ref_area, country) |>
  summarise(
    crude_total = mean(crude, na.rm = TRUE),
    missing_obs = sum(is.na(crude))
  ) |>
  ungroup() |>
  # particular problem if a country has any missing observations and averages
  # 10 million barrels or more of stock:
  mutate(
    all_there = as.logical(missing_obs == 0),
    some_missing = as.logical(
      missing_obs > 0 & missing_obs < possible_obs & !is.na(crude_total)
    ),
    problem = as.logical(crude_total > 10000 & missing_obs > 0)
  ) |>
  arrange(desc(crude_total))

good_countries <- country_sum |>
  filter(all_there) |>
  pull(country)

good_countries
{% endhighlight %}

That gives me this list of the countries that have a complete set of observations (largest crude oil stocks listed first):
```
> good_countries
 [1] "United States"  "Japan"          "Saudi Arabia"  
 [4] "Germany"        "South Korea"    "Canada"        
 [7] "France"         "Turkey"         "Italy"         
[10] "Poland"         "Spain"          "United Kingdom"
[13] "Netherlands"    "Taiwan"         "Thailand"      
[16] "Norway"         "Nigeria"        "Australia"     
[19] "Sweden"         "Finland"        "Czechia"       
[22] "Hungary"        "Portugal"       "Austria"       
[25] "Slovakia"       "Belgium"        "Denmark"       
[28] "Chile"          "New Zealand"    "Azerbaijan"    
[31] "Brunei"         "Ireland"        "Switzerland"   
[34] "Estonia"        "Iceland"        "Luxembourg"    
[37] "Latvia"         "Slovenia"   
```

It also lets me find some countries that have no observations at all:

{% highlight R lineanchors %}
# 20 countries that never have any observations for Crude:
never_crude <- filter(country_sum, is.na(problem))
print(never_crude$country)
{% endhighlight %}

```
> print(never_crude$country)
 [1] "Albania"             "Bangladesh"         
 [3] "Bermuda"             "Belarus"            
 [5] "China"               "Egypt"              
 [7] "Georgia"             "Hong Kong SAR China"
 [9] "Moldova"             "Malta"              
[11] "Malaysia"            "Niger"              
[13] "Nepal"               "Sudan"              
[15] "Singapore"           "Syria"              
[17] "Eswatini"            "Tajikistan"         
[19] "Vietnam"             "Yemen" 
```
Discovering that there were 20 of these, including China and Singapore, is what made me decide there was no point trying to impute the missing values. But I did a bit more exploration of which countries had *some* observations but not a complete set. Firstly, a scatter plot of all such countries:

<object type="image/svg+xml" data='/img/0334-scatter-missing.svg' width='100%'><img src='/img/0334-scatter-missing.png' width='100%'></object>

...and then a faceted time series of the 9 biggest problematic countries.  

<object type="image/svg+xml" data='/img/0334-facet-big-missing.svg' width='100%'><img src='/img/0334-facet-big-missing.png' width='100%'></object>

I judged that maybe I could impute values for India, Mexico , Venezuela and Brazil based on their levels going up or down on average at about the world rate but decided this just wasn't going to be worth it. And clearly Russia dwarfs them all and is just not worth trying, given the special circumstances there.

Here's the code for those two plots:

{% highlight R lineanchors %}
country_sum |>
  ggplot(aes(
    x = crude_total,
    y = missing_obs,
    label = ref_area,
    colour = problem
  )) +
  geom_text() +
  scale_x_log10(label = comma) +
  labs(
    x = "Average crude stocks (thousands of barrels)",
    y = "Number of months missing an observation",
    caption = the_caption,
    title = "Size of crude stocks by number of missing observations"
  ) +
  theme(legend.position = "none")

# The nine biggest problem countries in terms of partly missing data
jodi_wide |>
  #  filter(ref_area %in% filter(country_sum, some_missing)$ref_area) |>
  filter(ref_area %in% filter(country_sum, problem)$ref_area) |>
  mutate(country = fct_reorder(country, crude)) |>
  ggplot(aes(x = date, y = crude / 1000)) +
  facet_wrap(~country, scales = "free_y") +
  geom_line(colour = "steelblue") +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  labs(
    x = "",
    y = "Millions of barrels",
    title = "Crude oil stocks of countries missing at least one data point",
    caption = the_caption
  )
{% endhighlight %}

One last exploratory plot to try to see the impact of including or dropping those countries with partial data, here's an area chart:

<object type="image/svg+xml" data='/img/0334-missing-area.svg' width='100%'><img src='/img/0334-missing-area.png' width='100%'></object>

Thinking of the Russia situation in particular, I decided best just to exclude those partially missing countries altogether.

Here's the code for that chart (and which also makes the `crude_stocks` data object I will shortly be using for my real plot)
{% highlight R lineanchors %}
#----------------Summarise and draw chart------------

# Crude oil stocks (excludes NGL etc because often missing data):
crude_stocks <- jodi_wide |>
  left_join(select(country_sum, ref_area, some_missing), by = "ref_area") |>
  group_by(date, some_missing) |>
  summarise(
    total_crude_mbbl = sum(crude, na.rm = TRUE) / 1000,
    reporting_countries = length(unique(ref_area))
  ) |>
  ungroup()

crude_stocks |>
  ggplot(aes(x = date, y = total_crude_mbbl, fill = some_missing)) +
  geom_area() +
  labs(
    caption = the_caption,
    y = "Total crude oil (millions of barrels)",
    x = "",
    title = "Total crude oil by missingness status of countries",
    fill = "Countries missing any data:"
  )
{% endhighlight %}


Finally, the code to draw the actual presentation plot. A key magic number here is the world refinery usage of crude oil at 85 million barrels per day; this is the approximate "crude runs" value from [a recent IEA Oil Market Report](https://iea.blob.core.windows.net/assets/6e8bf347-c9af-4549-a3ac-07e5361813e9/-13AUG2025_OilMarketReport.pdf). Note that this is less than the commonly cited values of around 105 million barrels per day, which includes NGLs, biofuels, and other liquid fuels. The 85m per day value is the appropriate one for me to use when looking at the CRUDEOIL series in my original JODI data; noting that they also have a TOTCRUDE series that would relate to the 105m per day figure, but which has a lot more missing data and is arguably not really what I'm most interested in anyway.

{% highlight R lineanchors %}
# note the commonly used figure of 105 is for TOTCRUDE, not just CRUDEOIl (which is more like 85)
world_use_per_day <- 85

# Summary for calculation of how long until only 10 days of cover:
war_summary <- crude_stocks |>
  # restrict it to countries that have observations for all months
  filter(!some_missing) |>
  summarise(
    prewar = total_crude_mbbl[date == "2026-03-01"],
    latest = total_crude_mbbl[date == max(date)],
    days = max(date) - as.Date("2026-03-01")
  ) |>
  mutate(
    rate = (prewar - latest) / as.numeric(days),
    ten_days_left = (latest - world_use_per_day * 10) / rate
  )

# draw chart:
crude_stocks |>
  filter(!some_missing) |>
  ggplot(aes(x = date, y = total_crude_mbbl)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = world_use_per_day * 10, colour = "darkred") + #expand_limits(y = world_use_per_day * 10) +
  expand_limits(y = 0) +
  scale_y_continuous(
    label = comma_format(suffix = "m"),
    sec.axis = sec_axis(
      ~ . / world_use_per_day,
      name = glue(
        "Days of crude inventory cover,\nat {world_use_per_day} million barrels per day"
      )
    )
  ) +
  labs(
    x = glue(
      "{length(good_countries)} countries in total have data for all months in this period."
    ),
    title = "Crude oil stocks of consistently reporting countries worldwide",
    y = "Millions of barrels",
    subtitle = glue(
      "Excluding countries with any missing data (eg China, Russia, India, Venezuela).
If the rate of decline since March 2026 continued, crude inventories would fall to 10 days of cover in {round(war_summary$ten_days_left / 7)} weeks."
    ),
    caption = the_caption
  )
{% endhighlight %}

That gets me this graphic, which is the one I will keep up to date from now on:
<object type="image/svg+xml" data='/img/0334-world-crude-stocks.svg' width='100%'><img src='/img/0334-world-crude-stocks.png' width='100%'></object>

That's all for today. Take care out there, and buy yourself an electric vehicle and rooftop solar if you can!
