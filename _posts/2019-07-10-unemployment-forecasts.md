---
layout: post
title: Forecasting unemployment
date: 2019-07-10
tag: 
   - TimeSeries
   - Economics
   - Australia
   - R
description: Forecasting unemployment is hard. Also, while AIC is asymptotically equivalent to cross-validation, it's probably better to check.
image: /img/0155-dag.svg
socialimage: http://freerangestats.info/img/0155-dag.png
category: R
---

I'm interested in "nowcasting" unemployment rates - that is, estimating a value for a current or recently past period, before the official statistic became available. In particular, I'm interested in what leading indicators might be available to help with this. 

"Leading" in this case will have to mean pretty fast, because the official unemployment stats in Australia come out from the Australian Bureau of Statistics (ABS) with admirable promptitude given the complexity of managing the Labour Force Survey. ABS Series 6202.0, the monthly summary from the Labour Force Survey comes out around two weeks after the reference month, so the [next release (on 13 June 2019) will have the unemployment rate for May 2019](https://www.abs.gov.au/AUSSTATS/abs@.nsf/webpages/ABS+Release+Calendar). So in this blog post I look at two candidates for leading information that are readily available in more or less real time - interest rates and stock exchange prices.

One big change in the past decade in this sort of short-term forecasting of unemployment has been to model the transitions between participation, employed and unemployed people, rather than direct modelling of the resulting proportions. This innovation comes from [an interesting 2012 paper by Barnichon and Nekarda](https://www.brookings.edu/wp-content/uploads/2012/09/2012b_Barnichon.pdf). I've only skimmed this paper, but I'd like to look into how much of the gains they report comes from the focus on workforce transitions, and how much from their inclusion of new information in the form of vacancy postings and claims for unemployment insurance. My suspicion is that these latter two series have powerful new information. 

## Unemployment stats

Here's the Australian unemployment rate that we're trying to forecast:

<object type="image/svg+xml" data='/img/0155-unemp.svg' width='100%'></object>

The data go back to 1978 and are published as their "original" estimates, seasonally adjusted and trend. If I'm to be forecasting this I'll want to forecast both the original and the seasonally adjusted values, so it's worth checking out if I can replicate (or get close to) the ABS' seasonal adjustment. Here's a comparison of a seasonally adjusted series out of the box using X13-SEATS-ARIMA and the default settings provided by Christoph Sax's excellent `seasonal` package (which includes testing for and including if helpful trading days and Easter seasonal adjustments). 

<object type="image/svg+xml" data='/img/0155-seas.svg' width='100%'></object>

There are 13 observations where we are 0.1 of a percentage point or more out, mostly featuring a difference of opinion between my model and the ABS' on the seasonality of the winter months. This would be worth further exploring if I had a bit more time; perhaps the ABS' parameters are published somewhere. But it's close enough for today's purposes.

Here's the R code so far. Getting hold of and tidying ABS time series data is a cinch now thanks to Matt Cowgill et al's `readabs` package.

*Post continues after code excerpt*

{% highlight R lineanchors %}
library(quantmod)
library(tidyverse)
library(scales)
library(readabs)
library(lubridate)
library(forecast)
library(vars)
library(rvest)
library(seasonal)
library(ggdag)

the_caption <- "Analysis by http:://freerangestats.info"

#------------unemployment------------------
lfs1 <- read_abs("6202.0", tables = 1)

# first exploratory chart:
lfs1 %>%
  filter(series ==  "Unemployment rate ;  Persons ;") %>%
  ggplot(aes(x = date, y = value / 100, colour = series_type)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(x = "",
       y = "Unemployment rate",
       colour = "",
       title = "Unemployment in Australia, 1978 to present",
       subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics",
       caption = the_caption)

# prepare data frame for later with columns for each series	   
unemp <- lfs1 %>%
  filter(series ==  "Unemployment rate ;  Persons ;" &
           series_type %in% c("Seasonally Adjusted", "Original"))   %>%
  dplyr::select(date, value, series_type) %>%
  spread(series_type, value) %>%
  rename(unemployment = Original,
         unemployment_sa = `Seasonally Adjusted`) %>%
  mutate(unemployment_my_sa = ts(unemployment, start = c(1978, 2), frequency = 12) %>%
           seas() %>%
           final())

#--------------Comparison of seasonal adjustments------------------
col_labs <- seq(from = 1979, to = 2019, by = 10) %>%
  paste0("-01-01") %>%
  as.Date(format = "%Y-%m-%d")

my_col_scale <- scale_colour_viridis_c(breaks = as.numeric(col_labs),
                                       labels = year(col_labs),
                                       option = "D",
                                       direction = -1)

ggplot(unemp, aes(x = unemployment_sa, y = unemployment_my_sa, colour = as.numeric(date))) +
  geom_point() +
  coord_equal() +
  labs(x = "Seasonally adjusted series published by the ABS",
       y = "Seasonally adjusted from seasonal",
       title = "Comparison of seasonally adjusted unemployment series",
       subtitle = "Vertical axis uses the defaults from the {seasonal} R package front-end to X13-SEATS-ARIMA",
       caption = the_caption,
       colour = "") +
  my_col_scale +
  theme(legend.position = "right")

# check for the individual highest points
unemp %>%
  mutate(d = abs(unemployment_my_sa - unemployment_sa)) %>%
  arrange(desc(d)) %>%
  slice(1:20)
{% endhighlight %}

## Stock prices and interest rates

The Australian Securities Exchange publishes an index based on the price of the "ASX 200" list of companies. Daily data are available from the likes of Yahoo Finance (easily accessed from the R `quantmod` package), but the longest series I could find was from the ASX themselves.


<object type="image/svg+xml" data='/img/0155-combined-series.svg' width='100%'></object>

<object type="image/svg+xml" data='/img/0155-csp1.svg' width='100%'></object>
<object type="image/svg+xml" data='/img/0155-csp2.svg' width='100%'></object>
<object type="image/svg+xml" data='/img/0155-dag.svg' width='100%'></object>



{% highlight R lineanchors %}


{% endhighlight %}

{% highlight R lineanchors %}


{% endhighlight %}
{% highlight R lineanchors %}


{% endhighlight %}
{% highlight R lineanchors %}


{% endhighlight %}
