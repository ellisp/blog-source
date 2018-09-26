---
layout: post
title: Rents in Melbourne
date: 2018-08-31
tag: 
   - Timeseries
   - R
description: Rents in Melbourne have on average grown fastest in suburbs that were the cheapest in 2000; at least for two and three bedroom flats and for two bedroom houses.  Also, scatterplots are awesome.
image: /img/0123-melbourne-growth-scatter.svg
socialimage: http://freerangestats.info/img/0123-melbourne-growth-scatter.png
category: R
---

## Motivation

So, I'm in the process of moving to Melbourne and have a personal interest in the rental market there.  But I'm also more generally interested in the economics of housing.  Purchase costs get most of the attention, but rental costs are generally recognised as [the better indication of the real "price" of housing](https://theconversation.com/why-rents-not-property-prices-are-best-to-assess-housing-supply-and-need-driven-demand-100383) in supply and demand terms. Variance from speculative booms and busts is smoothed out, but even more fundamentally, it is rent that reflects the cost of living in a home.  Perhaps an analogy helps - when we are interested in the price of going to the movies we look at what it costs to receive the service of sitting in the cinema for two hours, not of purchasing the cinema.

My interest was also piqued by a tweet (perhaps in May or June 2018, that I can no longer locate) indicating that housing price growth in Sydney and Melbourne (but not Perth) is being led by lower-cost suburbs; I think the punch line was something like "first home owners are the ones driving the housing boom".  There are a lot of implicit assumptions in that conclusion but without being able to track down the tweet and the article it was referring to I can't address them here.  

I did wonder though whether the trend was actually a regression to the mean of sorts. If there is an element of randomness in suburbs' house prices (as surely there is), some suburbs would be higher-priced than their fundamentals support at one point in time due to chance. We would expect their growth from that point in time to be less than average, precisely because the starting point is an artifical and unsustainable high.  And vice versa for suburbs that happen to be lower than average at a particular point in time.

Anyway, I was interested in seeing if there was a pattern like this in rent prices in Melbourne.  To cut to the chase, here's what I found:

<img src='/img/0123-melbourne-growth-scatter.svg' width='100%'>

For flats/apartments in Melbourne and perhaps for smaller houses, the higher priced suburbs in 2000 have had weaker rental growth in the intervening period.   

Notably, other than flats in Port Melbourne (which started out the period as the most expensive by far), *all* the suburbs' median rental rates have grown faster than inflation as measured by the consumer price index, [which has grown at around 2.6% over this time period](https://www.rba.gov.au/calculator/annualDecimal.html).  Over this time period (which is long compared to the breathless up-and-down commentary we often get with housing markets), rents have generally grown pretty steadily, without some of the [fluctuations seen in purchase prices](http://www.abc.net.au/news/2018-06-30/melbourne-falling-unit-prices/9918452).  This growth in the real price of rent isn't surprising, but it is also definitely less than the growth in the cost of purchasing houses and flats.



## Data from DHHS

Luckily there is a great data source available, to a high degree of granularity, from the Victorian Department of Housing and Human Services who produce a report at least annually on key statistics on the private rental market in Victoria.  The most recent report has re-usable data to the March 2018 quarter.  The data are in an Excel workbook that covers all of Victoria.  There is a separate worksheet for each housing type: "1 bedroom flat",  "2 bedroom flat",  "3 bedroom flat",  "2 bedroom house", "3 bedroom house",
 "4 bedroom house" and "All properties".  Each actual value is either a median annual rent at the time of moving, or a count of how many dwellings had a change in tenant in the given quarter.

The data aren't "tidy", because there are two columns for each year, rather than a longer thin format with a single column for year, one for variable type (median rent or count) and one for value.  Also, there is a bug - column Z in each sheet has the heading "Dec 2003" when it clearly should be "Dec 2002".   However, it's pretty straightforward to read in all the data, fix the bug and tidy it up (including by adding a "Year Ending March" column which I'll be using as the latest data is finishes in March 2018 and I want to use a full year's worth for growth comparisons down the track): 
 
{% highlight R lineanchors %}
library(openxlsx)
library(tidyverse)
library(foreach)
library(ggpmisc) # for selecting objects to label based on 2d density
library(ggrepel)
library(scales)
library(viridis)
library(MASS)

# Caution - this URL: "https://dhhs.vic.gov.au/moving-annual-rents-suburb" - 
# looks like a permanent location but it's not, it's for a particular year 
# (not the latest).


# Instead, see https://dhhs.vic.gov.au/publications/rental-report
url <- "https://www.dhhs.vic.gov.au/sites/default/files/documents/201805/Moving%20Annual%20rents%20by%20suburb%20-%20March%20quarter%202018.xlsx"
download.file(url, destfile = "melb-rents.xlsx", mode = "wb")

# what are all the worksheet names in that workbook?
sns <- getSheetNames("melb-rents.xlsx")
sns <- sns[!sns %in% "Suburb groupings"]

# read the data in one worksheet at a time and combine with rbind():
rents <- foreach(i = 1:length(sns), .combine = rbind) %do% {
  sheet_name <- sns[i]
  rawdata <- read.xlsx("melb-rents.xlsx", sheet = sheet_name)
  
  # Correct an annoying bug where column Z, which should be Dec 2002, is actually Dec 2003:
  the_dates <- as.character(unique(rawdata[1, ]))
  the_dates <- the_dates[!is.na(the_dates)]
  sep02 <- which(the_dates == "Sep 2002")
  the_dates[sep02 + 1] <- "Dec 2002"
  
  # read in data:
  data <- rawdata[ , -3]
  data[1, ] <- c("", "", rep(the_dates, each = 2))
  names(data) <- paste(data[1, ], data[2, ])
  names(data)[1:2] <- c("district", "area")
  data <- data [-(1:2), ]
  
  # tidy up:
  data %>%
    gather(variable, value, -district, -area) %>% 
    as_tibble %>%
    separate(variable, c("mon", "yr", "variable"), sep = "\\s") %>%
    mutate(
      yr = as.numeric(yr),
      value = as.numeric(gsub("$", "", value, fixed = TRUE)),
      property = sheet_name,
      mon_num = case_when(
        mon == "Mar" ~ 3,
        mon == "Jun" ~ 6,
        mon == "Sep" ~ 9,
        mon == "Dec" ~ 12),
      yr_mon =  yr + (mon_num - 1.5) / 12,
      ye_mar = ifelse(mon == "Mar", yr, yr + 1)) %>%
    fill(district)
}
{% endhighlight %}

The lowest number of counts for any suburb - quarter - dwelling combination is 10:

```
> rents %>% filter(variable == "Count") %>% summarise(min(value, na.rm = TRUE))
# A tibble: 1 x 1
  `min(value, na.rm = TRUE)`
                       <dbl>
1                         10
```

I'm presuming that combinations with less than 10 changes of tenant have been suppressed.

## Melbourne rental for 2 or 3 bedrooms

If we restrict ourselves to the most common types of rental - 2 or 3 bedroom flats or houses - and to locations in Melbourne, we can use a "spaghetti plot" (time series plot for longitudinal panel data) to get an overview of the range and growth of rentals simultaneously:
 
<img src='/img/0123-all-medians-melbourne-2-3.svg' width='100%'>

Graphics like this are a good starting point but limited as analytical or presentation tools.

Here's the code for that graphic:

{% highlight R lineanchors %}
# Rentals with 2 to 3 bedrooms
rents_melb_23 <- rents %>%
  filter(variable != "Count") %>%
  filter(grepl("Melbourne", district) & grepl("[2-3]", property))

rents_melb_23 %>%
  mutate(district = fct_reorder(district, value, .fun = max, na.rm = TRUE)) %>%
  ggplot(aes(x = yr_mon, y = value, colour = area)) +
  facet_grid(property~district) +
  geom_line()  +
  theme(legend.position = "none") +
  scale_y_continuous("Median rent", label = dollar) +
  labs(x = "Quarter in which the move happened") +
  ggtitle("Rent at time of moving in Victoria",
          "Each line represents a collection of suburbs (not labelled)")
{% endhighlight %}

I had an idea of turning all the lines into indexes to highlight the average growth, and colouring each line in the spaghetti according to the average rent at the start of the data in 2000:

<img src='/img/0123-spaghetti-index.svg' width='100%'>

This is an improvement, but not by very much.  We can see some tentative support for the idea here, with some dark coloured lines conspicuously in the lower part of the charts indicating lower growth, but we need something more analytical to decide if anything's going on here.

Code for the indexed spaghetti chart:

{% highlight R lineanchors %}
rents_melb_23_2000 <-  rents_melb_23 %>%
  filter(yr == 2000) %>%
  group_by(area, property) %>%
  summarise(mean_median_2000 = mean(value, na.rm = TRUE)) %>%
  arrange(desc(mean_median_2000))

rents_melb_23 %>%
  group_by(area, property, yr_mon) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  group_by(area, property) %>%
  mutate(value = value / value[1] * 100) %>%
  left_join(rents_melb_23_2000, by = c("area", "property")) %>%
  ggplot(aes(x = yr_mon, y = value, colour = mean_median_2000, group = area)) +
  facet_wrap(~property) +
  geom_line() +
  scale_colour_viridis(option = "C", direction = -1, label = dollar, breaks = c(150, 350, 550)) +
  labs(colour = "Mean of four quarters' median rent in 2000:",
       y = "Index\n(defined to be 100 in first quarter of 2000)") +
  ggtitle("Rent at time of moving in Victoria, as an index to show growth",
          "Each line represents a suburb (not labelled)")
{% endhighlight %}

## Triumph of the scatter plot

Of course, we can see where we're heading - a scatter plot is the right tool here, if we can just work out which statistically transformed variables to put on the two axes.  Described in [this interesting blog by Dan Kopf as "data visualization's greatest invention"](https://qz.com/1235712/the-origins-of-the-scatter-plot-data-visualizations-greatest-invention/), scatterplots rule supreme for this sort of task.

Here's my first go - a straight comparison of the median rent in year ending March 2000 with median rent in year ending March 2018:

<img src='/img/0123-melbourne-scatter.svg' width='100%'>

...and finally, the best graphic I think for my question of interest, comparing median rent in year ending March 2000 with average annual growth in the 17 years since then:

<img src='/img/0123-melbourne-growth-scatter.svg' width='100%'>

Of course, this last graphic has the merits of reflecting how we'd use a linear regression model for statistical inference on this data.  It's hardly necessary to do the formal modelling, because clearly the effect we're seeing is statistically significant.

{% highlight R lineanchors %}
growth_melb <- rents_melb_23 %>%
  filter(ye_mar %in% c(2001, 2018)) %>%
  group_by(area, property, ye_mar) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  spread(ye_mar, value) %>%
  mutate(growth = (`2018` / `2001`) ^ (1/17) - 1)

# Scatterplot 1
growth_melb %>%
  ggplot(aes(x = `2001`, y = `2018`, label = area)) +
  facet_wrap(~property) +
  theme(panel.spacing = unit(1.5, "lines")) +
  geom_point() +
  geom_smooth(colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  scale_x_continuous("Average rental cost in year ending March 2001", label = dollar) +
  scale_y_continuous("Average rental cost in year ending March 2018", label = dollar) +
  ggtitle("Rental costs in Melbourne, comparing 2001 and 2018",
          "Each point represents a single area.")

# Scatterplot 2
growth_melb %>%
  ggplot(aes(x = `2001`, y = growth, label = area)) +
  geom_point() +
  geom_smooth(method = "rlm", colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  facet_wrap(~property) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  scale_x_continuous("Average rent in year ending March 2001", label = dollar) +
  scale_y_continuous("Average annual growth in rent, 2001 - 2018\n", label = percent) +
  ggtitle("Growth in rental costs in Melbourne compared to price of rent in the year to March 2001",
          "Each point represents a single area.
Higher cost flats have seen materially faster growth in rental prices, but this is not evident for houses.")


# modelling - completely unnecessary as the effect is so strong, but for the sake of it
growth_melb$start_price <- growth_melb$"2001"
model <- lm(growth ~ start_price * property, data = growth_melb)
summary(model)
anova(model)
par(mfrow = c(2,2), bty = "l", family = "Roboto")
plot(model)
{% endhighlight %}

For what it's worth and to encourage diagnostic checks being reported as a matter of course, here's the diagnostic charts from that model, which we see fits the assumptions (eg normality of residuals) very nicely.

<img src='/img/0123-diagnostics.svg' width='100%'>

## Price and volume

In thinking about some of that variation in growth in pricing, I wondered about the supply side.  Are volumes going up at different rates in these different suburbs? In the data to hand I don't have a great measure of volume of flats on the market, only the number of "moves" which is a proxy at best (because the velocity of moves might itself be an important factor).  For what it's worth, here's a comparison of growth in rental price to growth in number of moves:

<img src='/img/0123-growth-volume.svg' width='100%'>

That graphic feels a bit squashed compared to the others, because I've forced both axes to use the same scale (with `+ coord_equal()`), which is usually good practice in this situation.  It highlights that each facet is wider than it is tall, because of the greater variation in growth of volume of moves than in pricing.

{% highlight R lineanchors %}
#----------compare price and volume------
growth_melb_counts <-  rents %>%
  filter(variable == "Count") %>%
  filter(grepl("Melbourne", district) & grepl("[2-3]", property)) %>%
  filter(ye_mar %in% c(2001, 2018)) %>%
  group_by(area, property, ye_mar) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  spread(ye_mar, value) %>%
  mutate(growth_volume = (`2018` / `2001`) ^ (1/17) - 1) %>%
  dplyr::select(area, property, growth_volume) %>%
  left_join(growth_melb, by = c("area", "property")) %>%
  rename(growth_price = growth)

growth_melb_counts %>%
  ggplot(aes(x = growth_volume, y = growth_price, label = area)) +
  geom_smooth(method = "rlm", colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  geom_point(size = 0.8) +
  facet_wrap(~property) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  scale_x_continuous("Average annual growth in number of recorded tenancy moves/starts, 2001-2018", label = percent) +
  scale_y_continuous("Average annual growth in median rent\n2001 - 2018\n", label = percent) +
  ggtitle("Growth in rental costs in Melbourne compared to growth in number of tenancies",
          "Each point represents a single area. Scales are equal for both axes.
Growth in volume of moves has much higher variance than does growth in price.")   +
  coord_equal()
{% endhighlight %}

OK, that's it for today.  There's a lot more that could be done here by bringing in other datasets - particularly with a spatial element, and comparison to other prices - but that can wait for another day.