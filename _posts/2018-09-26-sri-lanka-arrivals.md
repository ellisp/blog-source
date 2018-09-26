---
layout: post
title: Sri Lanka visitor arrivals
date: 2018-09-26
tag: 
   - Tourism
   - Timeseries
   - R
description: Sri Lanka has a rapidly growing tourism industry, two international tourism seasons, and seasonality patterns in arrivals that vary according to country of origin.
image: /img/0134-seasonality.svg
socialimage: http://freerangestats.info/img/0134-seasonality.png
category: R
---

<img src = '/img/0134-P7220028.JPG' width = '180px' align = 'right' style='transform:rotate(270deg);margin: 20px 10px 20px 10px;' title = 'Colombo skyline from the Red Cinnamon hotel. Source: Peter Ellis.'>

So I've been holidaying in Sri Lanka for a while now, and thought I would do a brief blog post using some public tourism data.  Tourism has grown rapidly in Sri Lanka since the [civil war](https://en.wikipedia.org/wiki/Sri_Lankan_Civil_War) finished in 2009.  Travelling around, it's easy to see why. Sri Lanka excels in the *range* of attractions it has to offer visitors.  These include beaches, history and ruins (from ancient times right down to facilities from the civil war), forests,  wildlife, and an interesting contemporary cultural and religious mix around the island. 

Sri Lanka has [eight UNESCO world heritage sites](https://en.wikipedia.org/wiki/World_Heritage_Sites_of_Sri_Lanka), mostly relating to history and culture, but with some interesting biosphere / environmental locations too.

## Arrivals data basics

The Sri Lanka Tourism Development Authority publishes [a range of statistical and research products](http://www.sltda.lk/statistics).  

<img src = '/img/0134-P9220209.JPG' width = '250px' align = 'left' style='margin: 10px 10px 10px 10px;' title='Sigiriya rock fortress, including tourists climbing to the top.  Source: Peter Ellis.'>

As with other national tourism statistical systems in a controlled-border situation, the base of the tourism data comes from counting visitors crossing the border. All people entering Sri Lanka complete a physical arrival card with basic information.  Aggregate results are published promptly in [monthly reports](http://www.sltda.lk/node/765).  

Past years' detailed data are available only in awkward PDF documents that I wasn't able to efficiently scrape in the time available (I'm on holiday, ok?).  However, detailed data from January 2018 is being published in Excel, and from 2014 to 2017 the summary data is easy enough to [copy into a spreadsheet](https://github.com/ellisp/blog-source/blob/master/_working/sl-tourism/monthly-totals.xlsx).

Data on "days spent in the country" (combining arrivals and departures, sometimes known as "stay days" or "total nights") would be a better measure but are harder to deliver.  They aren't readily available.

Here's what the main story looks like up to August 2018, including a forecast out for another 18 months:

<img src = '/img/0134-forecast.svg' width = '100%'>

That forecast is done with `auto.arima` from Hyndman's 	`forecast` R package, using a logarithm transform because the variance increases roughly proportional to the average number of visitors.  The code for today's post, which is all unremarkable, is in one chunk at the bottom of the post.

Here's a different look at the seasonality and growth, putting month of the year on the X axis and giving a separate line to each year's arrivals figures:

<img src = '/img/0134-seasonality.svg' width = '100%'>

This visualization is better at showing the two peak periods of tourism per year.  These two seasons are partly driven by the weather; Sri Lanka experiences [two monsoons per year](http://www.meteo.gov.lk/index.php?option=com_content&view=article&id=94&Itemid=310&lang=en), and partly by holidays and other seasonal features in the origin market countries.

These images also suggest (to me) a slowing in arrivals growth since 2015, but without a longer time series (at least back to the end of the war) it's difficult to know how substantive that would be.

## Seasonality and country of origin

I was interested to see which were the main market countries for Sri Lankan tourism, and if seasonality differed by country of residence.  For this, the only data convenient for me was the Excel workbook with just the first eight months of 2018's arrival numbers by country.  The missing last four months of the year create an obvious limitation, but we've at least got a starting point.

Here's a chart showing the top 15 countries:

<img src = '/img/0134-country-barchart.svg' width = '100%'>

Unsurprisingly, neighbouring India is both the largest single source of visitors (which includes purposes of visit other than holiday making, such as visiting family, medical or religious purposes), and has a slightly different seasonal mix from other markets.  Indian visitors spiked in May 2018 for a reason I don't know but would doubtless be clear to someone more familiar with the situation.  However, May is a low period for other key countries such as the UK and Australia, as well as the "other" category that holds all unlisted countries.

Here's a line chart that converts each country's monthly arrivals into a percentage of the total arrivals from that country in the first eight months of 2018:

<img src = '/img/0134-country-seasonality.svg' width = '100%'>

The story is the same, but perhaps a little clearer particularly for the countries towards the bottom of the chart.  We see very different seasonality patterns for visitors from Saudi Arabia compared to Russia and Ukraine, for example.  A good reminder that climate in the country of origin can matter as much as in the destination country!
	
Finally, here's a more complex graphic that tries to show the results of principal component analysis, seeking to summarise similarities or differences between the various source countries.  The principal components analysis behind this biplot uses as source data the proportions calculated in the step above as the basis for a matrix with country of residence as rows, and months as columns.

<img src = '/img/0134-country-seasonality-pc.svg' width = '100%'>

This chart shows, for example, that (based on just eight months' data) visitor arrivals from Saudi Arabia and Australia have very different seasonal patterns, whereas Ukraine and Russia are very similar. This is useful for analytical and exploratory purposes, but my track record in being able to explain such images to non-statistical audiences (or anyone, in fact) is challenged so I wouldn't necessarily recommend it for broad use.

## R code

Here's the code behind today's post:

{% highlight R lineanchors %}
library(tidyverse)
library(openxlsx)
library(lubridate)
library(forecast)

#==============download data=================

dir.create("sl-tourism")

# From the actual website for the detailed data by country, just for 2018:
download.file("http://www.sltda.gov.lk/sites/default/files/monthly-international-tourist-arrivals-2018.xlsx",
              destfile = "sl-tourism/monthly-international-tourist-arrivals-2018.xlsx",
              mode = "wb")

# From my home-made hack for the past years, totals only:
download.file("https://github.com/ellisp/blog-source/raw/master/_working/sl-tourism/monthly-totals.xlsx",
              destfile = "sl-tourism/monthly-totals.xlsx",
              mode = "wb")

# There are PDFs of detail by country by month (one PDF for each year-month combination) but I wasn't able to 
# systematically scrape them all in in the time available.

#==============total arrivals by month========================

# file name:
fn <- "sl-tourism/monthly-totals.xlsx"
# sheet names:
sns <- 2018:2015
# handy vector of month titles in full for later use:
mons <- month(1:12, label = TRUE, abbr = FALSE)

# Import all sheets
all_sheets <- lapply(sns, function(x){
  tmp <- read.xlsx(fn, sheet = as.character(x))[, 1:3]
  tmp <- tmp %>%
    gather(year_orig, value, -Month) %>%
    mutate(year = as.numeric(str_sub(year_orig, 1, 4)),
           status = ifelse(grepl("\\*$", year_orig), "Provisional", "Final"),
           Month = factor(Month, levels = levels(mons)),
           value = as.numeric(value)) 
  return(tmp)
  })

# Combine into a single data frame and tidy up a bit eg remove duplicates:
monthly <- do.call("rbind", all_sheets) %>%
  mutate(yrmon = year + (as.numeric(Month) - 0.5) / 12) %>%
  filter(status == "Final" | year == 2018) %>%
  arrange(yrmon)

#-------------forecast-------------------
# put into a time series object
monthly_ts <- ts(monthly$value, frequency = 12, start = c(2014, 1))
# check what sort of transformation makes sense. Recommends < 0, so will use 0 (log) for simplicity:
BoxCox.lambda(monthly_ts)

# fit model:
mod <- auto.arima(monthly_ts, lambda = 0)

# draw graph with forecast:
autoplot(forecast(mod, h = 18)) +
  scale_y_continuous("Visitor arrivals to Sri Lanka per month\n(Shaded blue area shows forecasts and uncertainty)", 
                     label = comma) +
  coord_cartesian(ylim = c(80000, 300000)) +
  labs(x = "Growth has slowed slightly since 2015")
  
#--------------more detailed seasonality plot----------------
monthly %>%
  ggplot(aes(x = as.numeric(Month), y = value, colour = as.ordered(year))) +
  geom_line(size = 2) +
  scale_x_continuous("", breaks = 1:12, labels = month(1:12, label = TRUE, abbr = TRUE), minor_breaks = NULL) +
  scale_color_brewer("Year", palette = "Oranges", guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous("Arrivals per month", label = comma) +
  theme(legend.position = "right") +
  ggtitle("Visitor arrivals to Sri Lanka by month",
          "Two peak seasons (Dec-Mar and Jul-Aug) are clearly visible, as is the strong growth in recent years.")


#================country detail==================
# Import and tidy the detailed country data for first months of 2018:
detail <- read.xlsx("sl-tourism/monthly-international-tourist-arrivals-2018.xlsx",
                    startRow = 4) %>%
  # get rid of the total:
  filter(X1 != "TOTAL") %>%
  # only keep the COUNTRY and the columns that are months:
  select(COUNTRY, one_of(toupper(mons))) %>%
  # tidy:
  rename(country = COUNTRY) %>% 
  gather(month, value, -country) %>%
  filter(!is.na(value)) %>%
  mutate(country2 = str_to_title(fct_lump(country, n = 15, w = value)),
         country2 = gsub(" Of", " of", country2)) %>%
  group_by(country2, month) %>%
  summarise(value = sum(value)) %>%
  ungroup(month) %>%
  mutate(month = factor(str_to_title(month), levels = levels(mons)),
         country2 = fct_reorder(country2, -value)) 

# Straight barchart of numbers:
detail %>%
  ggplot(aes(x = as.numeric(month), weight = value)) +
  geom_bar() +
  facet_wrap(~country2) +
  scale_x_continuous("", breaks = 1:12, labels = month(1:12, label = TRUE, abbr = TRUE), minor_breaks = NULL) +
  scale_y_continuous("Visitor arrivals per month", label = comma) +
  ggtitle("Visitor arrivals to Sri Lanka by country and month", 
          "Top 15 countries, first eight months of 2018.  Differing seasonal patterns for different market countries are clear.")

# Calculate proportions so can see different countries on the same scale:
detail2 <- detail %>%
  group_by(country2) %>%
  mutate(prop = value / sum(value))

detail2 %>%
  ggplot(aes(x = as.numeric(month), y = prop)) +
  scale_x_continuous("", breaks = 1:12, labels = month(1:12, label = TRUE, abbr = TRUE), minor_breaks = NULL) +
  scale_y_continuous("Percentage of first eight months' visitor arrivals", label = percent) +
  facet_wrap(~country2) +
  geom_line() +
  ggtitle("Different peak seasons for different countries")

#------------------Principal components analysis-------

# make wide version of the data (one column per month):
detail3 <- detail2  %>%
  select(country2, month, prop) %>%
  spread(month, prop)

# convert to a matrix and fit principal components to it:
detail_m <- as.matrix(detail3[ , -1])
row.names(detail_m) <- detail3$country2
detail_pc <- prcomp(detail_m)

# visualize:
par(family = "Roboto", font.main = 1)
par(bg = "grey99")
biplot(detail_pc, choices = 1:2, col = c("darkblue",  "grey75"), pc.biplot = TRUE,
       xlim = c(-3, 3), xlab = "Countries with peaks in the same months are grouped close together", ylab = "", 
       main = "Comparison of seasonality patterns by country",
       sub = "(Data only from the first eight months of 2018)", col.axis = "transparent")
{% endhighlight %}

Finally, here's the view from the guesthouse where I'm writing this, in the hills above Kandy in central Sri Lanka:

<img src = '/img/0134-P9240358.JPG' width = '100%' title='View westwards from guesthouse near Kandy. Source: Peter Ellis.'>

Nice.
