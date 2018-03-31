---
layout: post
title: Truncated Negative Binomial distributions to model firearm violence deaths per incident
date: 2018-03-31
tag: 
   - R
   - Stan
   - Distributions
   - Crime
description: Two ways of fitting a model to truncated data.
image: /img/0120-truncated-poisson.svg
socialimage: http://ellisp.github.io/img/0120-truncated-poisson.png
category: R
---

## A grim research question

In my last post I looked at fitting a Poisson distribution to a set of count data that had been truncated so that observations below a certain threshold were not available.  My reason for doing was as a small step forward to today's task, which is to model the number of deaths per firearm violence incidence in the United States of America.

*Caution - this blog post discusses homicide and suicide statistics and could be upsetting.  If you feel disturbed or unhappy then [here is a list of ways to get some emergency counselling help](https://en.wikipedia.org/wiki/List_of_suicide_crisis_lines) around the world.*

My original research question was "what is the distribution of number of fatalities per incident in USA gun-related violence?".  The motivation was to better understand how material mass shooting or multiple-shooting events are in a broader context, and what proportion of violence is in smaller events than those that get the headlines.  For the modelling below, I chose not to distinguish between self-inflicted violence and that directed at others; this is clearly an important distinction for many purposes, but not for my first examination of the basic question of size of incidents.

Of course, there's a lot written on this topic already.  For example, Five Thirty Eight addressed the question in Maggie Koerth-Baker's piece [Mass shootings are a bad way to understand gun violence](https://fivethirtyeight.com/features/mass-shootings-are-a-bad-way-to-understand-gun-violence/).  Some of the important points made there:

- Mass shootings are rare despite the publicity they get; and the people doing them are different from most firearm violence perpetrators
- Two-thirds of firearm deaths are suicides (which may or may not be part of an incident involving other injuries and fatalities)
- Most firearm homicide victims are men between the ages of 15 and 34, and two-thirds are black; again, the profile of most firearm homicide victims differs from that of mass shootings (where 50% of victims are women, and around have of mass shootings involve domestic or family violence)

Putting aside the natural interest, importance and indeed horror that firearm violence in the USA  attracts around the world, my question was more about the number of deaths as interesting [extreme values](https://cran.r-project.org/web/views/ExtremeValue.html).  Most firearm incidents have few if any deaths, but some have very many; how can this be statistically modelled?

Some final bits of context.  I wrote about [violent deaths as a percentage of population](/blog/2015/11/26/violent-deaths) in an earlier post.  The USA has more homicides per capita than other rich countries, although less than the front runners Colombia, Brazil and Russia.  Here's a key graphic from that post:

<img src='/img/0020-deaths-trends.svg' width ='100%'>

On the particular issue of suicides that make up so many firearm-related deaths, here is a similar chart:

<img src='/img/0119-suicides.svg' width ='100%'>

Like the earlier post, this is based on the OECD's synthesis of data from national governments.  I can't vouch for the data's comparability across countries - there are some definite surprises here - other than to say that I'm sure every possible effort has been made by the various national statistical offices to get it right.  Here's the code to download and draw the chart of suicide rates:

{% highlight R %}
library(tidyverse)
library(scales)
library(readr)
library(lubridate)
library(ggrepel)
library(viridis)
library(rstan)
library(rsdmx)
library(ISOcodes)

#----------contextual data on suicides---------------
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_STAT/CICDHARM.TXCMFETF+TXCMHOTH+TXCMILTX.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LTU+RUS+ZAF/all?startTime=2000&endTime=2020"

# Deaths per 100,000 population, standardised only
if(!exists("suic_sdmx")){
  dataset <- readSDMX(url) 
  suic_sdmx <- as.data.frame(dataset)
}

data(ISO_3166_1)

suicide <- suic_sdmx %>%
  mutate(variable = ifelse(UNIT == "TXCMILTX", "All", ""),
         variable = ifelse(UNIT == "TXCMHOTH", "Males", variable),
         variable = ifelse(UNIT == "TXCMFETF", "Females", variable)) %>%
  rename(year = obsTime, value = obsValue) %>%
  select(year, COU, value, variable) %>%
  left_join(distinct(ISO_3166_1[ , c("Alpha_3", "Alpha_2", "Name")]), by = c("COU" = "Alpha_3")) %>%
  mutate(year = as.numeric(year),
         country = Name) %>%
  as_tibble()

suicide %>%
  mutate(
    variable = fct_reorder(variable, -value),
    country = fct_reorder(country, -value)
  ) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  facet_wrap(~country) +
  geom_line() +
  labs(colour = "", x = "",
       y = "Deaths per 100,000 population (standardised rates)") +
  ggtitle("Suicide rates around the world (all methods)",
          "USA is moderately high, but not as high as it features when compared on firearm-related deaths")
{% endhighlight %}


## Data sources

The [Gun Violence Archive](http://www.gunviolencearchive.org) aims to record every firearm violence event in the USA, certainly all those with fatalities.  However, it won't let you do a bulk download; and my requests for a way to do this have not gotten anywhere.  The database query tool is set up to return individual cases rather than aggregate statistics, which is a shame for my purposes.  In fact, only 500 incidents can be downloaded at once, and as far as I can tell it will give you the most recent 500 incidents matching your criteria.  Because of the astonishing number of firearm violence incidents in the USA (the database includes non-lethal incidents), this makes downloading them all an impractical proposition without breaching the intent of the original limitation.

I tried a couple of things to get around this.  

First, I queried the Gun Violence Archive database for incidents with at least one victim for each month in 2017, and downloaded the 500 incidents it gave me (the latest in the relevant month).  This isn't a simple random sample - events closer to the end of each month are more likely to be chosen; and events in shorter months eg February are more like to be chosen - but I thought it was close enough to act as though it was.  I've saved all the data I got from this (and a few abortive additional efforts) in a zip file accessible from this website

Second, I queried the database for incidents with at least four victims (which could include both killed and injured) in each year from 2014 to 2018.  Unless I misunderstood how the database worked, this returned less than 500 incidents each year.  I then counted on screen (having first sorted by `# Killed`) the frequency of events with 4, 5, 6 or more people killed, and manually entered the data myself.

My plan was to treat 2017 as my year for a representative sample, then to supplement that with a special sample of all the large events.  Then I could combine the two datasets for a model of the overall distribution that paid due attention to the rare, large events. 

## The 2017 data

Here's how it looks if you just take the sample I got of 2017 data. First, a histogram:

<img src="/img/0119-histogram.svg" width='100%'>

Then if we look at a scatterplot to get a sense of the basic question:

<img src="/img/0119-scatterplot.svg" width='100%'>

Or even a humble pie chart (yes, I believe they do have their purposes, if done properly)

<img src="/img/0119-pie.svg" width='100%'>

Here's the code to download the data and draw those charts.

{% highlight R %}
#===============downloading gun violence archives==============

# to download these files you need to go to http://www.gunviolencearchive.org/query/
# and do a query, then choose download to CSV.  It only downloads 500 at a time.
# "Save" actually means "Execute the query".  Then you need to explicitly download CSV.
# And then you need to pick "download" (even though you're already in the "download CSV"
# environment and it has downloaded it = "download" here means "yes, really download it,
# and save it to my local drive")

# to save others doing this, a subset of the data is on my website

# download the data
try(unlink("gun-violence", recursive = TRUE))
dir.create("gun-violence")
download.file("http://freerangestats.info/data/gva.zip",              
              destfile = "gun-violence/gva.zip", mode = "wb")
unzip("gun-violence/gva.zip", exdir = "gun-violence")

# read into a list in R
files <- list.files("gun-violence", full.names = TRUE, pattern = ".csv$")
gva_l <- lapply(files, read_csv)

# convert into a data frame and do a bit of tidying up of dates etc
gva <- do.call("rbind", gva_l) %>%
  mutate(date = as.Date(`Incident Date`, format = "%B %d, %Y"),
         year = year(date),
         month = month(date)) %>%
  # filter out zero deaths incidents, which we can't exclude from the original query
  # (can only filter by number of victims, not number killed)
  filter(`# Killed` > 0) %>%
  # remove duplicates in case I downloaded some data twice or there is a database error:
  distinct()

# this next graph not shown in the blog post, is just to illustrate that the data
# includes some non-2017 points to be wary of
gva %>%
  ggplot(aes(x = date, y = `# Killed`)) +
  geom_jitter(alpha = 0.1) +
  ggtitle("Fatal firearm incidents in the USA; 500 sampled from selected months",
          "Points are jittered to give a sense of how much concentration there is in '1 death per incident'") +
  labs(x = "", y = "Number killed in each incident",
       caption = "Searches from Gun Violence Archive")
# Only 2014 onwards is complete data
# Only 2017 is at all representative; the res includes various other downloads!

# Histogram
gva %>%
  filter(year == 2017) %>%
  ggplot(aes(x = `# Killed`)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), fill = "steelblue", alpha = 0.8) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous("Percentage of events", label = percent) +
  ggtitle("Random sample of firearm-events in 2017",
          "Excludes events with zero people killed")

# Scatterplot
gva %>%
  filter(year == 2017) %>%
  mutate(n_killed = paste0("'", ifelse(`# Killed` < 5, `# Killed`, "More than 4"), " death' incidents")) %>%
  group_by(n_killed) %>%
  summarise(incidents = n(),
            deaths = sum(`# Killed`)) %>%
  ungroup() %>%
  mutate(incidents = incidents / sum(incidents),
            deaths = deaths / sum(deaths)) %>%
  ggplot(aes(x = incidents, y = deaths, label = n_killed, colour = n_killed))  +
  geom_point(size = 2) +
  geom_text_repel() +
  theme(legend.position = "none") +
  scale_x_continuous("Proportion of incidents", label = percent) +
  scale_y_continuous("Proportion of deaths", label = percent)  +
  ggtitle("The vast majority of firearm deaths come from single-death incidents.",
          "Estimated firearms incidents and deaths in the USA in 2017, aggregated by number of deaths per incident"
    ) +
  # scale_color_viridis(discrete = TRUE, option = "D")
  scale_color_brewer(palette = "Set2")

# Pie chart
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    axis.text.x = element_blank()
  )

gva %>%
  filter(year == 2017) %>%
  mutate(n_killed = paste0("'", ifelse(`# Killed` < 5, `# Killed`, "More than 4"), " death' incidents")) %>%
  group_by(n_killed) %>%
  summarise(incidents = n(),
            deaths = sum(`# Killed`)) %>%
  ungroup() %>%
  mutate(incidents = incidents / sum(incidents),
         deaths = deaths / sum(deaths)) %>%
  ggplot(aes(x = "", weight = deaths, fill = n_killed, stat = "identity")) +
  # make width 0.5 if prefer a donut...
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer("Number of deaths per incident", palette = "Set2",
                     guide = guide_legend(reverse = TRUE)) +
  blank_theme +
  ggtitle("The vast majority of firearm deaths come from single-death incidents.",
          "Estimated firearms incidents and deaths in the USA in 2017, aggregated by number of deaths per incident")
{% endhighlight %}




{% highlight R %}


{% endhighlight %}



{% highlight R %}


{% endhighlight %}


