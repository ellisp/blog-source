---
layout: post
title: Long term economic growth rates
date: 2026-06-28
tag: 
   - Timeseries
   - Economics
   -
description: I draw some charts of GDP per capita growth from 1900 to 2023, for various countries, using data from the excellent Maddison Project in economic history at the Groningen Growth and Development Centre.
image: /img/0326-India.svg
socialimage: https:/freerangestats.info/img/0326-India.png
category: R
---
I came across a chart of long term real gross domestic product per capita for Australia (1900 to about 2020) that had a couple of mistakes in its y axis scale and labelling. I wanted to see for myself what it should actually look like. 

Looking for data for this drew my attention to the excellent Maddison Project, which publishes long term estimates of real GDP per capita, in 2011 prices, for many countries. It's certainly not perfect (price comparisons over such a long period of time&mdash;and they go well back before 1900&mdash;is a vexed business!), but it looks like a great effort. It's necessary to have something like this from the economic historians because most standard official statistics series for Australia only go back to 1959, due to the complications of comparisons over time.

The Maddison Project's [latest release is 2023](https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023).

Here's my re-creation of the original chart (which is not shown here), but with correct y axis labels:
<object type="image/svg+xml" data='/img/0326-Australia.svg' width='100%'><img src='/img/0326-Australia.png' width='100%'></object>

A distinctive feature of this chart is that it compares the actual growth trajectory to a constant rate of growth from 1900 to present. It's a nice comparison. It takes care to draw&mdash;it's definitely not the same as a line of best fit. On a logarithmic scale where the constant growth is a straight line, it gives a good point of reference for the faster and slower periods of growth.

Here's my code that draws the chart. The work is actually done by a project-specific function `draw_chart()`, which draws a fairly decent chart for any given country available in the database. This lets me easily produce similar charts for other countries (results shown after the R code).

{% highlight R lineanchors %}
# download latest version of Maddison data from:
# https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023

library(tidyverse)
library(readxl)
library(scales)
library(glue)
library(countrycode)

gdppc <- read_excel("mpd2023_web.xlsx", sheet = "GDPpc", skip = 2)

#' Draw time series chart from 1900 to 2023 for a single country
#' 
#' @param ccode 3 digit ISO country code
#' @param points whether to add points for each observation(default is to just draw line)
draw_chart <- function(ccode, points = FALSE){
    # country name for this country code  
    cname <- countrycode(ccode, origin = "iso3c", destination = "country.name.en")

    # Data for just this country"
    data_tc <- gdppc |> 
        select(year, all_of(ccode)) |> 
        filter(year >= 1900)

    names(data_tc)[2] <- "value"

    constant_growth <- data_tc |>
        arrange(year) |> 
        drop_na() |> 
        summarise(n = max(year) - min(year),
                    start = value[1],
                    end = value[n()],
                    start_year = min(year),
                    # for drawing labels, not actually a 'mid' point:
                    mid_point = (end + start) / 4) |> 
        mutate(growth_rate = (end / start) ^ (1 / n) - 1)

    # colours for constant growth and for data:
    cgcol <- "red"
    dcol <- "blue"

    # define plot
    p1 <- data_tc |> 
        ggplot(aes(x = year, y = value)) +
        # Draw constant growth line:
        annotate("segment", 
                x = constant_growth$start_year, 
                xend = max(data_tc$year),
                    y = constant_growth$start, yend = constant_growth$end, 
                colour = cgcol, linetype = 2) +
        # draw data line:
        geom_line(colour = dcol) +
        annotate("text", x = 1900, y = constant_growth$mid_point, 
                label = glue("Constant growth of {percent(constant_growth$growth_rate, accuracy = 0.1)}"), 
                colour = cgcol, hjust = 0) +
        annotate("text", x = 2010, y = constant_growth$mid_point, 
                    label = "Actual GDP per capita", 
                    colour = dcol, hjust = 1) +
        scale_y_log10(label = dollar_format(accuracy = 1), 
                    breaks = c(0, 0.25, 0.5, 1:6) * 10000) +
        labs(x = "",
            y= "",
            title = glue("Long term historical growth in GDP per person in {cname}"),
            subtitle = "GDP per capita, purchasing power parity, 2011 prices.",
            caption = "Source: Maddison Project Database 2023")
  
  # for some countries with broken series we might want to draw points, not just
  # lines:
  if(points){
    p1 <- p1 + geom_point(colour = dcol)
  }

    frs::svg_png(p1, glue("../img/0326-{cname}"), w = 9, h = 5)
}

draw_chart("AUS")
draw_chart("NZL")
draw_chart("USA")
draw_chart("DNK")
draw_chart("CHN", points = TRUE)
draw_chart("IND")
draw_chart("GBR")
draw_chart("IDN")
draw_chart("JPN")
{% endhighlight %}

Here's some of those other outputs, along with some glib and not-really-thought through observations from myself. Some interesting things here as we see the visible impact of historical events: Covid, the second world war and national independence across multiple countries; as well as national catastrophes such as China's Great Leap Forward.

New Zealand and the USA have slightly slower growth than Australia over this period but a similar overall trajectory. The USA has a particularly strong New Deal and World War II boost to growth:
<object type="image/svg+xml" data='/img/0326-New Zealand.svg' width='100%'><img src='/img/0326-New Zealand.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0326-United States.svg' width='100%'><img src='/img/0326-United States.png' width='100%'></object>

Denmark has less impact from the 1930s Great Depression but experienced the war as economic destruction (rather than a GDP boost as was the case for the USA). Overall its experienced fairly fast and steady economic growth:
<object type="image/svg+xml" data='/img/0326-Denmark.svg' width='100%'><img src='/img/0326-Denmark.png' width='100%'></object>

The UK had a bad time recovering from World War I but eventually attained faster (albeit periodically interrupted) economic growth until the early 2000s:
<object type="image/svg+xml" data='/img/0326-United Kingdom.svg' width='100%'><img src='/img/0326-United Kingdom.png' width='100%'></object>

China's economic growth really began with its new economic policies in the 1970s:
<object type="image/svg+xml" data='/img/0326-China.svg' width='100%'><img src='/img/0326-China.png' width='100%'></object>

India's significant economic growth began soon after 1947 independence but only accelerated from the 1980s onwards and then further from the 2000s onwards:
<object type="image/svg+xml" data='/img/0326-India.svg' width='100%'><img src='/img/0326-India.png' width='100%'></object>

Indonesia's post-war growth path reflects its turbulent political history but is essentially a success story. In addition to the interruption of meaningful estimates during the war and deconolonisation, and the troubled transition from Sukarno to Suharto in the 1960s, the East Asia Economic Crisis of the late 1990s really stands out:
<object type="image/svg+xml" data='/img/0326-Indonesia.svg' width='100%'><img src='/img/0326-Indonesia.png' width='100%'></object>

Japan had a particularly catastrophic World War II, recovered well until the late twentieth century, and in recent decades has sunk into well-known relative stagnation: 
<object type="image/svg+xml" data='/img/0326-Japan.svg' width='100%'><img src='/img/0326-Japan.png' width='100%'></object>

The Maddison Project estimates are presented and discussed in Bolt and Van Zanden (2024), "Maddison style estimates of the evolution of the world economy: A new 2023 update", Journal of Economic Surveys, 1–41.