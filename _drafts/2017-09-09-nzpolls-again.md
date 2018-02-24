---
layout: post
title: The long view on New Zealand political polls
date: 2017-09-09
tag: 
   - NewZealand
   - VotingBehaviour
   - R
description: New Zealand electoral polls going back 15 years
image: /img/0110-all-polls.svg
socialimage: http://ellisp.github.io/img/0110-polls.png
category: R
---

## The long view

There's been noise in the public debate on the forthcoming New Zealand election about the value of voting intention polls.  There's often not enough attention to the long view on these things, so here is a graphic of 15 years of polling data:

<img src='/img/0110-polls.svg' width='100%'>

The source is ultimately the volunteers contributing to [Wikipedia](https://en.m.wikipedia.org/wiki/Opinion_polling_for_the_New_Zealand_general_election,_2017).  The data is collated in my [nzelect R package](https://github.com/ellisp/nzelect).

Code for that graphic:

{% highlight R %}
# install the latest version of nzelect, with recent polls
devtools::install_github("ellisp/nzelect/pkg1")

library(tidyverse)
library(scales)
library(nzelect)
library(forcats)
library(RColorBrewer)
library(viridis)

pollsters <- unique(polls$Pollster)
pollsters <- pollsters[!grepl("Election result", pollsters)]

palette <- c("black", hue_pal()(15))
names(palette) <- c("Election result", pollsters)

polls %>%
    filter(Party %in% c("Labour", "National", "NZ First", "Green")) %>%
    mutate(Party = fct_reorder(Party, -VotingIntention),
           Pollster = fct_reorder(Pollster, VotingIntention, fun = length, .desc = TRUE)) %>%
    ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
    facet_wrap(~Party, scales = "free_y") +
    geom_line() +
    geom_text(aes(label = ifelse(Pollster == "Election result", "O", "")), 
              size = 8, colour = "black") +
    scale_y_continuous(label = percent) +
    labs(x = "Polling date",
         y = "Voting intention",
         caption = "Peter's Stats Stuff, http://ellisp.github.io") +
    scale_colour_manual(values = palette) +
    ggtitle("Fifteen years of opinion polls in New Zealand",
            "Intended party vote for the next election (election results shown in black)\nPolling results taken from Wikipedia, compiled in the nzelect R package")
{% endhighlight %}






