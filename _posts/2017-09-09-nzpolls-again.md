---
layout: post
title: The long view on New Zealand political polls
date: 2017-09-09
tag: 
   - NewZealand
   - VotingBehaviour
   - R
description: New Zealand electoral polls going back 15 years
image: /img/0110-polls.svg
socialimage: http://ellisp.github.io/img/0110-polls.png
category: R
---

## The long view

There's been noise in the public debate on the forthcoming New Zealand election about the value of voting intention polls.  There's often not enough attention to the long view on these things, so here is a graphic of 15 years of polling data:

<img src='/img/0110-polls.svg' width='100%'>

Taken all together, the polls are probably doing better than many people think.

There's been a bit of a systematic underestimate of New Zealand First support perhaps, and maybe last election an overestimate of the Greens vote, but it's not too bad.

The other thing we see in that chart is that voting intention obviously changes over time.  This is something that's often forgotten when all the attention is on just the latest number.  But look at the 2004 surge in National's support after [Don Brash's controversial "Orewa" speech](https://en.wikipedia.org/wiki/Orewa_Speech), followed by a partial collapse to not much more than it had been six months earlier.  Or the interesting high levels in National support in the year or so before the 2008 and 2011 elections followed by swift movement down in the lead up to the election.  Or Labour's journey between 2011 and 2014 - growth in support up to solidly in the 30% range before a collapse in the last year to the election.  In any of these cases, taking an average of an individual pollsters' results over time and treating it as a prediction of the election day result is just unfair. People do change their minds.

I always think in terms of there being three sources of uncertainty in translating a poll result into a prediction of the election:

- uncertainty from polls' relatively small sample size, which is published as the margin of error of "plus or minus two or three percent" and is the only indication of uncertainty that is usually given prominence
- the rest of what is known to statisticians as "total survey error", which comes from things like systematic small under- or over-estimates from subtleties in the polling companies' methods, or ways that sampling people in reality doesn't resemble just picking golf balls from a bucket (for example, unlike the golf balls, certain types of people may systematically refuse to talk to surveyers)
- even if we knew for sure how people would vote today, people change their minds right up to the last minute.

My [*forecasts*](/elections/combined.html) using this data try to take all of that into account.  Only on the evening of 23 September will we know how that went.  

The source of this data is ultimately the news organisations commissioning the polls and the volunteers contributing to [Wikipedia](https://en.m.wikipedia.org/wiki/Opinion_polling_for_the_New_Zealand_general_election,_2017).  The data is collated in my [nzelect R package](https://github.com/ellisp/nzelect).

Here's the R code that drew the graphic:

{% highlight R %}
# install the latest version of nzelect, with recent polls
devtools::install_github("ellisp/nzelect/pkg1")

library(tidyverse)
library(scales)
library(nzelect)
library(forcats)

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






