---
layout: post
title: House effects in New Zealand voting intention polls
date: 2017-03-21
tag: 
   - VotingBehaviour
   - NewZealand
   - R
description: I use generalized additive models to explore "house effects" (ie statistical bias) in polling firms' estimates of vote in previous New Zealand elections.
image: /img/0084-house1.svg
socialimage: http://ellisp.github.io/img/0084-house1.png
category: R
---

This post is one of a series leading up to a purely data-driven probabilistic prediction model for the New Zealand general election in 2017.  No punditry will be indulged in (if only to avoid complications with my weekday role as an apolitical public servant)!  This is straight statistics, if there is such a thing...

## There are important sources of uncertainty in political polling other than sampling error

An important question in using polling/survey data to predict election results is how to quantify the "house effects" of the different polling companies conducting surveys of potential voters.  This is important for establishing which of the various pollsters is best at predicting the overall result which has obvious commercial implications for them; and also for predicting future election results for polling aggregators.

The naive approach is the one taken in [this article on polling leading up to the 2014 New Zealand general election](http://m.nzherald.co.nz/nz/news/article.cfm?c_id=1&objectid=11328981), which takes polling results as predictions of the eventual result.  A more sophisticated approach is to treat polling numbers as data in an a predictive model in which election vote is a time-bound result.  Basically, voters get to change their mind between a poll and the election - the more time between the two, the more randomness there is, and the uncertainty from this can certainly swamp the often-quoted margins of error.

If we look at the data from three major pollsters that have both a track record and continue to poll today (March 2017) for New Zealand intended party vote, we see that discrepancies between election results and voting intention estimated from opinion polls are not purely a matter of pollsters' uncertainty or error:

<img src = '/img/0084-straight-polls-1.svg'  width = '100%'>

The case of the Labour Party in the period between the 2011 and 2014 elections is a particular case in point.  All three pollsters in the chart had, on average, higher intended party vote for Labour during the period than eventually transpired.  But the consistency of their patterns strongly suggests that there was a genuine trend over time, best captured by Reid Research and Roy Morgan, with a decrease in support in the months leading up to the actual election.

In contrast, in the 2011 and 2014 (and perhaps 2008) elections, all three pollsters do indeed seem to systematically underestimate the vote for New Zealand First.

## There are several things to take account of

A well-regarded approach to studying the relationship between opinion polls and electoral results has been based on [a seminal 2005 article by Simon Jackman, "Pooling the Polls Over an Election Campaign"](http://eppsac.utdallas.edu/files/jackman/CAJP%2040-4%20Jackman.pdf):

> "Poll results vary over the course of a campaign election and across polling organisations, making it difficult to track genuine changes in voter support. I present a statistical model that tracks changes in voter support over time by pooling the polls, and corrects for variation across polling organisations due to biases known as ‘house effects’. The result is a less biased and more precise estimate of vote intentions than is possible from any one poll alone."

*Simon Jackman*

The method is also described in his classic text, [Bayesian Analysis for the Social Sciences](http://au.wiley.com/WileyCDA/WileyTitle/productCd-0470011548.html).  The statistical method theorises a latent, largely unobserved (ie except on election day) state space of the voting intention each day, and that polls are various dirty variables linked to the state of that latent variable, but not direct observations of it.  The state space changes at random due to unknown forces over time, and the various opinion polls are grossly imperfect glimpses of its reality.  The method is state of the art but very computationally intensive - it requires estimating the state of the voting intention for each party on each day since observations began, resulting in many thousands of parameters if the model is fit over multiple election cycles.  Computational methods exist for fitting such models and I intend to do so at some point, but I also wanted a quicker look at the data that doesn't take hours or days to fit fit.

It's worth noting at this point that there's good reason to believe that the latent, unobserved voting intention isn't as volatile as polls indicate - ie an endorsement of Jackman's method or a similar smoothing approach to aggregate polls, with a conservative approach to change in the underlying intention compared even to the common weighted rolling average method of aggregation provides.  A [brilliant study during the 2012 USA Presidential Election by Andrew Gelman and Andrew Rothschild](http://www.slate.com/articles/news_and_politics/politics/2016/08/don_t_be_fooled_by_clinton_trump_polling_bounces.html) showed compellingly that much of the fluctuation in voting intention comes from bias in responses:

> "When there was good news for Mitt Romney, more Republicans opted to respond to the poll; when Obama was riding high, Democrats were more likely to respond. The result was that large and systematic changes in nonresponse had the effect of amplifying small changes in actual voter intention."

These results are compelling evidence for some kind of smoothing of polling results, that not only provides a weighted average of recent polls, but in addition some healthy statistical skepticism (or regularisation to shrink towards zero) to the apparent rapid moves up and down in voting intention.

## Generalized additive models give a cheap and quick solution to a broad range of smoothing challenges

To get a glance at house effects in New Zealand polling, I decided to use generalized additive models for the polls leading up to individual elections, for one party at a time, to produce a predicted election result for each pollster-party-election combination.  Most studies I've seen in this space have applied Jackman's method to a single election cycle; the computational problems magnify with longer periods as do the data management challenges.  My `nzelect` R package makes available multiple years of polling data sourced from Wikipedia (albeit unfortunately without sample size and margin of error information); using a GAM rather than a state space model massively reduces the computational challenges.

I limit myself to the seven parties with a party vote likely to influence the 2017 election and who also were in previous elections; and the three major pollsters who both have a track record and an intention of polling leading up to the 2017 election (my understanding is that Digipoll do not have such intention; otherwise they would be a fourth pollster to include in the analysis).

For each combination of party and election year I used the polling data from all pollsters to predict election result, allowing an absolute level of bias for each pollster in the model.  For two of the pollsters I have four complete election cycles of data and I obtained these results:

<img src = '/img/0084-house1.svg'  width = '100%'>

Roy Morgan stands out as particularly inclined to over-estimate the vote for the Greens; and Colmar Brunton to under-estimate the vote for New Zealand First. 

For the third pollster I only have two election cycles of data and I get these results:

<img src = '/img/0084-house2.svg'  width = '100%'>

Here is a table summarising the amount each pollster appears to over/under estimate voting intention, comparing the prediction for election day based on patterns to date with the actual result:

|Party         | Colmar Brunton| Reid Research| Roy Morgan|
|:-------------|--------------:|-------------:|----------:|
|ACT           |          -0.2%|         -0.1%|       0.2%|
|Green         |           0.6%|            2%|       2.6%|
|Labour        |          -0.1%|         -0.9%|        -1%|
|Maori         |          -0.2%|         -0.2%|      -0.1%|
|National      |           3.2%|          3.2%|       0.1%|
|NZ First      |          -1.8%|         -2.5%|         0%|
|United Future |          -0.4%|         -0.3%|         0%|


## Number of polls

For those who are interested, here is an exploratory chart on the number of actual polls available by pollster and election cycle which led me to my conclusion to fit models just to polls from Colmar Brunton, Reid Research and Roy Morgan:

<img src = '/img/0084-polls-year.svg'  width = '100%'>

## R code

Here is the code in R for all the above.  Caveat - this is very much a spare time project for me, and none of this has been peer reviewed.  I'd never allow this situation in my professional life, but here I am publishing results on a political subject this way... Use at your own risk, and if you spot something wrong or an idea for improvement, please let me know.

{% highlight R %}
library(nzelect)
library(mgcv)
library(tidyverse)
library(scales)
library(magrittr)
library(forcats)
library(RColorBrewer)


#=====================data prep=======================

# vector of colours to use in graphics
house_colours <- c("black", brewer.pal(3, "Set1"))
names(house_colours) <-   c("Election result", "Reid Research", "Colmar Brunton", "Roy Morgan")

# vector of just the seven main parties to use
parties <- polls %>%
   filter(ElectionYear == 2017) %>%
   distinct(Party) %>%
   filter(!Party %in% c("Destiny", "Progressive", "Mana", "Conservative")) %$%
   Party


#===============introductory graphic========================
election_dates <- polls %>%
   filter(Pollster == "Election result") %>%
   select(MidDate) %>%
   distinct()

d1 <- polls %>%
   filter(Party %in% parties) %>%
   filter(Pollster %in% c("Reid Research", "Colmar Brunton", "Roy Morgan", "Election result")) %>%
   mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
          Pollster = fct_relevel(Pollster, "Election result")) 

d1 %>%
   ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
   geom_vline(xintercept = as.numeric(election_dates$MidDate), colour = "orange") +
   geom_line(alpha = 0.4) +
   geom_smooth(data = filter(d1, Pollster != "Election result"), span = .3, se = FALSE) +
   geom_line(data = filter(d1, Pollster == "Election result"), size = 1, alpha = 0.5) +
   geom_point(data = filter(d1, Pollster == "Election result"), size = 2) +
   scale_y_continuous("Voting intention", label = percent) +
   scale_x_date("") +
   labs( colour = "")   +
   scale_colour_manual(values = house_colours) +
   ggtitle("Survey versus actual performance in New Zealand voting behaviour",
           "New Zealand First seems systematically underestimated; Greens perhaps overestimated.") +
   labs(caption = "Source: polls data collected by Wikipedia, available in the {nzelect} R package") +
   facet_wrap( ~ Party, scales = "free") +
   theme(legend.position = c(0.7, 0.15)) 

#=============estimate and present house "bias"=============
house_bias <- function(elect_years, pollsters){
   # depends on these objects being in environmenet:
   # polls, house_colours, parties
   
   houses <- expand.grid(elect_years, pollsters)
   names(houses) <- c("ElectionYear", "Pollster")
   
   for(j in 1:length(parties)){
      the_party = parties[j]
      
      # election results:
      results <- polls %>%
         filter(ElectionYear %in% elect_years & ElectionYear != 2002) %>%
         filter(Pollster == "Election result")  %>%
         filter(Party == the_party) 
      
      
      for(i in 1:length(elect_years)){
         
         # Note we include *all* pollsters in the data for fitting the model
         thedata <- polls %>%
            filter(ElectionYear == elect_years[i] & Pollster != "Election result") %>%
            filter(Party == the_party)
         
         mod <- gam(VotingIntention ~ s(as.numeric(MidDate)) + Pollster, 
                    family = "quasibinomial", data = thedata)
         
         # for predicting values, we only take the pollsters we have an interest in:
         preddata <- data.frame(MidDate = as.numeric(results[i, "MidDate"]), Pollster = pollsters)
         
         # house effect is shown by the amount the predicted value from polling
         # is *more* than the actual vote.  So a positive score means the poll
         # overestimated the actual vote:
         houseeffects <- predict(mod, newdata = preddata, type = "response") -
            results[i, "VotingIntention"]
         houses[houses$ElectionYear == elect_years[i], the_party] <- houseeffects
      }
   
   }   
   
   p <- houses %>%
      gather(Party, `Polling overestimate`, -ElectionYear, -Pollster) %>%
      ggplot(aes(x = ElectionYear, y = `Polling overestimate`, colour = Pollster)) +
      geom_hline(yintercept = 0, colour = "black") +
      geom_point() +
      geom_line() +
      facet_wrap(~Party, ncol = 4) +
      scale_colour_manual(values = house_colours) +
      scale_x_continuous("Election year", breaks = c(2005, 2008, 2011, 2014), limits = c(2004, 2015)) +
      scale_y_continuous(label = percent) +
      theme(legend.position = c(0.9, 0.18)) +
      ggtitle("Statistical forecast of election compared to actual result",
              "Forecasts use time series methods based on pollsters' results, are not actual pollsters' forecasts") +
      labs(caption = "Source: polls data collected by Wikipedia, available in the {nzelect} R package")
   
   print(p)
   
   houses_av <- houses %>%
      gather(Party, Bias, -ElectionYear, -Pollster) %>%
      group_by(Party, Pollster) %>%
      summarise(Bias = mean(Bias))
   
   return(houses_av)
}
   
hb1 <- house_bias(elect_years = c(2005, 2008, 2011, 2014),
		  pollsters   = c("Colmar Brunton", "Roy Morgan"))      

hb2 <- house_bias(elect_years = c(2011, 2014),
	   pollsters    = c("Reid Research", "Colmar Brunton", "Roy Morgan"))      

# table for blog post:
hb2 %>%
   filter(Pollster == "Reid Research") %>%
   rbind(hb1) %>%
   arrange(Party, Pollster) %>%
   mutate(`Average bias` = paste0(round(Bias * 100, 1), "%")) %>%
   select(-Bias) %>%
   spread(Pollster, `Average bias`) %>%
   knitr::kable(align = "lrrr")

#===================how many polls per year==========================
polls %>%
   select(ElectionYear, Pollster, MidDate) %>%
   distinct() %>%
   group_by(ElectionYear, Pollster) %>%
   summarise(Polls = n()) %>%
   ungroup() %>%
   mutate(Pollster = fct_reorder(Pollster, Polls)) %>%
   ggplot(aes(x = Polls, y = Pollster, colour = as.factor(ElectionYear))) +
   geom_point() +
   facet_wrap(~ElectionYear) +
   theme(legend.position = "none")


{% endhighlight %}
