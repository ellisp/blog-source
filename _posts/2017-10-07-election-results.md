---
layout: post
title: New Zealand 2017 election results
date: 2017-10-07
tag: 
   - NewZealand
   - VotingBehaviour
   - R
description: New Zealand's election results have been released and were within the range of my probabilistic predictions.  The pollsters did a good job.
image: /img/0111-histograms.svg
socialimage: http://ellisp.github.io/img/0111-histograms.png
category: R
---

## Election results are in

The New Zealand Electoral Commission have released the [final count](http://www.electionresults.govt.nz/electionresults_2017/) of votes and seats from the 2017 election.  Moving in the direction most sensible observers expected, the National Party lost two seats compared to the "on-the-night" provisional vote once all the special votes were in, and the final results are:

|  Party  |  Seats  |
|:----|----:|
National   | 56 |
Labour |46 |
New Zealand First      | 9 |
Greens | 8 |
ACT | 1 |

Negotiations will take a few days more on the shape of the government, but the make-up of Parliament is now fixed (bar unlikely challenges) and the result is firmly in the "New Zealand First needed to form a coalition" category. Other coalitions are still technically possible but politically infeasible, despite the best attempts of some people over the last week to drum up support for improbable combinations.


## Comparison to my forecasts

The summary of all my [historical forecasts](http://ellisp.github.io/elections/changelog.html) is available for posterity.  When I started doing forecasts for the election back in April 2017, I estimated around a 20% chance of this outcome.  This was a bit against the common expectation at that point - most pundits and people I spoke to seemed to think the National-led coalition was headed for a fourth election win.  The highest chance I ever gave that possibility was in July 2017 when I gave it just less than 50%; from that point onwards the chances of the National coalition plummeted amidst a wave of positive sentiment to the Labour party under their new leader.  

By the eve of the election there was little doubt of the result, barring a major polling error.  Labour and Greens never had a realistic chance of government by themselves, and the most plausible outcome by far was a coalition of either National or Labour/Green with New Zealand First.  My [final forecasts](http://ellisp.github.io/elections/combined.html) gave this outcome a 95% chance.

Here's the end results in numbers of seats compared to my final probabilistic forecasts:

<img src='/img/0111-histograms.svg' width='100%'>

National outperformed my forecasts a bit, and Labour and New Zealand First underperformed.  The Māori Party wipeout was a significant forecasting failure on my part, but not that surprising a one.  I got their party vote more or less right, but with no significant electorate-level polling data there was no real way of estimating how they'd go in the six Māori electorates they were relying on for Parliamentary representation.  

Note that on the eve of the election, I gave *zero* seats as the single most likely outcome for the Greens, with a one in four chance of that outcome.  When we can consider a range of outcomes though, 6-9 seats was most likely and this is where they ended up.

## The pollsters did a good job

Only three firms published regular polls, making prediction uncertain, and volatility difficult to estimate.  However, the net results were very good.  A simple average of recent polls came within a point or two of the actual results.

[Grumpollie](https://grumpollie.wordpress.com/2017/10/07/how-did-the-polls-do-final-outcome/) has compared the success or otherwise of the pollsters' point estimates and of those of us who aggregated them by various means. The dominating fact is that there wasn't a major polling error.  Reid Research and Colmar Brunton both did better than Roy Morgan.  My own party vote point forecasts weren't particularly good, but that was never the point for me, as much as quantifying the uncertainty, particularly after converting to seats.  My hunch is that there was a swing back towards National in the last two or three weeks before the election, and with so few data points the evidence for this was treated sceptically by my (deliberately) slow moving models.

Predicting New Zealand elections is easier than in many other jurisdictions because the most important number by far is simply the national party vote.  If the US presidential elections were as simple as that, all the predictions in 2016 would have been right (as the pollsters correctly picked a nation-wide popular vote for Hillary Clinton).

## Code

Here's the code that creates the graphic comparing actual results to the probabilistic forecasts.  This is adapted from the code under the hood of my ["choose your own coalition" web tool](https://ellisp.shinyapps.io/nz-election-2017/).

{% highlight R %}
#-------------functionality and data----------------

library(tidyverse)
library(scales)
library(nzelect)
library(grid)
library(forcats)
library(testthat)

# load up the last prediction data, which is a data frame of 4,800 rows
# (one for each simulated result) and 10 columns of party results

download.file("https://github.com/ellisp/ellisp.github.io/raw/source/data/ellis-final-nz-election-forecasts-2017.rda",
              destfile = "tmp.rda", mode = "wb")
load("tmp.rda")
unlink("tmp.rda")


#------------electorate seats that matter---------
# probability of Labour win in each of the seven Maori seats:
maori_probs <- data.frame(Labour = c(0.49, 0.52, 0.55, 0.58, 0.48, 0.64, 0.3)) %>%
  mutate(Other = 1 - Labour)

n <- nrow(sims) # number of simulations ie 4800

filler <- data.frame(
  party = c("Conservative", "Green", "NZ First", "United Future"),
  seats = c(0, 0, 1,0),
  sim = rep(1:n, each = 4)
)

# probability of ACT win in Epsom
epsom <- 0.8

# simulate electorate seat results:
electorate_sims <- data_frame(
  epsom = sample(c("ACT", "National"), prob = c(epsom, 1 - epsom), size = n, replace = TRUE),
  m1 = sample(c("Labour", "Maori"), prob = maori_probs[1, 1:2], size = n, replace = TRUE),
  m2 = sample(c("Labour", "Maori"), prob = maori_probs[2, 1:2], size = n, replace = TRUE),
  m3 = sample(c("Labour", "Maori"), prob = maori_probs[3, 1:2], size = n, replace = TRUE),
  m4 = sample(c("Labour", "Maori"), prob = maori_probs[4, 1:2], size = n, replace = TRUE),
  m5 = sample(c("Labour", "Mana"),  prob = maori_probs[5, 1:2], size = n, replace = TRUE),
  m6 = sample(c("Labour", "Maori"), prob = maori_probs[6, 1:2], size = n, replace = TRUE),
  m7 = sample(c("Labour", "Maori"), prob = maori_probs[7, 1:2], size = n, replace = TRUE)
) %>%
    mutate(sim = 1:n()) %>%
    gather(seat, party, -sim) %>%
    group_by(party, sim) %>%
    summarise(seats = n()) %>%
    ungroup() %>%
    rbind(filler) %>%
    spread(party, seats, fill = 0)

#-------------convert to total seats-------------------------
seats <- t(sapply(1:n, function(i){
  allocate_seats(votes      = as.numeric(sims[i, 1:9]), 
                 electorate = as.numeric(electorate_sims[i, -1]),
                 parties    = gsub("M.ori", "Maori", names(sims)[1:9]))$seats_v
})) %>%
    as_tibble()

#-------------compare to actual results----------------------

actual_results <- data_frame(
  party = c("ACT", "Green", "Labour", "Mana", "Maori", "National", "NZ First"),
  final_seats = c(1, 8, 46, 0, 0, 56, 9)
)
expect_equal(sum(actual_results$final_seats), 120)

d <- seats %>%
  gather(party, n_seats) %>%
  filter(!party %in% c("Conservative", "United Future", "Mana")) %>%
  left_join(actual_results) %>%
  mutate(success = ifelse(n_seats == final_seats, "Actual result", "Probability of other results")) %>%
  mutate(party = fct_reorder(party, desc(n_seats)))


# see https://stackoverflow.com/questions/4646020/ggplot2-axis-transformation-by-constant-factor
# for this idea to do a linear transformation of the y axis so it is probabilities rather than
# counts of the 4800 simulations:
formatter <- function(x, n = 4800){ 
  format(signif(x / n , 2), digits = 3)
}

levels(d$party)[levels(d$party) == "Maori"] <- "M\U0101ori"

d %>%
  ggplot(aes(x = n_seats, fill = party, alpha = success)) +
  facet_wrap(~party, scales = "free") +
  geom_histogram(colour = "white", binwidth = 1) +
  geom_histogram(data = filter(d, success == "Actual result"), colour = "white", fill = "grey10", binwidth = 1) +
  scale_alpha_manual("", values = c(`Actual result` = 0.9, `Probability of other results` = 0.3)) +
  labs(x = "Number of seats") +
  scale_y_continuous("Probability of outcome\n", labels = formatter) +
  ggtitle("Comparison of forecast and actual number of seats in the 2017 New Zealand election",
          "Forecasts are the combination of Peter's Stats' Stuff Models A and B") +
  scale_fill_manual(values = parties_v, guide = FALSE) 
{% endhighlight %}

