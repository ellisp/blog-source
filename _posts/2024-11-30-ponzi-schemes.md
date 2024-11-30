---
layout: post
title: Simulating Ponzi schemes
date: 2024-11-30
tag: 
   - Simulations
   - Economics
   - Pacific
   - Distributions
description: I write a function to simulate Ponzi schemes with various types of 'investor' growth, withdrawal rates, and extraction by the scammer / owner of the scheme.
image: /img/0283-ponzi2.svg
socialimage: https:/freerangestats.info/img/0263-ponzi2.png
category: R
---
I recently read John Cox's 2018 book [Fast Money Schemes: Hope and Deception in Papua New Guinea](https://www.amazon.com/Fast-Money-Schemes-Deception-Framing/dp/0253025605) and gave it a rare (for me) five star rating. It's fascinating research into those involved in the [U-Vistract](https://en.wikipedia.org/wiki/U-Vistract) pyramid scheme. 

At the time of writing U-Vistract had been bankrupt and exposed for years yet many of those who had 'invested' in it were still hoping to get their money back. Cox conducted in-depth interviews not just with victims but with some of those involved in running the scam. He successfully puts it all in a wider context, not just of Melanesian history with cults and cons but its place in global "prosperity theology" (good grief), international financial cons such as the "Nigerian Prince scam" (not a Ponzi scheme) and the biggest Ponzi scheme of all, Bernie Madoff's multi-billion dollar rip-off of some of the more privileged, educated and well-connected people on the planet.

U-Vistract (which still has its true believers, apparently) targeted not pre-modern villagers in the Papua New Guinea highlands but the growing middle class that lives in the formal economy and is connected to or part of the governing elites. Part of the appeal of Cox's book is that it is "post-village ethnology", looking at society as it is now, not just interested in some pre-contact tribal life.

Anyway, it got me thinking about Ponzi schemes. U-Vistract's mode of operation was to get people to invest 100 or so kina and promise a 100% return per month. In fact, part of U-Vistract's propaganda appeal was a criticism of traditional banks for giving you only 5% per year or so of the full 100% that must exist somewhere. This got me thinking, how fast does a Ponzi scheme need to grow new investor/victims to sustain a promised rate of return of doubling their money *every month*? How long could such a scheme plausibly last?

Of course, this depends not just on how many new investors come in, but other factors such as how often they seek to withdraw their money. In the U-Vistract world several early investors - often well known and apparently respectable - made early fortunes. Some subsequent investors sought more modestly just to withdraw 100 kina a month (still a stunning return on an initial investment of the same amount) or even to make a steady savings-like contribution of that amount, whereas others waited to see their on-paper fortune climb into the millions.

To find out how it worked I wrote an R function that parameterises all this. Full details are to the end of the blog, but the key parameters are:

* number of new investors that come in each month
* mean and variance of the amount invested by each investor
* a four-way set of proportions indicating what happens to investors each month:
  - seek to withdraw it all
  - seek to withdraw a smaller amount about the same as initial contribution
  - invest a further amount roughly equal to the initial contribution
  - leave it alone and just roll over the existing investment
* proportion of the real money in the scheme that is extracted each month by the scammers running it

Caveat - I'm not a financial fraud expert and haven't bothered to do even a minimal literature review in this space. I'm just toying around, and am probably reinventing some wheels, and possibly falling into beginner traps (assuming that there are others who *are* such experts).

Here is a simulated Ponzi scheme with a plausible starting set of values for all of those

<object type="image/svg+xml" data='/img/0283-ponzi1.svg' width='100%'><img src='/img/0283-ponzi1.png' width='100%'></object>

created with this code:

{% highlight R lineanchors %}
set.seed(123)
ponzi(invest_more_rate = 0.2,
      withdraw_all_rate = 0.01,
      cv = 2, keep_history = TRUE) |>
  ponzi_plot()
{% endhighlight %}

In this version, all investors put in $100 on average (but random log-normal distribution with standard deviation of 200) and we start with 10 such people. 20% of them decide they are on a particularly good thing, and put in another ~$100 per month; 10% (the default value, so not shown explicitly) try to withdraw just ~$100, 1% try to withdraw everything at the end of any given month and the remainder (so 69%) just roll over whatever they have.

Not shown here is the default growth path in new investor/victims, which I set to be 10 new investors in the first month, then 20, 30, 40, 50 etc. Which looks like a lot of new people but is actually a declining growth rate each month; by month 10 or so this is down to about 20% growth per month in investors and declining further, noticeably less than the promised growth rate of 100% per month in paper money. So it's got to end in tears quickly!

In fact, we see the total paper value of the scheme gets up to about $1m after about 10 months and then the scheme collapses - one investor too many tries to cash in on their paper value and the scheme goes bankrupt. The total paper value got nearly up to $1m; the real amount invested was about $64k, and the scammers got away with only $1,524.

That's a little hard to read off the graphic with the log scales, so here's the actual numbers I was quoting there:

```
> set.seed(123)
> ponzi(invest_more_rate = 0.2,
+             withdraw_all_rate = 0.01,
+             cv = 2, keep_history = TRUE) |>
+   ponzi_plot(return_numbers = TRUE)
                      [,1]
total_invested   64302.256
total_withdrawn 103190.149
total_extracted   1524.389
paper_value     888278.519
months               9.000
total_investors    450.000
leverage            14.000
```
Note that I've ducked the ugly question of exactly what happens when bankruptcy happens. The "total withdrawn" is $103k but only $64k was ever put in. Someone tried to withdraw $39k more than was available. That would have been ugly (as indeed would have been the case when the other people who thought they had nearly $900k of value missed out).

The actual amount extracted is pretty modest compared to the high paper value of the scheme just before disaster. But this was a very unstable scheme. Here's an alternative where I let the number of new investors grow steadily at 50% per month, plus have a generous 50% of existing investors put more in; and reduce the proportion of those seeking to withdraw their whole fortune to just 0.1%.

{% highlight R lineanchors %}
ponzi(number_investors = round(10 * 1.5 ^ (1:100)),
     invest_more_rate = 0.5, 
     withdraw_all_rate = 0.001,
     cv = 2,
     keep_history = TRUE) |>
  ponzi_plot()
{% endhighlight %}

<object type="image/svg+xml" data='/img/0283-ponzi2.svg' width='100%'><img src='/img/0283-ponzi2.png' width='100%'></object>

Now the scheme lasts a couple of years; the paper value got well into the billions; and the scammers were able to extract well over $1 million for themselves. Intuitively, the big difference here is that the number of new investor/victims was growing consistently at 50% per month. Nowhere near as much as the doubling that intuitively is needed if we are to keep the paper value of things doubling plausibly, but enough to keep things going for a couple of years.

To explore more systematically the impact of these different parameters I ran a bunch of simulations with this code:

{% highlight R lineanchors %}
#--------------systematic exploration-----------

set.seed(123)
params <- expand_grid(
  investor_growth = c(1, 1.2, 1.4, 1.6, 1.8),
  cv = c(0, 0.5, 1),
  invest_more_rate = c(0, 0.1, 0.2),
  withdraw_all_rate = c(0.001, 0.01, 0.05),
  extraction_rate = c(0.001, 0.01, 0.05)
) |>
  # give me three of each
  slice(1:n(), 1:n(), 1:n()) |>
  # bit of randomness in one of the most important parameters
  mutate(investor_growth = investor_growth + runif(n(), -0.1, 0.1)) |>
  mutate(i = 1:n()) 

# set up parallel processing cluster
cluster <- makeCluster(7) 
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(foreach)
  library(tidyverse)
})

clusterExport(cluster, c("params", "ponzi"))

results <- foreach(i = 1:nrow(params), .combine = rbind) %dopar% {
  set.seed(i)
  
  ni <- round(10 * params[i, ]$investor_growth ^ (1:2000))
  
  this_sim <- ponzi(number_investors = ni,
                  invest_more_rate = params[i, ]$invest_more_rate, 
                  withdraw_all_rate = params[i, ]$withdraw_all_rate,
                  extraction_rate = params[i, ]$extraction_rate,
                  cv = params[i, ]$cv,
                  ceiling = 1e7,
                  keep_history = FALSE) |>
    filter(time_period == max(time_period))|>
    summarise(total_invested = sum(invested),
              total_withdrawn = sum(withdrawn),
              total_extracted = sum(extracted),
              paper_value = sum(value),
              months = unique(time_period),
              total_investors = max(id),
              leverage = round(paper_value / total_invested),
              i = i)
  
  return(this_sim)
  
}
{% endhighlight %}

This took half an hour or more to run (I didn't time it). Most of the scenarios are quick to run, but those where the number of new investors increases by around 1.8 times per month (ie nearly doubling) can go for a long time. How long? Let's see the impact of all those parameters on the time each scheme could run before it collapsed:

<object type="image/svg+xml" data='/img/0283-months.svg' width='100%'><img src='/img/0283-months.png' width='100%'></object>

The interesting turning point in these charts comes when the number in new victim/investors hits the ceiling of 10 million people. Growth rates can't conceiveably keep on forever like this. But what we see here is that, a few random cases aside, the best chance of prolonging a Ponzi scheme is to have one where the exit rate (ie the proportion of investors who try to pull all their paper value out of the scheme at once) is as low as possible. Which seems intuitive.

We get a different angle when we focus not on how long the scheme continues, but on the total dollar value extracted by the scammers:

<object type="image/svg+xml" data='/img/0283-extract.svg' width='100%'><img src='/img/0283-extract.png' width='100%'></object>

Again we see that things flattern out in those scams that hit the 10 million person ceiling (which by the way is absurdly too high to be realistic - I hope). But we can clearly see that the schemes that make the most money for the scammers are those with growth rates of 1.5 or higher, high extraction rates, and low withdrawal rates. Which is all intuitive.

Those two charts were drawn with this code:

{% highlight R lineanchors %}
results |>
  left_join(params, by = "i") |>
  mutate(extraction_rate = glue("Extrct: {extraction_rate}"),
         withdraw_all_rate = glue("Exit: {withdraw_all_rate}")) |>
  ggplot(aes(x = investor_growth, y = months, colour = as.ordered(cv))) +
  facet_grid(extraction_rate~withdraw_all_rate) +
  geom_jitter()  +
  labs(colour = "Variation in investor amounts",
       y = "Length of scam (in months)",
       x = "Monthly growth in number of 'investors'\n(2 = doubling per month; capped at 10 million)",
       subtitle = "A low 'exit rate' (by investors/victims) is key to prolonging a Ponzi scheme",
       title = "How long can a Ponzi scheme last?")

p4 <- results |>
  left_join(params, by = "i") |>
  mutate(extraction_rate = glue("'Tax' scammed:\n{extraction_rate}"),
         withdraw_all_rate = glue("Exit: {withdraw_all_rate}")) |>
  ggplot(aes(x = investor_growth, y = total_extracted, colour = as.ordered(cv))) +
  facet_grid(extraction_rate~withdraw_all_rate) +
  geom_jitter() +
  scale_y_log10(label = dollar) +
  labs(colour = "Variation in investor amounts",
       y = "Total extracted by scammers",
       x = "Monthly growth in number of 'investors'\n(2 = doubling per month; capped at 10 million)",
       subtitle = "Key success factor for getting rich from a Ponzi scheme is to quickly and consistently grow your 'investors' and extract as much 'tax' you can.",
       title = "How much can the scammers extract from a Ponzi scheme?")
{% endhighlight %}

Now, I don't know what set of parameters are realistic for different scams. In a different world, we could observe a bunch of these things - how long scams continue, the growth rate in victim/investors, what proportion of them withdraw their money or invest more, and so on, and create real-life calibrated models. In fact, it seems likely to me that someone has done this! I haven't bothered to look, because my interest is basically whimsical and curiousity. And I've scratched that itch sufficiently to get an idea that maintaining a Ponzi scheme that promises to double investors' money every month is going to be very, very hard to maintain for more than 10 months.

## Supporting code

This section has the code defining functions used in the main part of the blog. You have to run these definitions before running the code above, if you are following along. Or you could just go to [the source of the blog](https://github.com/ellisp/blog-source/blob/master/_working/0283-ponzi.R) to see it directly as run by me.

I've put this stuff at the end of the blog because it's quite long and I thought likely to put off people who wanted to just read about Ponzi schemes.

### Log-normal distrbution

First there's a little function to generate samples from a log normal distribution. Now of course there is a function in R to do this already, `rlognorm()`, but it is parameterised by the mean and standard deviation of the underlying normal distribution that you get by taking the logarithm of the distribution we actually observe. I wanted, for easy interpretability, to be able to specify the mean and standard deviation (or in the end I chose the coefficient of variation, which is just the standard deviation except, well, standardised so it is described relative to the mean). I was surprised to find there wasn't already a function doing this; probably if I looked I harder I'd find one. But this is what I ended up with from rolling my own:

{% highlight R lineanchors %}
library(tidyverse)
library(foreach)
library(doParallel)
library(glue)

#' Generate samples from a log normal distribvution given E(X) and coefficient
#' of variation
#'
#' @param n number of samples to generate
#' @param ex expected value of the distribution, on its observed (not log) scale
#' @param cv coefficient of variation (ie standard deviation as a proportion of
#'   the mean) of the distribution, on its observed (not log) scale
#' @details Only needed because rlnorm() has the parameters of the Normal
#'   distribution that log(X) follows and I wanted a version that used the
#'   actual mean of X and its coefficient of variation.
#' 
rlognormal <- function(n, ex, cv){
  if(cv == 0){
    output <- rep(ex, n)
  } else {
    
    sdlog <- sqrt(log(cv ^ 2 + 1)) 
    
    meanlog <- log(ex) - (sdlog ^ 2) / 2
    
    
    output <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  }
  
  return(output)
  
}

# check that this function works

# If cv is zero, should just return a whole bunch of ex (i.e. 100):
stopifnot(mean(rlognormal(1000, 100, 0)) == 100)
stopifnot(sd(rlognormal(1000, 100, 0)) == 0)

# If cv is not zero, check that the mean and coefficient of variation
# are as expectd:
set.seed(321)
stopifnot(round(mean(rlognormal(100000, 100, 0.5))) == 100)
stopifnot(round(sd(rlognormal(100000, 100, 0.5)) / 100, 1) == 0.5)
{% endhighlight %}

Those tests are all passed ok. Here are a couple of demo distributions:

<object type="image/svg+xml" data='/img/0283-log-normal.svg' width='100%'><img src='/img/0283-log-normal.png' width='100%'></object>

produced with:

{% highlight R lineanchors %}
par(bty = "l", mfrow = c(1, 2))
plot(density(rlognormal(1000, 100, cv = 0.5)), 
     main = "Log-normal distribution,\nmean 100 and cv 0.5")
plot(density(rlognormal(1000, 100, cv = 2)), 
     main = "Log-normal distribution\nmean 100 and cv 2")
{% endhighlight %}

### Ponzi scheme

Here's the code for the main workhorse function simulating the Ponzi scheme

{% highlight R lineanchors %}
#' Simulate ponzi scheme that doubles in paper value each time period
#'
#' @param number_investors vector of number of new investors joining the scheme
#'   each time period. If this is less than 2000 time periods long it will be
#'   filled out with repeats of the last element.
#' @param mu average investment of each investor when they first join or invest
#'   further
#' @param cv coefficient of variation of the amount that investors each invest
#' @param invest_more_rate the proportion of investors in each time period that
#'   invest additional funds
#' @param withdraw_small_rate the proportion of investors in each time period
#'   that withdraw a small amount equivalent to their original investment
#' @param withdraw_all_rate the proportion of investors in each time period that
#'   withdraw the entire paper value of their investment
#' @param roll_over_rate the proportion of investors who simply leave their
#'   investment as it is at the end of each investment, neither withdrawing or
#'   investing further
#' @param ceiling the maximum number of investors that can ever be involved in
#'   the scheme, including those that have already withdrawn all funds. If the
#'   cumulative sum of number_investors exceeds ceiling then no more investors
#'   are added ie \code{ceiling} overrides \code{number_investors}
#' @param extraction_rate The proportion of the total real value of the scheme
#'   that the owners extract for their own use, each time period
#' @param keep_history whether to return the state of the scheme at each
#'   time_period (if TRUE), or only the final state (if FALSe)
#' @param time_multiplier how much the investors are promised their investment
#'   increases by each time period; defaults to 2 ie doubling
ponzi <- function(number_investors = c(1:100, 100:1) * 10, 
                  mu = 100, 
                  cv = 0, 
                  invest_more_rate = 0.1,
                  withdraw_small_rate = 0.1,
                  withdraw_all_rate = 0.05,
                  roll_over_rate = (1 - invest_more_rate - 
                                         withdraw_small_rate - withdraw_all_rate),
                  ceiling = 1e7,
                  extraction_rate = 0.01,
                  keep_history = TRUE,
                  time_multiplier = 2){
  
  #---------checks on number of investors------------
  if(min(number_investors) < 0){
    stop("number_investors should be a vector of numbers of 0 or greater")
  }
  
  if(number_investors[1] < 1){
    stop("First element of number_investors should be greater than 0")
  }
  
  number_investors <- round(number_investors)
  
  # make sure we have enough for 2000 time periods
  nni <- length(number_investors) 
  if(nni < 2000){
    number_investors <- c(number_investors, 
                          rep(number_investors[length(number_investors)]), 2000 - nni)
  }
  
  #--------------Other checks--------------
  if(invest_more_rate + withdraw_small_rate + withdraw_all_rate + roll_over_rate != 1){
    stop("invest_more_rate, withdraw_small_rate, withdraw_all_rate and roll_over_rate should add to one")
  }
  
  if(extraction_rate < 0 | extraction_rate > 1){
    stop("extraction_rate should be between zero and one")
  }
  
  #---------------------set up month 1------------------
  number_new_investors <- number_investors[1]
  
  status <- tibble(id = 1:number_new_investors,
                   invested = rlognormal(number_new_investors, mu, cv),
                   value_tmp = invested,
                   value = invested,
                   withdrawn = 0,
                   time_period = 1,
                   extracted = 0)
  
  cash_on_hand <- with(status, sum(invested) - sum(withdrawn) - sum(extracted))
  
  #---------------------months 2 and onwards---------------------
  while(cash_on_hand > 0 & max(status$id) < ceiling){
    
    # we make a new state for our existing investors, which is going to later be
    # appended to the state so far:
    update <-  status |>
      # limit to the state from the last time_period:
      filter(time_period == max(time_period)) |>
      # Double the 'value' of existing investments
      mutate(value_tmp = value_tmp * time_multiplier) |>
      # Decide for each investor what they are going to do this time period:
      mutate(action = sample(c("withdraw_all", "withdraw_small", "rollover", "invest"),
                             size = n(),
                             replace = TRUE,
                             prob = c(withdraw_all_rate, withdraw_small_rate, 
                                      roll_over_rate, invest_more_rate))) |>
      # An incremental amount which might be used for furhter investments or for withdrawals:
      mutate(incr_tmp = rlognormal(n(), mu, cv)) |>
      # some people try to withdraw their paper value. This increases the amount
      # they have ever withdrawn:
      mutate(withdrawn = case_when(
        action == "withdraw_all" ~ withdrawn + value_tmp,
        action == "withdraw_small" ~ withdrawn + incr_tmp,
        TRUE ~ withdrawn
      )) |>
      # ... and decreases their paper value by the same amount
      mutate(value = case_when(
        action == "withdraw_all" ~ 0,
        action == "withdraw_small" ~ value_tmp - incr_tmp,
        action == "rollover" ~ value_tmp,
        action == "invest" ~ value_tmp + incr_tmp
      )) |>
      # Some other people choose to invest more:
      mutate(invested = case_when(
        action == "invest" ~ invested + incr_tmp,
        TRUE ~ invested
      ))  |>
      # the people running the Ponzi scheme extract a percentage of the
      # real money available, calculated per investor (so fully withdrawn
      # investors not included):
      mutate(extract_tmp = pmax(0, (invested - withdrawn - extracted) * extraction_rate),
             extracted = extracted + extract_tmp) |>
      select(-action, -incr_tmp, -extract_tmp) |>
      # time period goes up one:
      mutate(time_period = max(status$time_period + 1))
    
    # We are also going to get a number of new investors:
    number_new_investors <- number_investors[unique(update$time_period)]
    if(number_new_investors > 0){
    
      new_investors <- tibble(id = 1:number_new_investors + max(status$id),
                              invested = rlognormal(number_new_investors, mu, cv),
                              value_tmp = invested,
                              value = invested,
                              withdrawn = 0,
                              time_period = unique(update$time_period),
                              extracted = 0)
    } else {
      new_investors <- tibble()
    }  
    
    # depending on whether the user wants all the history, we either append our
    # new update for the previous investors and our new investors to the old state,
    # or we just keep the latest update on previous investors plus our new investors:
    if(keep_history){
      status <- rbind(status, update, new_investors)
    } else {
      status <- rbind(update, new_investors)
    }
    
    # We need to determine if the scheme has gone bust, by calculating its
    # actual cash available (the amount invested so far, minus the total
    # withdrawn and total extracted)
    cash_on_hand <- status |>
      filter(time_period == max(time_period)) |>
      summarise(x = sum(invested) - sum(withdrawn) - sum(extracted)) |>
      pull(x)
    
  }  
  
  return(status)
}
{% endhighlight %}

### Ponzi summary table and plot

Finally, here's a function that takes the output of a single Ponzi simulation and draws the chart showing extraction, paper value, and real value invested over time.

{% highlight R lineanchors %}
ponzi_plot <- function(status, return_numbers = FALSE){

  numbers <- status  |>
    dplyr::filter(time_period == max(time_period)) |>
    dplyr::summarise(total_invested = sum(invested),
              total_withdrawn = sum(withdrawn),
              total_extracted = sum(extracted),
              paper_value = sum(value),
              months = unique(time_period),
              total_investors = max(id),
              leverage = round(paper_value / total_invested)) |>
    t()
  
  plot <- status  |>
    dplyr::group_by(time_period) |>
    dplyr::summarise(`Paper value` = sum(value),
              `Real value` = pmax(0, sum(invested) - sum(withdrawn)),
              `Extracted by scammers` = sum(extracted)) |>
    tidyr::gather(variable, value, -time_period) |>
    ggplot2::ggplot(aes(x = time_period, y = value, colour = variable)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10(label = dollar) +
    labs(colour = "", y = "Value", x = "Number of months")
  
  if(return_numbers){
    return(numbers)
  } else {
    return(plot)
  }
}
{% endhighlight %}

That's all folks. Take care out there, and if the rate of return on a proposed investment is too good to be true, it probably isn't true.


