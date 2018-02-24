---
layout: post
title: Estimating Gini coefficient when we only have mean income by decile 
date: 2017-08-19
tag: 
   - Inequality
   - R
description: Calculating the Gini coefficient for inequality directly of mean income by decile produces a slightly biased downwards estimate.  I correct for this and demonstrate on the World Panel Income Distribution data.
image: /img/0107-ginis-from-deciles.svg
socialimage: http://ellisp.github.io/img/0107-ginis-from-deciles.png
category: R
---

## Income inequality data 

Ideally the Gini coefficient to estimate inequality is based on original household survey data with hundreds or thousands of data points.  Often this data isn't available due to access restrictions from privacy or other concerns, and all that is published is some kind of aggregate measure.  Some aggregations include the income at the 80th percentile divided by that at the 20th (or 90 and 10); the number of people at the top of the distribution whose combined income is equal to that of everyone else; or the income of the top 1% as a percentage of all income.  I wrote a little more about this in [one of my earliest blog posts](http://ellisp.github.io/blog/2015/09/12/inequality-stats-distributions).

One way aggregated data are sometimes presented is as the mean income in each decile or quintile.  This is not the same as the actual quantile values themselves, which are the *boundary* between categories. The 20th percentile is the value of the 20/100th person when they are lined up in increasing order, whereas the mean income of the first quintile is the mean of all the incomes of a "bin" of everyone from 0/100 to 20/100, when lined up in order.

To explore estimating Gini coefficients from this type of binned data I used data from the wonderful [Lakner-Milanovic World Panel Income Distribution database](https://www.gc.cuny.edu/CUNY_GC/media/LISCenter/brankoData/LaknerMilanovic2013WorldPanelIncomeDistributionLMWPIDDescription.pdf), which is available for free download.  This useful collection contains the mean income by decile bin of many countries from 1985 onwards - the result of some careful and doubtless very tedious work with household surveys from around the world.  This is an amazing dataset, and amongst other purposes it can be used (as [Milanovic and co-authors have pioneered](https://en.wikipedia.org/wiki/Branko_Milanovi%C4%87) dating back to his World Bank days) in combination with population numbers to estimate "global inequality", treating everyone on the planet as part of a single economic community regardless of national boundaries.  But that's for another day.

Here's R code to download the data (in Stata format) and grab the first ten values, which happen to represent Angloa in 1995.  These particular data are based on consumption, which in poorer economies is often more sensible to measure than income:

{% highlight R %}
library(tidyverse)
library(scales)
library(foreign) # for importing Stata files
library(actuar)  # for access to the Burr distribution
library(acid)
library(forcats)

# Data described at https://www.gc.cuny.edu/CUNY_GC/media/LISCenter/brankoData/LaknerMilanovic2013WorldPanelIncomeDistributionLMWPIDDescription.pdf
# The database has been created specifically for the
# paper “Global Income Distribution: From the Fall of the Berlin Wall to the Great Recession”,
# World Bank Policy Research Working Paper No. 6719, December 2013, published also in World
# Bank Economic Review (electronically available from 12 August 2015). 
download.file("https://wfs.gc.cuny.edu/njohnson/www/BrankoData/LM_WPID_web.dta", 
              mode = "wb", destfile = "LM_WPID_web.dta")

wpid <- read.dta("LM_WPID_web.dta")

# inc_con whether survey is income or consumption
# group income decline group 1 to 10
# RRinc is average per capita income of the decile in 2005 PPP

# the first 10 rows are Angola in 1995, so let's experiment with them
angola <- wpid[1:10, c("RRinc", "group")]
{% endhighlight %}

Here's the resulting 10 numbers.  N

<img src='/img/0107-angola-quantiles.svg' width='100%'>

And this is the Lorenz curve:

<img src='/img/0107-angola-lorenz.svg' width='100%'>

Those graphics were drawn with this code:

{% highlight R %}
ggplot(angola, aes(x = group, y = RRinc)) +
  geom_line() +
  geom_point() +
  ggtitle("Mean consumption by decile in Angola in 1995") +
  scale_y_continuous("Annual consumption for each decile group", label = dollar) +
  scale_x_continuous("Decile group", breaks = 1:10) +
  labs(caption = "Source: Lakner/Milanovic World Panel Income Distribution data") +
  theme(panel.grid.minor = element_blank())

angola %>%
  arrange(group) %>%
  mutate(cum_inc_prop = cumsum(RRinc) / sum(RRinc),
         pop_prop = group / max(group)) %>%
  ggplot(aes(x = pop_prop, y = cum_inc_prop)) +
  geom_line() +
  geom_ribbon(aes(ymax = pop_prop, ymin = cum_inc_prop), fill = "steelblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  labs(x = "Cumulative proportion of population",
       y = "Cumulative proportion of consumption",
       caption = "Source: Lakner/Milanovic World Panel Income Distribution data") +
  ggtitle("Inequality in Angola in 1995",
          "Lorenz curve based on binned decile mean consumption")
{% endhighlight %}


## Calculating Gini directly from deciles?

Now, I could just treat these 10 deciles as a sample of 10 representative people (each observation after all represents exactly 10% of the population) and calculate the Gini coefficient directly.   But my hunch was that this would underestimate inequality, because of the straight lines in the Lorenz curve above which are a simplification of the real, more curved, reality.  

To investigate this issue, I started by creating a known population of 10,000 income observations from a [Burr distribution](https://en.wikipedia.org/wiki/Burr_distribution), which is a flexible, continuous non-negative distribution often used to model income.  That looks like this:

{% highlight R %}
population <- rburr(10000, 1, 3, 3)

par(bty = "l", font.main = 1)
plot(density(population), main = "Burr(1,3,3) distribution")
{% endhighlight %}

<img src='/img/0107-burr.svg' width='100%'>

Then I divided the data up into between 2 and 100 bins, took the means of the bins, and calculated the Gini coefficient of the bins.  Doing this for 10 bins is the equivalent of calculating a Gini coefficient directly from decile data such as in the Lakner-Milanovic dataset.  I got this result, which shows, that when you have the means of 10 bins, you are underestimating inequality slightly:

<img src='/img/0107-ginis-from-deciles.svg' width='100%'>

Here's the code for that little simulation.  I make myself a little function to bin data and return the mean values of the bins in a tidy data frame, which I'll need for later use too:

{% highlight R %}
#' Quantile averages
#' 
#' Mean value in binned groups
#' 
#' @param y numeric vector to provide summary statistics on
#' @param len number of bins to calculate means for
#' @details this is different from producing the actual quantiles; it is the mean value of y within each bin.
bin_avs <- function(y, len = 10){
  # argument checks:
  if(class(y) != "numeric"){stop("y should be numeric") }
  if(length(y) < len){stop("y should be longer than len")}
  
  # calculation:
  y <- sort(y)
  bins <- cut(y, breaks = quantile(y, probs = seq(0, 1, length.out = len + 1)))
  tmp <- data.frame(bin_number = 1:len,
                    bin_breaks = levels(bins),
                    mean = as.numeric(tapply(y, bins, mean)))
  return(tmp)
}

ginis <- numeric(99)
for(i in 1:99){
  ginis[i]   <- weighted.gini(bin_avs(population, len = i + 1)$mean)$Gini
}
ginis_df <- data.frame(
  number_bins = 2:100,
  gini = ginis
)

ginis_df %>%
  mutate(label = ifelse(number_bins < 11 | round(number_bins / 10) == number_bins / 10, number_bins, "")) %>%
  ggplot(aes(x = number_bins, y = gini)) +
  geom_line(colour = "steelblue") +
  geom_text(aes(label = label)) +
  labs(x = "Number of bins",
       y = "Gini coefficient estimated from means within bins") +
  ggtitle("Estimating Gini coefficient from binned mean values of a Burr distribution population",
          paste0("Correct Gini is ", round(weighted.gini(population)$Gini, 3), ". Around 25 bins needed for a really good estimate."))
{% endhighlight %}

## A better method for Gini from deciles?

Maybe I should have stopped there; after all, there is hardly any difference between 0.32 and 0.34; probably much less than the sampling error from the survey.  But I wanted to explore if there were a better way.  The method I chose was to: 

- choose a log-normal distribution that would generate (close to) the 10 decile averages we have; 
- simulate individual-level data from that distribution; and
- estimate the Gini coefficient from that simulated data.

I also tried this with a Burr distribution but the results were very unstable.  The log-normal approach was quite good at generating data with means of 10 bins very similar to the original data, and gave plausible values of Gini coefficient just slightly higher than when calculated directly of the bins' means.  

Here's how I did that:

{% highlight R %}
# algorithm will be iterative
# 1. assume the 10 binned means represent the following quantiles: 0.05, 0.15, 0.25 ... 0.65, 0.75, 0.85, 0.95
# 2. pick the best lognormal distribution that fits those 10 quantile values. 
#    Treat as a non-linear optimisation problem and solve with `optim()`.
# 3. generate data from that distribution and work out what the actual quantiles are
# 4. repeat 2, with these actual quantiles

n <- 10000
x <- angola$RRinc

fn2 <- function(params) {
  sum((x - qlnorm(p, params[1], params[2])) ^ 2 / x)
}
p <- seq(0.05, 0.95, length.out = 10)
fit2 <- optim(c(1,1), fn2)
simulated2 <- rlnorm(n, fit2$par[1], fit2$par[2])
p <- plnorm(bin_avs(simulated2)$mean, fit2$par[1], fit2$par[2])
fit2 <- optim(c(1,1), fn2)
simulated2 <- rlnorm(n, fit2$par[1], fit2$par[2])
{% endhighlight %}

And here are the results.  The first table shows the means of the bins in my simulated log-normal population (`mean`) compared to the original data for Angola's actual deciles in 1995 (`x`).   The next two values, 0.415 and 0.402, are the Gini coefficents from the simulated and original data respectively:

```
> cbind(bin_avs(simulated2), x)
   bin_number         bin_breaks      mean    x
1           1         (40.6,222]  165.0098  174
2           2          (222,308]  266.9120  287
3           3          (308,393]  350.3674  373
4           4          (393,487]  438.9447  450
5           5          (487,589]  536.5547  538
6           6          (589,719]  650.7210  653
7           7          (719,887]  795.9326  785
8           8     (887,1.13e+03] 1000.8614  967
9           9 (1.13e+03,1.6e+03] 1328.3872 1303
10         10  (1.6e+03,1.3e+04] 2438.4041 2528
> weighted.gini(simulated2)$Gini
          [,1]
[1,] 0.4145321
> 
> # compare to the value estimated directly from the data:
> weighted.gini(x)$Gini
         [,1]
[1,] 0.401936
```

As would be expected from my earlier simulation, the Gini coefficient from the estimated underlying log-normal distribtuion is verr slightly higher than that calculated directly from the means of the decile bins.

## Applying this method to the Lakner-Milanovic inequality data

I rolled up this approach into a function to convert means of deciles into Gini coefficients and applied it to all the countries and years in the World Panel Income Distribution data.  Here are the results, first over time:

<img src='/img/0107-all-countries-ginis.svg' width='100%'>

.. and then as a snapshot

<img src='/img/0107-all-countries-snapshot.svg' width='100%'>

Neither of these is great as a polished data visualisation, but it's difficult data to present in a static snapshot, and will do for these illustrative purposes.

Here's the code for that function (which depends on the previously defined ) and drawing those charts.  Drawing on the convenience of Hadley Wickham's `dplyr` and `ggplot2` it's easy to do this on the fly and in the below I calculate the Gini coefficients twice, once for each chart.  Technically this is wasteful, but with modern computers this isn't a big deal even though there is quite a bit of computationally intensive stuff going on under the hood; the code below only takes a minute or so to run.

{% highlight R %}
#' Convert data that is means of deciles into a Gini coefficient
#' 
#' @param x vector of 10 numbers, representing mean income (or whatever) for 10 deciles
#' @param n number of simulated values of the underlying log-normal distribution to generate
#' @details returns an estimate of Gini coefficient that is less biased than calculating it
#' directly from the deciles, which would be slightly biased downwards.
deciles_to_gini <- function(x, n = 1000){
  fn <- function(params) {
    sum((x - qlnorm(p, params[1], params[2])) ^ 2 / x)
  }
  
  # starting estimate of p based on binned means and parameters
  p <- seq(0.05, 0.95, length.out = 10)
  fit <- optim(c(1,1), fn)
  
  # calculate a better value of p
  simulated <- rlnorm(n, fit$par[1], fit$par[2])
  p <- plnorm(bin_avs(simulated)$mean, fit$par[1], fit$par[2])
  
  # new fit with the better p
  fit <- optim(c(1,1), fn)
  simulated <- rlnorm(n, fit$par[1], fit$par[2])
  output <- list(par = fit$par)
  output$Gini <- as.numeric(weighted.gini(simulated)$Gini)
  return(output)
}

# example usage:
deciles_to_gini(x = wpid[61:70, ]$RRinc)
deciles_to_gini(x = wpid[171:180, ]$RRinc)

# draw some graphs:
wpid %>%
  filter(country != "Switzerland") %>%
  mutate(inc_con = ifelse(inc_con == "C", "Consumption", "Income")) %>%
  group_by(region, country, contcod, year, inc_con) %>%
  summarise(Gini = deciles_to_gini(RRinc)$Gini) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = Gini, colour = contcod, linetype = inc_con)) +
  geom_point() +
  geom_line() +
  facet_wrap(~region) +
  guides(colour = FALSE) +
  ggtitle("Inequality over time",
          "Gini coefficients estimated from decile data") +
  labs(x = "", linetype = "",
       caption = "Source: Lakner/Milanovic World Panel Income Distribution data") 

wpid %>%
  filter(country != "Switzerland") %>%
  mutate(inc_con = ifelse(inc_con == "C", "Consumption", "Income")) %>%
  group_by(region, country, contcod, year, inc_con) %>%
  summarise(Gini = deciles_to_gini(RRinc)$Gini) %>%
  ungroup() %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, Gini),
         region = fct_lump(region, 5)) %>%
  ggplot(aes(x = Gini, y = country, colour = inc_con, label = contcod)) +
  geom_text(size = 2) +
  facet_wrap(~region, scales = "free_y", nrow = 2) +
  labs(colour = "", y = "", x = "Gini coefficient",
       caption = "Source: Lakner-Milanovic World Panel Income Distribution") +
  ggtitle("Inequality by country",
          "Most recent year available up to 2008; Gini coefficients are estimated from decile mean income.")
{% endhighlight %}

There we go - deciles to Gini fun with world inequality data!

{% highlight R %}
# cleanup
unlink("LM_WPID_web.dta")
{% endhighlight %}
