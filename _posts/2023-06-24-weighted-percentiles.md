---
layout: post
title: Weighted versus unweighted percentiles
date: 2023-06-24
tag: 
   - Surveys
   - Distributions
   - Inequality
   - Simulations
description: When working with complex survey data where the weights are related to a continuous variable of interest, using a weighted rather than unweighted percentile rank will lead to different results towards the middle of the distribution; but the two measures will be highly correlated with eachother. Also, R reportedly calculates weighted percentile ranks much much faster than Stata.
image: /img/0250-scatter2.svg
socialimage: http://freerangestats.info/img/0250-scatter2.png
category: R
---

This interesting paper came out recently: [A test of the predictive validity of relative versus absolute income for self-reported health and well-being in the United States](https://www.demographic-research.org/volumes/vol48/26/), by David Brady, Michaela Curran and Richard Carpiano. It uses a large sample of longitudinal data to exploit between-individual and within-individual (across time) variation in absolute and relative income to explore which one is more associated with well-being. Seems that in the USA, relative income is more important.

"Relative income" here is defined as your position (percentile rank) in the national distribution of incomes in a particular year. The average relative income should be 50 by definition. I noticed that the mean percentile reported in table A.1 was 57, not 50, which led to this [slightly confusing (for me, anyway) exchange on Twitter](https://twitter.com/ellis2013nz/status/1671996132757757953). It sparked my interest in a broader question - what is the relationship between weighted and unweighted percentile rank when using survey data?

## Weighted percentile rank

I was surprised to find (well actually, I had discovered this before, so really should say to be "reminded") that there doesn't seem to be a weighted version of dplyr's `percent_rank()` function in dplyr or other R packages. There are several packages with functions that will estimate quantiles from weighted data, but if you want to then turn the original vector of continuous variables into percentiles you need to take those quantiles and `cut` the original variable using them as breaks.

I started by making my own convenience function to do this. To calculate the breaks, I am using the `wtd.quantile` function from Mark Hancock's `reldist` package, which was notably faster than its competition (more on this later).

*Post continues after R code*
{% highlight R lineanchors %}
library(DescTools)
library(microbenchmark)
library(modi)
library(reldist)
library(sampling)
library(tidyverse)
library(glue)
library(kableExtra)

#' Weighted percent rank
#' 
#' @param x a vector
#' @param weights weights eg survey or frequency weights
#' @param q quantiles to compute as the basis for cutting x, 
#'   passed through to reldist::wtd.quantile 
#' @details
#' This is a bit of a hack, a homemande version of dplyr::percent_rank
#' I haven't thought through things like NAs, ties, etc
#' @importFrom reldist wtd.quantile
#' @export
wt_percent_rank <- function(x, 
                            weights, 
                            q = (0:1000) / 1000){
  # fastest method to calculate breaks
  breaks <- reldist::wtd.quantile(x, weight = weights, q = q)
  # problem sometimes with ties in the breaks. This forces a way through:
  breaks <- unique(breaks)
  breaks[1] <- -Inf
  # cut the data where the quantile breaks are:
  y <- cut(x, breaks = breaks, labels = FALSE) # labels = FALSE for speed
  
  # convert this into a number from 0 to 1
  y <- as.numeric(y) - 1
  y <- y / max(y)
  return(y)
}

# for unweighted data, wt_percent_rank should give identical
# results to percent_rank
x <- rnorm(1000)
tibble(homemade = wt_percent_rank(x, weights = rep(1, length(x))),
      dplyr = percent_rank(x)) |>
  mutate(difference = homemade - dplyr) |>
  arrange(desc(abs(difference)))
{% endhighlight %}

That little test of whether I get the same result as `dplyr::percent_rank()` with unweighted data turns out ok:

```
  homemade dplyr difference
      <dbl> <dbl>      <dbl>
 1    0.189 0.189          0
 2    0.557 0.557          0
 3    0.869 0.869          0
 4    0.944 0.944          0
 5    0.704 0.704          0
 6    0.448 0.448          0
 7    0.411 0.411          0
 8    0.121 0.121          0
 9    0.584 0.584          0
10    0.307 0.307          0
# ℹ 990 more rows
```

## Simulating a population and drawing a sample

So to actually explore the difference between weighted and unweighted percentiles, I wanted to simulate a complex survey with unequal probabilities of observations being in the sample, and a skewed variable (income a good example) that was also related, directly or indirectly, to the probability of an observation being in the sample. I did this by creating a educatin variable with 7 different levels, and said 
- the higher the education the higher the mean income
- the higher the education, the lower the chance of being sampled (or, which is effectively the same for my purpose, the lower the chance of completing a response to the survey)

Once I have drawn the sample I am going to use standard post-stratification methods to calculate weights that mean the points in the sample between them represent the original population fully.

Whether or not this sampling or non-response effect is realistic doesn't matter much. Obviously in reality things will be much more complicated. I just wanted something where survey weights were going to vary in a known way (that could be inferred from the data) that correlates with the variable of interest.

Actual income is simulated as coming from a log-normal distribution.

*Post continues after R code*
{% highlight R lineanchors %}
#----------------define population-----------------
n <- 1e4
N <- 1e7
set.seed(123)
population <- tibble(educ = sample(1:7, N, replace = TRUE)) |>
  mutate(income = exp(rnorm(N, 9 + educ / 5, 1)),
         prob_sample = (10 - educ) * n / N)

# marginal totals we will use later for weighting
marginal_total <- population |>
  count(educ, name = "Freq")

# check the income is indeed higher for people in higher education groups:
population |>
  group_by(educ) |>
  summarise(mean(income))
{% endhighlight %}

This is what the mean income looks like for my different education levels:

```
   educ `mean(income)`
  <int>          <dbl>
1     1         16307.
2     2         19982.
3     3         24369.
4     4         29761.
5     5         36302.
6     6         44355.
7     7         54212.
```
OK, now I need to draw a sample. I originally did this with `dplyr::slice_sample()` but found it slow. The `strata()` function in Yves Tillé and Alina Matei's `sampling` package is about twice as fast (benchmarking later in this post) so I used that instead. The code below draws that sample, does the post-stratification weighting (by simply joining the sample to the marginal totals data frame calculated earlier and creating weights forced to add up to those marginal totals) and then calculates the percentile ranks (both weighted and unweighted).

BTW, the reason I was particularly attuned to speed in this overall exercise is that David Brady had commented that Stata was very slow in calculating the weighted percentile ranks. But apart from drawing the sample, everything I was doing in R turned out to be very fast; even calculating percentile ranks on 200,000 observations grouped by year (so similar to the actual data in the original paper) only takes a few seconds.

*Post continues after R code*
{% highlight R lineanchors %}
#---------------draw a sample------------------------
# Sampling, with unequal probabilities
# the slow part here is actually the sampling with unequal probabilities. Not
# sure why so slow. See benchmarking later for why we use this strata()
# function.
sample_rows <- sampling::strata(population, size = n, 
                     pik = population$prob_sample, method = "systematic")

sample <-  population[sample_rows$ID_unit, ] |>
  # join with marginal totals and calculate weights necessary
  # so weights add up to the marginal totals
  left_join(marginal_total, by = "educ") |>
  group_by(educ) |>
  mutate(wt = Freq / n()) |>
  ungroup() |>
  # calculate unweighted and weighted percentile ranks
  mutate(uw_perc = percent_rank(income) * 100,
         w_perc = wt_percent_rank(income, weights = wt) * 100,
         difference = w_perc - uw_perc)

# check that the weights add up to the population number
stopifnot(sum(sample$wt) == N)

# visual check that the sum of weights in each category
# of educ matches the population totals (they do):
count(sample, educ, wt = wt, name = "Sum of weights") |>
  left_join(marginal_total, by = "educ")
{% endhighlight %}

## Comparing the weighted and unweighted percentiles

OK, time to do the actual comparisons. Here's a scatter plot of the weighted and unweighted percentiles. Note the very very high correlation - 0.999! - but also the visually clear slight curvature:

<object type="image/svg+xml" data='/img/0250-scatter1.svg' width='90%'><img src='/img/0250-scatter1.png' width='90%'></object>

That curvature is more obvious if we plot the difference between the two estimates:

<object type="image/svg+xml" data='/img/0250-scatter2.svg' width='90%'><img src='/img/0250-scatter2.png' width='90%'></object>

Why do we get this pattern? It's simple enough if you consider three people - at the bottom, the top and the middle of the sample's income distribution. For the person at the bottom, whether we are weighting the observations or not, we know that this person has the lowest observed income. They're in the bottom percentile either way, and the weights of all the people above them simply don't matter. This applies in reverse for the person at the top. So the weighted and unweighted percentiles are identical in these extremes.

For the person in the middle however it does matter. We look at the unweighted sample and say "right, you're bang in the middle, you're the 50th percentile". But then we look at the weights and say "hold on, the half of the sample that has incomes below you has relatively low weights (because, in my simulation, people with lower education were more likely to be in the survey so get lower weights to compensate). So while it looks like you've got higher income than 50% of people, that 50% of the sample actually only represents 45% of the population." So we get a material difference between weighted and unweighted percentile estimates in the middle, but not at the ends, of the distribution.

There's nothing inherent that says the weighted percentiles in the middle will always be lower than unweighted as in my simulation - that's only the case because the lower income people on average had lower weights. With other sampling designs or non-response rates, this could be reversed. But however the weights work out, there will be more discrepancy between the weighted and unweighted percentiles in the middle of the distribution than the ends.

Here's the simple code for drawing those plots.

*Post continues after R code*
{% highlight R lineanchors %}
#-----------visual comparisons-------------

the_title = str_wrap("Comparison between weighted and unweighted percentiles 
                        in a simulated right-skewed variable from a complex 
                        survey.", 80)

# it *looks* like the unweighted and weighted percentils are indistinguishable:
p1 <- ggplot(sample, aes(x = uw_perc, y = w_perc)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "steelblue") +
  labs(title = the_title,
  subtitle = str_squish(glue("Correlation = 
                       {round(cor(sample$uw_perc, sample$w_perc), 3)}.
                       Blue line shows equality.")),
  x = "Unweighted percentile",
  y = "Weighted percentile")
# Note  this line is slightly curved.
print(p1)

# at the higher incomes and lower incomes there is no difference -
# we know you are the richest or the poorest. But for people in the
# middle it makes a real difference:
p2 <- ggplot(sample, aes(x = uw_perc, y = difference)) +
  geom_point(size= 0.11) +
  labs(x = "Unweighted percentile",
       y = "Weighted minus\nunweighted percentile",
       title = the_title)
print(p2)
{% endhighlight %}

A couple of other interesting observations. The unweighted mean of the unweighted percentile is 50; and so is the weighted mean of the weighted percentile. But the unweighted mean of the weighted percentile is low - about 47 - and the weighted mean of the unweighted percentile is high - about 53. This all makes sense based on the above reasoning.

I now believe the reason the mean percentile in the original paper is about 57 is for a similar reason to what we see here. The percentiles were probably calculated on an unweighted basis (as David said, the correlation was 0.99 so for linear modelling purposes it makes very little difference, and because they were using Stata it is much faster for them to use the unweighted one); so the average shown in Table A.1 is a weighted average of the unweighted percentile. I haven't checked this in David's code, I don't think it matters very much at all, but I'm pleased I've at least found what I think is a plausible line of reasoning on this.

Here are some summary stats on the sample:

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> educ </th>
   <th style="text-align:right;"> mean(wt) </th>
   <th style="text-align:right;"> mean(income) </th>
   <th style="text-align:right;"> mean(uw_perc) </th>
   <th style="text-align:right;"> mean(w_perc) </th>
   <th style="text-align:right;"> n() </th>
   <th style="text-align:right;"> sum(wt) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 690.4403 </td>
   <td style="text-align:right;"> 16980.99 </td>
   <td style="text-align:right;"> 37.66090 </td>
   <td style="text-align:right;"> 34.70156 </td>
   <td style="text-align:right;"> 2067 </td>
   <td style="text-align:right;"> 1427140 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 762.0085 </td>
   <td style="text-align:right;"> 21147.84 </td>
   <td style="text-align:right;"> 42.89254 </td>
   <td style="text-align:right;"> 39.78324 </td>
   <td style="text-align:right;"> 1873 </td>
   <td style="text-align:right;"> 1427242 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 842.4838 </td>
   <td style="text-align:right;"> 25146.92 </td>
   <td style="text-align:right;"> 48.18424 </td>
   <td style="text-align:right;"> 45.01756 </td>
   <td style="text-align:right;"> 1697 </td>
   <td style="text-align:right;"> 1429695 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 989.3329 </td>
   <td style="text-align:right;"> 31281.51 </td>
   <td style="text-align:right;"> 54.45006 </td>
   <td style="text-align:right;"> 51.25665 </td>
   <td style="text-align:right;"> 1445 </td>
   <td style="text-align:right;"> 1429586 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1175.7494 </td>
   <td style="text-align:right;"> 35873.40 </td>
   <td style="text-align:right;"> 58.81986 </td>
   <td style="text-align:right;"> 55.67827 </td>
   <td style="text-align:right;"> 1217 </td>
   <td style="text-align:right;"> 1430887 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1500.4732 </td>
   <td style="text-align:right;"> 42648.17 </td>
   <td style="text-align:right;"> 62.77626 </td>
   <td style="text-align:right;"> 59.67482 </td>
   <td style="text-align:right;"> 951 </td>
   <td style="text-align:right;"> 1426950 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 1904.6667 </td>
   <td style="text-align:right;"> 50135.01 </td>
   <td style="text-align:right;"> 66.77896 </td>
   <td style="text-align:right;"> 63.82556 </td>
   <td style="text-align:right;"> 750 </td>
   <td style="text-align:right;"> 1428500 </td>
  </tr>
</tbody>
</table>

And the code to make those summary stats and compare the different means of the different percentiles.

*Post continues after R code*
{% highlight R lineanchors %}
# check that the average weights are higher for the higher
# education groups, as we wanted it to be
sample |>
  group_by(educ) |>
  summarise(mean(wt),
            mean(income),
            mean(uw_perc),
            mean(w_perc),
            n(),
            sum(wt)) |>
  kable() |>
  kable_styling()

# unweighted mean of the unweighted percentile is 50:
mean(sample$uw_perc)
# but unweighted mean of the weighted percentile isn't!
mean(sample$w_perc)

# Weighted mean of the weighted percentile is 50:
weighted.mean(sample$w_perc, w = sample$wt)
# but weighted mean of the unweighted percentile isn't!
weighted.mean(sample$uw_perc, w = sample$wt)
{% endhighlight %}

## Some speed benchmarking

Finally, here are some speed tests that were behind some of the design choices above. As it turns out, none of it matters very much. As mentioned above, R can calculate the weighted percentiles for hundreds of thousands of observations grouped by year in only a couple of seconds, so I didn't need to worry about that. But in case it matters, here's my core conclusions from running the benchmarking below

- `reldist::wtd.quantile` is about twice as fast as `Desctools::Quantile` in calculating weighted quantiles and gives near-identical results.
- setting `labels = FALSE` in a call to `cut()` makes the function about four times as fast
- `dplyr::slice_sample` takes about the same time to do a sample with unequal probability of selection as a home made function using `base::sample`, but `sampling:strata` function is about twice as fast.
- my final homemade function `wt_percent_rank` for weighted percentile ranks is about four times slower than the unweighted `dplyr::percent_rank`, but is still plenty fast enough (<5 seconds for 200,000 observations).

Benchmarking code:

{% highlight R lineanchors %}
#===================speed tests==================

# Some tests done here behind some of the decisions of what is actually used in
# the script above.

#---------------calculating the breaks at various points in quantile---------
set.seed(321)
n <- 100000
x <- exp(rnorm(n))
w <- exp(rnorm(n)) 
p <- 0:1000 / 1000 # percentiles to use in benchmarking qunatile functions


# note modi::weighted.quantile(x, w = w, prob = 0.5) only works with a scalar value of prob
microbenchmark(
  unweighted = quantile(x, probs = p),
  Desctools = DescTools::Quantile(x, weights = w, probs = p),
  reldist = reldist::wtd.quantile(x, q = p, weight = w)
)
# reldist is notably faster than DescTools in fact it is nearly as fast as
# the unweighted base r calculation

# the results are a tiny bit different but usually not:
tibble(
  dt = DescTools::Quantile(x, weights = w, probs = p),
  rd = reldist::wtd.quantile(x, q = p, weight = w)
) |>
  mutate(diff = rd - dt) |>
  arrange(desc(abs(diff)))

#-------------------actually assigning the percentiles to the original data-----------
breaks <- wtd.quantile(x, q = p, weight = w)

microbenchmark(
  cut(x, breaks = breaks),
  cut(x, breaks = breaks, labels = FALSE)
)
# much faster (about 4x) with labels = FALSE


#-------------------sampling with unequal probabilities---------
# sampling a bunch of rows at random with unequal probabilities using
# slice_sample is surprisinglyu slow. Is there a better way?
# try my home made sampler, and also one from the sampling package

eg_d <- tibble(
  x = x, 
  w = w
)

my_sampler <- function(d, n, weight_by){
  id <- 1:nrow(d)
  rows <- sample(id, size = n, prob = weight_by)
  return(d[rows, ])
}


this_n <- 1000
microbenchmark(
  unweighted        = slice_sample(eg_d, n = this_n),
  `weighted dplyr`    = slice_sample(eg_d, n = this_n, weight_by = w),
  `weighted homemade` = my_sampler(eg_d, n = this_n, weight_by = eg_d$w),
  `sampling package` = eg_d[strata(eg_d, 
                          size = this_n, 
                          pik = eg_d$w, 
                          method = "systematic")$ID_unit, ]
)
# times for weighted dplyr and home made funcionts are very similar 
# for this_n of 100, 1000, 10000
# the sampling::strata() function is about twice as fast
# unweighted sampling is much much faster

#---------comparing speed of unweighted and weighted percentile calculation------
microbenchmark(
  wt_percent_rank(x = x, weights = w, probs = p),
  percent_rank(x = x)
)
# Note that the weighted version is 3-4 times slower even after
# not using labels for cut and using the quickest quantile
# calculation we have.


#----------percentile rank for large sample-----------
# simulate similar size data to that which is in
# https://www.demographic-research.org/volumes/vol48/26/48-26.pdf
psid_eg <- tibble(
  x = exp(rnorm(200000, 8, 1)),
  w = exp(rnorm(200000)),
  year = sample(1:10, size = 200000, replace = TRUE))
)

system.time({
  psid_eg |>
    group_by(year) |>
    mutate(wpr = wt_percent_rank(x, weights = w))
})
# < 5 seconds
{% endhighlight %}

That's all, cheerio.