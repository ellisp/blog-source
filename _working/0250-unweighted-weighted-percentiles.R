
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
  
  # convert this into a number from 0 to 100
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

# think hard about how this work, is it how I expect
table(wt_percent_rank(rnorm(1000), weights = rep(1, 1000), q = (0:5) / 5))
table(wt_percent_rank(rnorm(1000), weights = rep(1, 1000), q = (0:10) / 10))
table(wt_percent_rank(rnorm(1000), weights = rep(1, 1000), q = (0:11) / 11))
table(wt_percent_rank(rnorm(1000), weights = rep(1, 1000), q = (0:100) / 100))
table(wt_percent_rank(rnorm(1000), weights = rep(1, 1000), q = (0:101) / 101))

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

svg_png(p1, "../img/0250-scatter1")

# at the higher incomes and lower incomes there is no difference -
# we know you are the richest or the poorest. But for people in the
# middle it makes a real difference:
p2 <- ggplot(sample, aes(x = uw_perc, y = difference)) +
  geom_point(size= 0.11) +
  labs(x = "Unweighted percentile",
       y = "Weighted minus\nunweighted percentile",
       title = the_title)

svg_png(p2, "../img/0250-scatter2")

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
  kable_styling() |>
  writeClipboard()

# unweighted mean of the unweighted percentile is 50:
mean(sample$uw_perc)
# but unweighted mean of the weighted percentile isn't!
mean(sample$w_perc)

# Weighted mean of the weighted percentile is 50:
weighted.mean(sample$w_perc, w = sample$wt)
# but weighted mean of the unweighted percentile isn't!
weighted.mean(sample$uw_perc, w = sample$wt)


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