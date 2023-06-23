
library(tidyverse)
library(DescTools)
library(microbenchmark)
library(modi)
library(reldist)
library(sampling)

n <- 100000

x <- exp(rnorm(n))
w <- exp(rnorm(n))
p <- 0:1000 / 1000


# note modi::weighted.quantile(x, w = w, prob = 0.5) only works with a scalar value of prob
microbenchmark(
  quantile(x, probs = p),
  DescTools::Quantile(x, weights = w, probs = p),
  reldist::wtd.quantile(x, q = p, weight = w)
)
# reldist is notably faster than DescTools

# the results are a tiny bit different but actually only materially at the top:
tibble(
  dt = DescTools::Quantile(x, weights = w, probs = p),
  rd = reldist::wtd.quantile(x, q = p, weight = w)
) |>
  mutate(diff = rd - dt) |>
  arrange(desc(abs(diff)))


breaks <- wtd.quantile(x, q = p, weight = w)

microbenchmark(
  cut(x, breaks = breaks),
  cut(x, breaks = breaks, labels = FALSE)
)
# much faster with labels = FALSE


eg_d <- tibble(
  x = x, 
  w = w
)

# sampling a bunch of rows at random with unequal probabilities
# is surprisinglyu slow. Is there a better way?
my_sampler <- function(d, n, weight_by){
  id <- 1:nrow(d)
  rows <- sample(id, size = n, prob = weight_by)
  return(d[rows, ])
}


this_n <- 1000
microbenchmark(
  unweighted        = slice_sample(eg_d, n = this_n),
  weighted_dplyr    = slice_sample(eg_d, n = this_n, weight_by = w),
  weighted_homemade = my_sampler(eg_d, n = this_n, weight_by = eg_d$w),
  sampling <- strata(eg_d, size = this_n, pik = eg_d$w, method = "systematic")
  )
# times for weighted are very similar for this_n of 100, 1000, 10000
# unweighted sampling is much much faster

wt_percent_rank <- function(x, 
                            weights, 
                            probs = (0:1000) / 1000, 
                            as_percentile = TRUE, ...){
  breaks <- reldist::wtd.quantile(x, w = weights, q = probs)
  breaks <- unique(breaks)
  breaks[1] <- -Inf
  y <- cut(x, breaks = breaks, labels = FALSE)
  if(as_percentile){
    y <- as.numeric(y) - 1
    y <- y / max(y) * 100
  } 
  return(y)
}

microbenchmark(
  wt_percent_rank(x = x, weights = w, probs = p),
  percent_rank(x = x)
)
# the weighted version is 3-4 times slower

n <- 1e4
N <- 1e7
set.seed(123)
population <- tibble(educ = sample(1:7, N, replace = TRUE)) |>
  mutate(income = exp(rnorm(N, 9 + educ / 5, 1)),
         prob_sample = (10 - educ) * n / N)

marginal_total <- population |>
  count(educ, name = "Freq")

population |>
  group_by(educ) |>
  summarise(mean(income))
# 
# ggplot(population, aes(x = income, colour = as.factor(educ))) +
#   geom_density() +
#   scale_x_log10(label = dollar)
# 
# ggplot(population, aes(x = income)) +
#   geom_density() +
#   scale_x_log10(label = dollar)

# the slow part here is actually the slice_sample. Not sure why
# so slow.
sample_raw <- slice_sample(population, n = n, weight_by = prob_sample) 

sample <-  sample_raw |>
  left_join(marginal_total, by = "educ") |>
  group_by(educ) |>
  mutate(wt = Freq / n()) |>
  ungroup() |>
  mutate(uw_perc = percent_rank(income) * 100,
         w_perc = wt_percent_rank(income, weights = wt),
         difference = w_perc - uw_perc)

stopifnot(sum(sample$wt) == N)

# it looks like they're indistinguishable:
ggplot(sample, aes(x = uw_perc, y = w_perc)) +
  geom_point()

# And the correlation is practically 1:
cor(sample[, c("uw_perc", "w_perc")])
# but this line is very slightly curved!



# at the higher incomes and lower incomes there is no difference -
# we know you are the richest or the poorest. But for people in the
# middle it makes a real difference:
ggplot(sample, aes(x = uw_perc, y = difference)) +
  geom_point(size= 0.11) +
  labs(x = "Unweighted percentile",
       y = "Weighted minus unweighted percentile")

sample |>
  group_by(educ) |>
  summarise(mean(wt),
            mean(income),
            mean(uw_perc),
            mean(w_perc),
            n(),
            sum(wt)) 

# unweighted mean of the unweighted percentile is 50:
mean(sample$uw_perc)
mean(sample$w_perc)

# But weighted mean of the weighted percentile is 50:
weighted.mean(sample$uw_perc, w = sample$wt)
weighted.mean(sample$w_perc, w = sample$wt)

tibble(unweighted_mean = mean(sample$income), 
       unweighted_se = sd(sample$income) / sqrt(nrow(sample)),
       weighted_mean = weighted.mean(sample$income, w = sample$wt),
       weighted_uw_perc = weighted.mean(sample$uw_perc, w = sample$wt),
       weighted_w_perc = weighted.mean(sample$w_perc, w = sample$wt),
       population = mean(population$income)) |>
  t()

