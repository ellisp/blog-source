library(tidyverse)
library(acid)
library(microbenchmark)
library(forcats)
# Peter's Stats Stuff miscellaneous stuff package, install with: 
devtools::install_github("ellisp/pssmisc-r-package/pkg")
library(pssmisc) 



StatsGini <- function(x, w = rep(1, length(x))){
  # x and w are vectors
  # w can be left blank when calling the fn (i.e. no weighting)
  # Examples:
  # x <- c(3, 1, 7, 2, 5)
  # w <- c(1, 2, 3, 4, 5)
  # StatsGini(x, w) should yield 0.2983050847
  # StatsGini(c(0.25, 0.75), c(1, 1)) should yield 0.25
  n <- length(x)
  wxsum <- sum(w * x)
  wsum <- sum(w)
  sxw <- order(x, w) # Ascending order sort
  sx <- w[sxw] * x[sxw]
  sw <- w[sxw]
  pxi <- vector(mode = "numeric", length = n)
  pci <- vector(mode = "numeric", length = n)
  pxi <- cumsum(sx) / wxsum
  pci <- cumsum(sw) / wsum
  G <- 0.0
  for (i in 2:n){
    G <- G - (pci[i] * pxi[i - 1] - pxi[i] * pci[i - 1] )
  }
  return(G)
}


options(digits = 10)
x <- c(3, 1, 7, 2, 5)
w <- c(1, 2, 3, 4, 5)
StatsGini(x, w) # should yield 0.2983050847
StatsGini(c(0.25, 0.75), c(1, 1)) # should yield 0.25

weighted.gini(x, w)
weighted.gini(c(0.25, 0.75), c(1, 1))

#' Generate a realistic mixed distribution of incomes
#'
#' alpha ~ t(d_alpha) scaled to mean, sd of (log(mu), sigma_alpha)
#' beta ~ t(d_beta) scaled to mean, sd of (alpha, sigma_beta)
#' x = exp(beta)
#' y = x * I(-1, 0, 1)
#'
#' @param n sample size to generate
#' @param mu median income
#' @param sigma_alpha standard deviation of underlying latent means
#' @param sigma_beta standard deviation of scaled t distribution underlying the population's log-t distribution
#' @param d_alpha degrees of freedom in th
#' @param d_beta degrees of freedom in the t distribution underlying the log-t
#' @param pr vector of 2 elements, the probability of negative income and of zero income respectively
gen_incomes <- function(n, mu = 30000, 
                        sigma_alpha = 1, sigma_beta = 1, 
                        d_alpha = 15, d_beta = 15,
                        pr = c(0.01, 0.2)){
  alpha <- rt(n, d_alpha) * sigma_alpha + log(mu)
  beta <- exp(rt(n, d_beta) * sigma_beta + alpha)
  y <- beta * sample(c(-1, 0, 1), n, replace = TRUE, prob = c(pr, 1 - sum(pr)))
  return(y)
}

set.seed(123)
N <- 100000
population <- data.frame(income = gen_incomes(N), sigma_alpha = 1, d_alpha = 10, d_beta = 15,
                         sigma_beta = 2,
                         prob = rbeta(N, 2, 2))


svg("../img/0106-density.svg", 7, 4)
population %>%
  slice(1:1000) %>%
  ggplot(aes( x = income)) + 
  scale_x_continuous("Annual income (modulus transform with power of 0.02)",
                     label = dollar, 
                     trans = modulus_trans(0.02),
                     breaks = modulus_breaks(lambda = 0.02)) + 
  geom_density(fill = "grey", alpha = 0.2)  +
  geom_rug() +
  ggtitle("Density of simulated annual incomes",
          "First 1,000 values")
dev.off()

svg("../img/0106-lorenz.svg", 7, 5)
population %>%
  # thin down to just a sample of 10,000 to save on vector graphic file size:
  sample_n(10000) %>%
  arrange(income) %>%
  mutate(cum_prop_inc = cumsum(income) / sum(income),
         seq = 1:n() / n()) %>%
  ggplot(aes(x = seq, y = cum_prop_inc)) +
  geom_line() +
  geom_ribbon(aes(ymax = seq, ymin = cum_prop_inc), fill = "steelblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  labs(x = "Cumulative proportion of population, from poorest to richest",
       y = "Cumulative proportion of income") +
  annotate("text", x = c(0.6, 0.95), y = c(0.4, 0.05), label = c("A", "B"), size = 7) +
  annotate("text", x = 0.5, y = 0.6, label = "Line of equality", angle = 45) +
  annotate("text", x = 0.85, y = 0.15, label = "Actual distribution", angle = 45) +
  coord_equal() +
  ggtitle("Lorenz curve with simulated income data",
          paste("Gini coefficient is", round(gini(population$income)$Gini, 2)))
dev.off()  


small_sample <- population[1:100, ]
weighted.gini(small_sample$income, w = 1 / small_sample$prob)$Gini
StatsGini(small_sample$income, w = 1 / small_sample$prob)

y <- population$income
w <- 1/ population$prob
mb <- microbenchmark(
  weighted.gini(y, w)$Gini,
  StatsGini(y, w),
  times = 1000)
mb

svg("../img/0106-benchmark.svg", 8, 4)
  autoplot(mb, log = FALSE) +
    ggtitle("Time taken to estimated Gini coefficient from 100,000 observations",
            "Microbenchmark results") 
dev.off()


# number of repeats at each sample size:
reps <- 100
# sample sizes, chosen to resemble realistic survey sizes:
ns <- c(30, 100, 500, 1000, 3000, 10000)

# matrix of zeros that we'll replace with the estimates of Gini
res <- matrix(0, nrow = reps, ncol = length(ns))

# obviously this next bit could be made much more efficient by parallel processing,
# but it's only a few minutes of running 
set.seed(666)
for(i in 1:length(ns)){
  n <- ns[i]
  for(j in 1:reps){
    the_sample <- sample_n(population, size = n, replace = FALSE, weight = population$prob)    
    
    res[j, i] <- weighted.gini(the_sample$income, w = 1 / the_sample$prob)$Gini
   }
}

res_df <- as.data.frame(res)
names(res_df) <- format(ns, big.mark = ",")

svg("../img/0106-sample-distributions.svg", 8, 6)
res_df %>%
  gather(sample_size, value) %>%
  mutate(sample_size = fct_reorder(sample_size, -value)) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~sample_size) +
  geom_density(alpha = 0.3, fill = "grey") +
  # a few values of > 1 make no sense and warp the graph:
  coord_cartesian(xlim = c(0.6, 1)) +
  labs(x = "Estimated weighted Gini") +
  ggtitle("Sampling distribution of weighted Gini point estimate from a complex survey",
          paste0("Various sample sizes; true value is ", round(weighted.gini(population$income)$Gini, 2)))

dev.off()

# note might be a bug in dplyr, format doesn't work in the pipeline?

convert_pngs("0106")
