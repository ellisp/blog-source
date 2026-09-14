library(nortest)
library(tidyverse)

set.seed(123)

sim_clt <- function(x, n = 30, reps = 5000, replace = TRUE, plot = TRUE){
  means <- replicate(reps, mean(sample(x = x, size = n, replace = replace)))
  if (plot){
    qqnorm(means)
    grid()
  }
  return(nortest::ad.test(means)$statistic)
}


N <- 1e6
ns <- c(5:30, 50, 100, 300, 1000, 2000, 5000, 10000)
sims <- list()

pop_dists <- list(
  "exp(rnorm(N))"                   = function() exp(rnorm(N)),
  "rnorm(N)"                        = function() rnorm(N),
  "rcauchy(N)"                      = function() rcauchy(N),
  "rexp(N)"                         = function() rexp(N),
  "c(rexp(N), exp(rnorm(N)))"       = function() c(rexp(N), exp(rnorm(N))),
  "c(rnorm(N,0,1), rnorm(N,0.5,2))" = function() c(rnorm(N, 0, 1), rnorm(N, 0.5, 2))
)


all_sims <- lapply(names(pop_dists), function(lbl) {
  pop <- pop_dists[[lbl]]()
  tibble(
    n = ns,
    ad_pval = sapply(ns, function(n) sim_clt(pop, n = n, plot = FALSE)),
    pop_dist = lbl
  )
}) |> bind_rows()


all_sims |> 
  ggplot(aes(x = n, y = ad_pval))+
  facet_wrap(~pop_dist)+
  geom_hline(yintercept = 0.05, colour = "steelblue") +
  geom_point()+
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma)+
  theme(panel.grid.minor.y = element_blank())

# thoughts - test coverage of a t distribution confidence interval rather than the AD statistic (or as well as)