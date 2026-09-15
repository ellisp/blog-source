---
layout: post
title: Sample size needed for central limit theory to kick in
date: 2026-09-16
tag: 
   - Simulations
   - Distributions
description: It takes a larger sample size than you might hope for the mean of a sample to have a distribution that's close enough to 'normal' for common inference methods to work.
image: /img/0332-ad-stat.svg
socialimage: https:/freerangestats.info/img/0332-ad-stat.png
category: R
---




<object type="image/svg+xml" data='/img/0332-ad-stat.svg' width='100%'><img src='/img/0332-ad-stat.png' width='100%'></object>


<object type="image/svg+xml" data='/img/0332-coverage.svg' width='100%'><img src='/img/0332-coverage.png' width='100%'></object>


<object type="image/svg+xml" data='/img/0332-coverage-trunc.svg' width='100%'><img src='/img/0332-coverage-trunc.png' width='100%'></object>



{% highlight R lineanchors %}
library(nortest)
library(tidyverse)
library(actuar)
library(glue)
library(scales)

set.seed(123)

#--------------Function to do the simulations---------------

reps <- 10000
sim_clt <- function(x, n = 30, reps = reps, replace = TRUE, plot = TRUE, conf = 0.95){
  true_mean <- mean(x)
  
  samples <- replicate(reps, sample(x = x, size = n, replace = replace), simplify = FALSE)
  means <- sapply(samples, mean)
  
  covered <- sapply(samples, function(s) {
    ci <- t.test(s, conf.level = conf)$conf.int
    ci[1] <= true_mean & true_mean <= ci[2]
  })
  
  if (plot){
    qqnorm(means)
    grid()
  }
  
  return(list(
    ad_stat  = nortest::ad.test(means)$statistic,
    coverage = mean(covered)
  ))
}

#--------------define distributions and run simulations---------------

N <- 1e6
ns <- c(5:30, 50, 100, 300, 1000, 2000, 5000, 10000, 20000, 40000)
sims <- list()

pop_dists <- list(
  "exp(rnorm(N))"                            = function() exp(rnorm(N)),
  "exp(rnorm(N, sd = 2))"                    = function() exp(rnorm(N, sd = 2)),
  "rnorm(N)"                                 = function() rnorm(N),
  "rcauchy(N)"                               = function() rcauchy(N),
  "rpareto(N, shape = 2.1, scale = 1)"       = function() rpareto(N, shape = 2.1, scale = 1),
  "rexp(N)"                                  = function() rexp(N),
  "c(rexp(N*0.6), exp(rnorm(N*0.4)))"        = function() c(rexp(N*0.6), exp(rnorm(N*0.4))),
  "c(rnorm(N*0.9,0,1), rnorm(N*0.1,3,3))"    = function() c(rnorm(N*0.9,0,1), rnorm(N*0.1,3,3))
)


all_sims <- lapply(names(pop_dists), function(lbl) {
  pop <- pop_dists[[lbl]]()
  
  results <- lapply(ns, function(n) sim_clt(pop, n = n, plot = FALSE))
  
  tibble(
    n = ns,
    ad_stat  = sapply(results, `[[`, "ad_stat"),
    coverage = sapply(results, `[[`, "coverage"),
    pop_dist = lbl
  )
}) |> bind_rows()

#-----------------------draw charts--------------

normal_band <- all_sims |> 
  filter(pop_dist == "rnorm(N)") |> 
  pull(ad_stat) |> 
  quantile(probs = c(0.025, 0.975))

p1 <- all_sims |> 
  mutate(pop_dist = fct_reorder(pop_dist, ad_stat)) |> 
  ggplot(aes(x = n, y = ad_stat))+
  facet_wrap(~pop_dist, nrow = 2)+
  annotate("rect", xmin = min(ns), xmax = Inf, ymin = normal_band[1], ymax = normal_band[2], alpha = 0.5, fill = "orange")+
  geom_point()+
  scale_x_log10(label = comma, breaks = ns[ns >=30 | ns %in% c(5, 10, 20)]) +
  scale_y_log10(label = comma)+
  theme(panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "plain", size = 9)) +
  labs(x = "Sample size",
       y = "Anderson-Darling statistic",
       subtitle = glue("Points represent average from {comma(reps)} simulations of given sample size. Shaded area covers 95% of values from a normal distribution."),
       title = "Increasing sample size and growing effectiveness of the central limit theorem - Anderson-Darling statistic")

print(p1)


p2 <- all_sims |> 
  mutate(pop_dist = fct_reorder(pop_dist, ad_stat)) |> 
  ggplot(aes(x = n, y = coverage)) +
  facet_wrap(~pop_dist, nrow = 2) +
  annotate("rect", xmin = min(ns), xmax = Inf, ymin = 0.95, ymax = 1, alpha = 0.5, fill = "orange")+
  geom_point() +
  scale_x_log10(label = comma, breaks = ns[ns >=30 | ns %in% c(5, 10, 20)]) +
  scale_y_continuous(label = percent) +
  theme(panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "plain", size = 9)) +
  labs(x = "Sample size",
       y = "Coverage of a 95% confidence interval based on t distribution",
       subtitle = glue("Points represent average from {comma(reps)} simulations of given sample size. Shaded area shows 95% and higher, as desired."),
       title = "Increasing sample size and growing effectiveness of the central limit theorem - confidence interval coverage")

print(p2)

p3 <- p2 +
  labs(y = "Coverage of a 95% confidence interval based on t distribution\nY axis truncated to only start at 80%; some points excluded because of that.") +
  coord_cartesian(ylim = c(0.8, 1))

print(p3)



{% endhighlight %}


