library(tidyverse)
library(glue)
library(patchwork)

#'
post_alt <- function(p = 0.05, power = 0.8, prior_null){
  prior_alt <- 1 - prior_null
  
  post_null <- p * prior_null / (p * prior_null + power * prior_alt)
  
  return(1 - post_null)
}

rep_rate <- function(rep_power = 0.8, rep_alpha = 0.05, orig_p = 0.05, orig_power = 0.8, prior_null){
  pa <- post_alt(p = orig_p, power = orig_power, prior_null = prior_null)
  
  repeat_false_positive <- rep_alpha * orig_p * (1 - pa)
  repeat_true_positive <- rep_power * pa
  
  return(repeat_false_positive + repeat_true_positive)

  }

post_alt(prior_null = 0.5)
rep_rate(prior_null = 0.9, rep_power = 0.9, orig_p = 0.01)
rep_rate(prior_null = 0.9, rep_power = 0.8, orig_p = 0.05)


scenarios <- function(pn         = 0.90, 
                      op         = 0.05, 
                      rp         = 0.80, 
                      orig_power = 0.80, 
                      rep_alpha  = 0.05,
                      ncol       = 2){
  p1 <- tibble(prior_null = pn,
         orig_p = op,
         rep_power = seq(from = 0.8,to = 0.95, length.out = 20)) |> 
    mutate(rate = rep_rate(prior_null = prior_null,
                           orig_p = orig_p,
                           rep_power = rep_power,
                           rep_alpha = rep_alpha,
                           orig_power = orig_power)) |> 
    ggplot(aes(x = rep_power, y = rate)) +
    geom_line() +
    labs(y = "Replication rate",
         x = "Power of the replication experiment",
         subtitle = glue("A. Prior probability of alt was {1-pn} and\noriginal p-value was {op}"))
  
  p2 <- tibble(prior_null = pn,
         orig_p = seq(from = 0.001, to = 0.5, length.out = 20),
         rep_power = rp) |> 
    mutate(rate = rep_rate(prior_null = prior_null,
                           orig_p = orig_p,
                           rep_power = rep_power,
                           rep_alpha = rep_alpha,
                           orig_power = orig_power)) |> 
    ggplot(aes(x = orig_p, y = rate)) +
    geom_line() +
    labs(y = "Replication rate",
         x = "p-value of the original finding",
         subtitle = glue("B. Prior probability of alt was {1-pn} and\npower of replication experiment was {rp}"))
  
  
  p3 <- tibble(prior_null = seq(from = 0.1, to = 0.9, length.out = 20),
         orig_p = op,
         rep_power = rp) |> 
    mutate(rate = rep_rate(prior_null = prior_null,
                           orig_p = orig_p,
                           rep_power = rep_power,
                           rep_alpha = rep_alpha,
                           orig_power = orig_power)) |> 
    ggplot(aes(x = 1 - prior_null, y = rate)) +
    geom_line() +
    labs(y = "Replication rate",
         x = "Prior probability of the alt hypothesis",
         caption = glue("Original power of {orig_power}, and replication alpha cut-off of {rep_alpha}"),
         subtitle = glue("C. Original p-value was {op} and\npower of replication experiment was {rp}"))

  p4 <- expand_grid(prior_null = seq(from = 0.1, to = 0.9, length.out = 20),
         orig_p = seq(from = 0.001, to = 0.5, length.out = 20),
         rep_power = rp) |> 
    mutate(rate = rep_rate(prior_null = prior_null,
                           orig_p = orig_p,
                           rep_power = rep_power,
                           rep_alpha = rep_alpha,
                           orig_power = orig_power)) |> 
    ggplot(aes(x = orig_p, y = 1 - prior_null, fill = rate)) +
    geom_tile() +
    scale_fill_viridis_c(, limits = c(0, 1)) +
    labs(y = "Prior probability\nof the alt hypothesis",
         x = "Original p-value",
         fill = "Replication rate:",
         subtitle = glue("D. Power of replication experiment was {rp}"))
  
    
  return(p1 + p2 + p3 + p4 + 
           plot_layout(ncol = ncol))
}

scenarios(op = 0.001, orig_power = 0.99, pn = 0.5)
scenarios(op = 0.01, orig_power = 0.9, pn = 0.9)
scenarios(op = 0.01, orig_power = 0.8, pn = 0.999)
