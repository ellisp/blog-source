
library(boot)
library(tidyverse)

n <- 100
b0 <- 0.1
b1 <- 0.1


simulator <- function(n = 100, b0 = 0, b1 = 0){
  x <- rnorm(n, 0, 1)
  lp <- b0 +b1 * x
  y <- rbinom(n, 1, prob = inv.logit(lp))
  
  mod1 <- glm(y ~ x, family = binomial(link = "logit"))
  mod2 <- glm(y ~ x, family = binomial(link = "probit"))
  
  
  results <- tibble(
    b1 = b1,
    n = n,
    b0 = b0,
    mod1 = sign(coef(mod1)[2]) == sign(b1),
    mod2 = sign(coef(mod2)[2]) == sign(b1)
  )
  return(results)
}

set.seed(123)
nsim = 10000
b0 = rnorm(nsim)
b1 = rnorm(nsim)
n <- MASS::rnegbin(nsim, mu = 100, theta = 5)
full_results_l <- list()
for(i in 1:nsim){
  full_results_l[[i]] <- simulator(n = n[i],
                                 b0 = b0[i],
                                 b1 = b1[i])
}

full_results <- do.call(rbind, full_results_l)
summary(full_results)

full_results %>% filter(mod1 != mod2)
