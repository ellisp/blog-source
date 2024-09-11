library(MASS)
library(tidyverse)

?mvrnorm

set.seed(123)
 
n <- 200
k <- 15
runs <- 100

results <- tibble()

true_coef <- c(rep(1, 7), rep(0.2, 3), rep(0, 5))


for(i in 1:runs){
  Sigma <- matrix(2, nrow = k, ncol = k)
  diag(Sigma) <- 4
  
  m <- mvrnorm(n = n, mu = rep(0, k), Sigma = Sigma) 
  
  d <- m |>
    as.data.frame() |>
    mutate(y = m %*% true_coef + rnorm(n(), 0, 16))
  
  mod <- lm(y ~ ., data = d)
  #summary(mod)       
  
  step_mod <- stepAIC(mod, trace = FALSE)
  summary(step_mod)
  
  csm <- coef(step_mod)[-1]
  
  results <- rbind(results,
                   tibble(
                      variable = names(csm),
                      coefficient = csm,
                      run = i
                   ))
  
  
}
true_coef_df <- tibble(
  variable = factor(names(d)[1:k], levels = names(d)[1:k]),
  coefficient = true_coef
)

results |>
  complete(run, variable, fill = list(coefficient = 0)) |>
  mutate(variable = factor(variable, levels = true_coef_df$variable)) |>
  count(variable, coefficient) |>
  ggplot(aes(x = coefficient, y = variable)) +
  geom_point(aes(size = n)) +
  geom_point(data = true_coef_df, colour = "red", size = 4, shape = 12) +
  labs(x = "Coefficient value",
       y = "Variable",
       title = "Stepwise regression returns coefficient estimates biased away from zero",
       subtitle = "Black dots show coefficient estimates from one run of a stepwise (AIC-based) model fitting. Red squares show correct values.
Variables' coefficients are nearly always either zero (ie dropped by the stepwise procedure), or too large.
Also, real explanatory variables are often dropped. Fake ones are often included.",
       caption = "Simulated data with a model that explains about 30% of variation in response variable, by https://freerangestats.info")
