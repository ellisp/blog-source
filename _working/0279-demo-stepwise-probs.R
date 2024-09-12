library(MASS)
library(tidyverse)
library(glue)

set.seed(123)
 
#' @param xcv covariances of the X variables with eachother
#' @param xvm variance of the x variables, expressed as a multiplier of xcv
#' @param yvm variance of the y variable, expressed as a multiplier of xcv
sim_steps <- function(xcv = 1, xvm = 4, yvm = 200, n = 200, k = 15, runs = 50){
  results <- tibble()
  
  true_coef <- c(rep(1, 7), rep(0.2, 3), rep(0, 5))
  
  
  for(i in 1:runs){
    Sigma <- matrix(xcv, nrow = k, ncol = k)
    diag(Sigma) <- xcv * xvm
    
    m <- mvrnorm(n = n, mu = rep(0, k), Sigma = Sigma) 
    
    d <- m |>
      as.data.frame() |>
      mutate(y = m %*% true_coef + rnorm(n(), 0, sqrt(yvm) * xcv))
    
    mod <- lm(y ~ ., data = d)
    #summary(mod)       
    
    step_mod <- stepAIC(mod, trace = FALSE)
    summary(step_mod)
  
    cm <- coef(mod)[-1]  
    csm <- coef(step_mod)[-1]
    
    results <- rbind(results,
                     tibble(
                        variable = names(csm),
                        coefficient = csm,
                        run = i,
                        model = "Stepwise"
                     ),
                     tibble(
                       variable = names(cm),
                       coefficient = cm,
                       run = i,
                       model = "Full model"
                     )
                     
                     
                     )
    
    
  }
  true_coef_df <- tibble(
    variable = factor(names(d)[1:k], levels = names(d)[1:k]),
    coefficient = true_coef
  )
  
  p1 <- results |>
    filter(model == "Stepwise") |>
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
  Variables' coefficients are most often either zero (ie dropped by the stepwise procedure), or too large.
  Also, real explanatory variables are often dropped. Fake ones are often included.",
         caption = glue("Simulated data with a model that explains about {percent(summary(step_mod)$r.squared)} of variation in response variable, with mild multicollinearity, by https://freerangestats.info"))
  
  
  
  the_vif <- car::vif(mod)
  
  p2 <- results |>
    filter(model != "Stepwise") |>
    complete(run, variable, fill = list(coefficient = 0)) |>
    mutate(variable = factor(variable, levels = true_coef_df$variable)) |>
    count(variable, coefficient) |>
    ggplot(aes(x = coefficient, y = variable)) +
    geom_point() +
    geom_point(data = true_coef_df, colour = "red", size = 4, shape = 12) +
    labs(x = "Coefficient value",
         y = "Variable",
         title = "Using the full model returns unbiased coefficient estimates",
         subtitle = "Black dots show coefficient estimates from one run of a all-variables-in model fitting. Red squares show correct values.",
         caption = glue("Simulated data with a model that explains about {percent(summary(mod)$r.squared)} of variation in response variable, with mild multicollinearity, by https://freerangestats.info"))
  
  return(list(
    results = results,
    p1 = p1,
    p2 = p2,
    the_vif = the_vif
  ))
}

my_sim <- sim_steps()


sim_steps(yvm = 10)$p1
sim_steps(yvm = 5000)$p1
sim_steps(xvm = 1.5)$p1
sim_steps(xvm = 1.5, yvm = 10)$p1


svg_png(my_sim$p1, "../img/0279-stepwise-results", w = 10, h = 8)
svg_png(my_sim$p2, "../img/0279-full-results", w = 10, h = 8)

