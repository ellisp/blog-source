library(MASS)
library(tidyverse)
library(glue)

 
#' @param xcm correlations of the X variables with eachother, as a multiplier of
#'   their standard deviation (all X variables have the same variance / sd of 1)
#' @param ysdm standard deviation of the y variable, expressed as a multiplier of variance
#'   of the X
sim_steps <- function(xcm = 0.4, ysdm = 9, n = 200, k = 15, runs = 50, seed = 321){
  
  set.seed(seed)
  
  results <- tibble()
  
  true_coef <- c(rep(1, 7), rep(0.2, 3), rep(0, 5))
  
  
  for(i in 1:runs){
    Sigma <- matrix(xcm, nrow = k, ncol = k)
    diag(Sigma) <- 1
    
    # Sigma, not sigma squared
    m <- mvrnorm(n = n, mu = rep(0, k), Sigma = Sigma) 
    
    d <- m |>
      as.data.frame() |>
      mutate(y = m %*% true_coef + rnorm(n(), 0, ysdm))
    
    mod <- lm(y ~ ., data = d)
    
    step_mod <- stepAIC(mod, trace = FALSE)
    
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
  
  results <- results |>
    complete(run, variable, model, fill = list(coefficient = 0)) |>
    mutate(variable = factor(variable, levels = true_coef_df$variable)) 
  
  biases_df <- results |>
    filter(coefficient != 0) |>
    left_join(true_coef_df, by = "variable") |>
    group_by(model) |>
    summarise(bias = round(mean(coefficient.x - coefficient.y), 2)) |>
    mutate(r2 = c(summary(mod)$r.squared, summary(step_mod)$r.squared),
           xcm = xcm,
           ysdm = ysdm,
           n = n)
  
  biases <- pull(biases_df, bias)
  names(biases) <- pull(biases_df, model)
  
  mclabel <- case_when(
    abs(xcm) < 0.05 ~ "negligible",
    abs(xcm) < 0.19 ~ "mild",
    abs(xcm) < 0.39 ~ "medium",
    TRUE            ~ "strong"
  ) |>
    paste("multicollinearity")
  
  p1 <- results |>
    filter(model == "Stepwise") |>
    count(variable, coefficient) |>
    ggplot(aes(x = coefficient, y = variable)) +
    geom_point(aes(size = n)) +
    geom_point(data = true_coef_df, colour = "red", size = 4, shape = 12) +
    labs(x = "Coefficient value",
         y = "Variable",
         size = "Number of observations:\n(usually only one, except when coefficient dropped altogether)",
         title = "Stepwise regression returns coefficient estimates biased away from zero",
         subtitle = glue("Black dots show coefficient estimates from one run of a stepwise (AIC-based) model fitting. Red squares show correct values.
Coefficients of variables left in the model are on average {biases['Stepwise']} too large (compared to real value of 0, 0.1 or 1).
Also, real explanatory variables are often dropped. Fake ones are often included."),
         caption = glue("Simulated data with a model that explains about {percent(summary(step_mod)$r.squared)} of variation in response variable, with {mclabel}, by https://freerangestats.info"))
  
  
  
  p2 <- results |>
    filter(model != "Stepwise") |>
    count(variable, coefficient) |>
    ggplot(aes(x = coefficient, y = variable)) +
    geom_point() +
    geom_point(data = true_coef_df, colour = "red", size = 4, shape = 12) +
    labs(x = "Coefficient value",
         y = "Variable",
         title = "Using the full model returns unbiased coefficient estimates",
         subtitle = glue("Black dots show coefficient estimates from one run of a all-variables-in model fitting. Red squares show correct values.
Coefficients of variables left in the model are on average {biases['Full model']} too large (compared to real value of 0, 0.1 or 1)."),
         caption = glue("Simulated data with a model that explains about {percent(summary(mod)$r.squared)} of variation in response variable, with mild multicollinearity, by https://freerangestats.info"))
  
  return(list(
    results = results,
    p1 = p1,
    p2 = p2,
    biases_df = biases_df
  ))
}

my_sim <- sim_steps()
my_sim$p1
my_sim$p2

# If the model is a great fit it's not such a problem:
sim_steps(ysdm = 1)$p1
sim_steps(ysdm = 1)$p2

# if sample size is large it's not a problem
sim_steps(n = 1e4)$p1

# if the model leaves a lot of unexplained variance it's bigger
sim_steps(ysdm = 100)$p1

# if there's no multicollinearity it's a bit smaller
# version below has no correlations in the X, and smaller than default
# variation in the Y so the model is still got an R2 of about 0.2
sim_steps(xcm = 0, ysdm = 6)$p1

# if there's high multicollinearity it's worse
sim_steps(xcm = 0.7, ysdm = 12)$p1




var_params <- expand_grid(xcm = 0:9 / 10, ysdm = 1:9 * 3, n = c(200, 2000))

many_params <- tibble()

for(i in 1:nrow(var_params)){
  cat(i)
  res <- sim_steps(xcm = var_params[i, ]$xcm, 
                   ysdm = var_params[i, ]$ysdm,
                   n = var_params[i, ]$n,
                   runs = 200)
  many_params <- rbind(many_params, res$biases_df)
}

many_params |>
  mutate(n = glue("n = {n}")) |>
  ggplot(aes(x = r2, y = bias, colour = model)) +
  facet_grid(n ~ as.ordered(xcm)) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.8) +
  geom_point() +
  scale_x_continuous(breaks = c(0.2, 0.8)) +
  labs(x = "R-squared (proportion of Y's variance explained by model)",
       y = "Average bias of estimated variable coefficients",
       title = "Bias in regression coefficients after stepwise selection of variables",
       subtitle = "Bias is worst with small samples, models with low R-squared, and multicollinearity in the explanatory variables (shown from 0.1 to 0.9).
Model with 15 explanatory 'X' variables. Correct values of coefficients are 0, 0.1 or 1;  so a bias of +1 is very serious.",
colour = "Model fitting procedure")

many_params |>
  mutate(n = glue("n = {n}")) |>
  ggplot(aes(x = xcm, y = ysdm, fill = bias)) +
  facet_grid(model~n) +
  geom_tile() +
  scale_fill_gradientn(colours = c("white", "steelblue", "darkred")) +
  labs(x = "Correlation between the X variables",
       y = "Standard deviation of the Y variable",
       fill = "Average bias of estimated variable coefficients:",
       title = "Bias in regression coefficients after stepwise selection of variables",
       subtitle = "Bias is worst with small samples, high variance response, and multicollinearity in the explanatory variables.
Model with 15 explanatory 'X' variables. Correct values of coefficients are 0, 0.1 or 1;  so a bias of +1 is very serious.")

# save(many_params, file = "many_params.rda")

svg_png(my_sim$p1, "../img/0279-stepwise-results", w = 10, h = 8)
svg_png(my_sim$p2, "../img/0279-full-results", w = 10, h = 8)

