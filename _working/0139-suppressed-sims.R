
# TODO - the MSE of the coefs is going to be on a different scale each time. Find a better metric.


#------------------ with multiple imputation----------
#' Imputation function for suppressed data for use with mice - Poisson-based
#'
#' @param y vector to be imputed
#' @param ry logical vector of observed (TRUE) and missing (FALSE) values of y
#' @param x design matrix. Ignored but is probably needed for mice to use.
#' @param wy vector that  is TRUE where imputations are created for y. Not sure when this is different
#' to just !ry (which is the default).
mice.impute.desuppress <- function (y, ry, x, wy = NULL, max_value = 5, ...) {
  # during dev:
  # y <- data$censored_count_num; ry <- !is.na(y)
  if (is.null(wy)){ 
    wy <- !ry
  }
  
  # What are the relative chances of getting values from 0 to the maximum allowed value,
  # if we have a Poisson distribution  which we have estimated the mean of via trimmed mean?
  # (this is very approximate but probably better than just giving equal chances to 0:5)
  probs <- dpois(0:max_value, mean(y[ry], tr = 0.2))
  
  return(sample(0:max_value, sum(wy), prob = probs, replace = TRUE))
}

#' Imputation function for suppressed data for use with mice - simple
#'
mice.impute.uniform <- function (y, ry, x, wy = NULL, max_value = 5, ...) {
  return(sample(0:max_value, sum(wy), replace = TRUE))
}

# next function fails
coefs_comp <- function(run){
  set.seed(run)
  true_coefs <- c(2, runif(n - 1, -1, 1))
  data <- data %>%
    mutate(expected = exp(mm %*% true_coefs),
           count = rpois(n, lambda = exp(mm %*% true_coefs)),
           censored_count = ifelse(count < 6, "<6", count),
           censored_count_num = suppressWarnings(as.numeric(censored_count)),
           count_replaced = ifelse(count < 6, 3, count))
  
  data$z <- pmax(6, data$count)
  data$lcensored <- is.na(data$censored_count_num )
  prop_suppressed <- mean(data$lcensored)
  
  if(prop_suppressed > 0.5){
    return(NULL)
  } else {
    
    #--------------straightforward methods-----------
    mod0 <- glm(count ~ animals * region, data = data, family = "poisson")
    mod2 <- glm(count_replaced ~ animals * region, data = data, family = "poisson")
    
    #------------with censored poisson regression-------------------------
    # (this method does so badly that I can only assume I am using it wrongly
    
    mod3 <- vglm(SurvS4(z, lcensored, type = "left") ~ animals * region, family = cens.poisson, data = data)
    
    
    m <- 20
    data_imp1 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                      method = "desuppress", print = FALSE, m = m)
    
    data_imp2 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                      method = "uniform", print = FALSE, m = m)
    
    mod_mice1 <- with(data_imp1, glm(censored_count_num ~ animals * region, family = "poisson"))
    coef_mice1 <- pool(mod_mice1)$pooled$estimate
    
    mod_mice2 <- with(data_imp2, glm(censored_count_num ~ animals * region, family = "poisson"))
    coef_mice2 <- pool(mod_mice2)$pooled$estimate
    
    #---------------results----------------------
    # comparison data
    d <- data_frame(underlying = true_coefs, 
                    `Using full data including suppressed values (not usually possible!)` = coef(mod0),
                    `Replacing suppressed values with 3` = coef(mod2),
                    `Left-censored survival-based method` = coef(mod3),
                    `MICE with Poisson proportional probabilities` = coef_mice1,
                    `MICE with uniform probabilities` = coef_mice2,
                    labels = names(coef(mod0))) %>%
      mutate(labels = gsub("animals", "", labels),
             labels = gsub("region", "", labels)) %>%
      gather(method, value, -underlying, -labels) %>%
      mutate(value = ifelse(is.na(value), 0, value)) 
    
    # summary data:  
    d2 <- d %>%
      # the intercept is on a different scale to all the other coefficients
      filter(labels != "(Intercept)") %>%
      mutate(square_error = (value - underlying) ^ 2) %>%
      group_by(method) %>%
      summarise(mse = mean(square_error),
                trmse = mean(square_error, tr = 0.2)) %>%
      ungroup()  %>%
      mutate(method = fct_reorder(method, mse)) %>%
      arrange(trmse) %>%
      mutate(method = fct_reorder(method, trmse)) %>%
      mutate(run = run,
             prop_suppressed = prop_suppressed)
    
    return(d2)
  }
}

results <- list()
for(i in 1:50){
  results[[i]] <- coefs_comp(i)
}

results_df <- do.call("rbind", results)

results_df %>%
  mutate(method = fct_reorder(method, trmse)) %>%
  ggplot(aes(x = method, y = trmse)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip()

results_df %>%
  mutate(method = fct_reorder(method, mse)) %>%
  ggplot(aes(x = prop_suppressed, y = mse)) +
  facet_wrap(~method) +
  geom_jitter() +
  scale_y_log10() +
  geom_smooth(method = "lm")
