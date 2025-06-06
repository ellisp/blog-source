library(tidyverse)
library(mice)
library(VGAM)
library(Cairo)
library(parallel)


#------------------ imputation functions----------
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
  if (is.null(wy)){ 
    wy <- !ry
  }
  
  return(sample(0:max_value, sum(wy), replace = TRUE))
}

#--------------main function---------------
# This function  generates data and compares coefficients
coefs_comp <- function(run, output = c("summary", "all")){
  set.seed(run)
  output <- match.arg(output)
  
  data <- expand.grid(animals = letters[1:sample(2:10, 1)],
                      region = LETTERS[1:sample(2:10, 1)],
                      count = 1) 
  
  n <- nrow(data)
  
  mm <- model.matrix(count ~ animals * region, data = data)
  
  true_coefs <- c(runif(1, 1, 3), runif(n - 1, -1, 1))
  data <- data %>%
    mutate(expected = exp(mm %*% true_coefs),
           count = rpois(n, lambda = exp(mm %*% true_coefs)),
           censored_count = ifelse(count < 6, "<6", count),
           censored_count_num = suppressWarnings(as.numeric(censored_count)),
           count_replaced_5 = ifelse(count < 6, 5, count),
           count_replaced_3 = ifelse(count < 6, 3, count),
           count_replaced_1 = ifelse(count < 6, 1, count),
           count_replaced_0 = ifelse(count < 6, 0, count))
  
  data$z <- pmax(6, data$count)
  data$lcensored <- is.na(data$censored_count_num )
  prop_suppressed <- mean(data$lcensored)
  data$which_complete <- as.numeric(!data$lcensored)
  
  if(prop_suppressed > 0.5){
    return(NULL)
  } else {
    
    #--------------straightforward methods-----------
    # with the full unsuppressed data, as though you were the ABS:
    mod0 <- glm(count ~ animals * region, data = data, family = "poisson")
    
    # with replacing the suppressed data with 3 or 5:
    mod1 <- glm(count_replaced_3 ~ animals * region, data = data, family = "poisson")
    mod2 <- glm(count_replaced_5 ~ animals * region, data = data, family = "poisson")
    mod2a <- glm(count_replaced_1 ~ animals * region, data = data, family = "poisson")
    mod2b <- glm(count_replaced_0 ~ animals * region, data = data, family = "poisson")
    
    #------------with censored poisson regression-------------------------
    
    mod3 <- vglm(SurvS4(z, which_complete, type = "left") ~ animals * region, 
                 family = cens.poisson, data = data)
    
    #------------with multiple imputation
    # number of sets of imputations to make
    m <- 20
    
    # See https://github.com/stefvanbuuren/mice/issues/150 for why remove.collinear = FALSE.
    data_imp1 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                      method = "desuppress", print = FALSE, m = m,
                      remove.collinear = FALSE)
    
    data_imp2 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                      method = "uniform", print = FALSE, m = m,
                      remove.collinear = FALSE)

    # default mice method:
    data_imp3 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                      print = FALSE, m = m,
                      remove.collinear = FALSE)
    
        
    mod_mice1 <- with(data_imp1, glm(censored_count_num ~ animals * region, family = "poisson"))
    coef_mice1 <- pool(mod_mice1)$pooled$estimate
    
    mod_mice2 <- with(data_imp2, glm(censored_count_num ~ animals * region, family = "poisson"))
    coef_mice2 <- pool(mod_mice2)$pooled$estimate

    mod_mice3 <- with(data_imp3, glm(censored_count_num ~ animals * region, family = "poisson"))
    coef_mice3 <- pool(mod_mice3)$pooled$estimate
    
        
    
    

    #---------------results----------------------
    # comparison data
    d <- data_frame(underlying = true_coefs, 
                    `Using full data including suppressed values (not usually possible!)` = coef(mod0),
                    `Replacing suppressed values with 0` = coef(mod2b),
                    `Replacing suppressed values with 1` = coef(mod2a),
                    `Replacing suppressed values with 3` = coef(mod1),
                    `Replacing suppressed values with 5` = coef(mod2),
                    `Left-censored survival-based method` = coef(mod3),
                    `MICE with Poisson proportional probabilities` = coef_mice1,
                    `MICE with uniform probabilities` = coef_mice2,
                    `MICE with default imputation` = coef_mice3,
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
             prop_suppressed = prop_suppressed,
             n = n)
    if(output == "summary"){
      return(d2)  
    } 
    if(output == "all"){
      return(list(
        all_coefficients = d,
        performance_summary = d2,
        data = data
      ))
    }
    
  }
}
#--------------------do the runs----------------
# It was useful to do this in a single-threaded way during dev to locate problems such
# as with run 357, but when that was sorted it was better to dso the experiments with 
# parallel processing.

# set up cluster with number of cores minus one
cores <- detectCores()
cluster <- makePSOCKcluster(max(1, cores - 1))

# set up the functionality on the cluster
clusterEvalQ(cluster, {
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(mice)
  library(VGAM)
})
clusterExport(cluster, c("coefs_comp", "mice.impute.desuppress", "mice.impute.uniform"))

# Generate data for a sequence of seeds and compare the coefficient from the various methods
results <- parLapply(cluster, 1:1000, coefs_comp)

stopCluster(cluster)

#--------------------present results-------------------
results_df <- do.call("rbind", results) %>%
  mutate(method = fct_reorder(str_wrap(method, 27), mse))

summary(results_df)

p1 <- results_df %>%
  ggplot(aes(x = method, y = mse)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  labs(y = "Mean squared error of coefficients, compared to true data generating process",
       x = "") +
  ggtitle("Fitting a Poisson GLM to a cross tab of counts",
          "Comparison of methods of dealing with suppressed counts under 6")

CairoSVG("../img/0139-boxplot.svg", 8, 5)
print(p1)
dev.off()

p2 <- results_df %>%
  ggplot(aes(x = prop_suppressed, y = mse)) +
  facet_wrap(~method) +
  geom_point(size = 0.3) +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(y = "Mean squared error of coefficients,\ncompared to true data generating process",
       x = "Proportion of cells in original data that are under 6") +
  ggtitle("Fitting a Poisson GLM to a cross tab of counts",
          "Comparison of methods of dealing with suppressed counts under 6")


CairoSVG("../img/0139-scatter.svg", 10, 6)
  print(p2)
dev.off()

p3 <- results_df %>%
  ggplot(aes(x = n, y = mse)) +
  facet_wrap(~method) +
  geom_point(size = 0.3) +
  scale_y_log10() +
  geom_smooth(method = "lm")  +
  labs(y = "Mean squared error of coefficients,\ncompared to true data generating process",
       x = "Number of coefficients to estimate") +
  ggtitle("Fitting a Poisson GLM to a cross tab of counts",
          "Comparison of methods of dealing with suppressed counts under 6")

CairoSVG("../img/0139-scatter-n.svg", 10, 6)
print(p3)
dev.off()

#--------------further investigation----------
p4 <- results_df %>%
  ggplot(aes(x = mse)) +
  facet_wrap(~method) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_log10(label = comma, breaks = c(0.1, 0.3, 1, 3, 10, 100)) +
  ggtitle("Bimodal distribution of performance when using the real data") +
  theme(panel.grid.minor = element_blank())

CairoSVG("../img/0139-densities.svg", 10, 6)
print(p4)
dev.off()

# which are the really poor performing methods when using the full data?
full_data_method <- results_df %>%
  filter(grepl("^Using full data", method )) %>%
  arrange(desc(mse))

full_data_method
# some really bad runs are 125, 721, 506

CairoSVG("../img/0139-full-data-method.svg", 8, 6)
full_data_method %>%
  ggplot(aes(x = prop_suppressed, y = n, colour = mse > 3, size = mse)) +
  geom_point()  +
  labs(x = "Proportion of data that were suppressed as <6",
       y = "Number of cells in table",
       size = "Mean squared error of coefficients:",
       colour = "Unusually large:") +
  ggtitle("Performance of a model with the true underlying data",
          "General success and occasional complete failure to pick the underlying data generating process")
dev.off()

run125 <- coefs_comp(125, output = "all")

run125$data %>%
  select(animals, region, count, expected) %>%
  knitr::kable()

CairoSVG("../img/0139-run125.svg", 10, 8)
run125$all_coefficients %>%
  mutate(method = str_wrap(method, 30)) %>%
  ggplot(aes(x = underlying, y = value, label = labels)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_text() +
  facet_wrap(~method) +
  ggtitle("Example of a particularly poorly performing model with the real data",
          "Model run 125;\nMany zeroes in the data mean the GLM estimates are highly unstable") +
  labs(x = "True value of underlying coefficients",
       y = "Estimate from various types of model")
dev.off()
  
glm(count ~ animals * region, data = run125$data, family = "poisson")

convert_pngs("0139")
