library(tidyverse)
library(magrittr)
library(forcats)
library(scales)
library(MatchIt)           # for propensity score matching
library(MASS)              # for mvrnorm
library(clusterGeneration) # for genPositiveDefMat
library(boot)              # for inv.logit
library(testthat)          # for expect_equal
library(doParallel)        # for parallel processing
library(foreach)
library(stringr)
library(RColorBrewer)

#==========explore omitted variable bias==================
#' generate data suitable for modelling with propensity score matching
#' @param n number of observations to make
#' @param k number of "x" variables that influence both the probability of getting the treatment,
#' and the response variable
#' @param p overall probability of getting the treatment
#' @param x_coefs_y the coefficients for each of the x variables impacting on y
#' @param x_coefs_propensity the coefficients for each of the x variables impacting on the probability 
#' of getting the treatment (prior to scaling proportions)
#' @param treatment_coef the effect of the treatment on y
#' @return a data frame
generate_data <- function(n = 50, k = 5, p = 0.1, 
                          x_coefs_y = rnorm(k), 
                          x_coefs_propensity = rnorm(k),
                          treatment_coef = 1,
                          sigma_y = 1,
                          seed = NULL){
   if(!is.null(seed)){
      set.seed(seed)
   }
   sigma <- cov2cor(clusterGeneration::genPositiveDefMat(k)$Sigma)
   x <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = sigma)
   true_prob <- boot::inv.logit(x %*% x_coefs_propensity)
   # scale so only p proportion will get the treatment
   true_prob <- true_prob * (p / (sum(true_prob) / n))
   treatment <- rbinom(n = n, size = 1, prob = true_prob)
   y <- x %*% x_coefs_y + treatment * treatment_coef + rnorm(n, 0, sigma_y)
   the_data <- as.data.frame(x)
   the_data$treatment <- treatment
   the_data$y <- as.vector(y)
   return(the_data)
}

#=====================loop=================
# sequence takes about 8 hours

# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(foreach)
   library(tidyverse)
   library(MatchIt)
   library(magrittr)
})

# Different sample sizes to try:
n_options <- c(500, 1000, 2000, 5000, 10000, 20000, 10^5)
# n_options <- c(500, 1000, 2000) # used during dev
# Different number of variables to include:
var_options <- 1:10 * 10
# Number of reps to do of each combination of sample size and variables in:
reps <- 30

# loop through all the options for sample size.  Note this loop is serial, not parallel:
results <- foreach(j = 1:length(n_options), .combine = rbind) %do% {
   
   # parallelising this next loop, but not the others.  So each rep of the given
   # sample size will be given its own processor:
   tmp1 <- foreach(i = 1:reps, .combine = rbind) %dopar% {
      the_data <- generate_data(n = n_options[j], k = 100, seed = i * 100 + j)
      
      # treatments <- dplyr::filter(the_data, treatment == 1)
      # controls <- dplyr::filter(the_data, treatment == 0)
      # n_treatment <- nrow(treatments)
      
      # Loop through all the different numbers of observed explanatory variables.
      # Note this is done in serial too, so a single processor, given its own
      # rep dataset of a particular sample size will perform all the different
      # matchings and regressions for different selections of variables.  This
      # is hoped to be a good compromise between overall CPU utilisation and
      # not swapping data and stuff in and out of processors too frequently:
      tmp2 <- foreach(v = 1:length(var_options), .combine = rbind) %do% {
         vars_in <- 1:var_options[v]
         incomplete_data <- dplyr::select(the_data, vars_in, treatment, y)
         
         #-----------------Simple regression-----------
         mod1 <- lm(y ~ ., data = incomplete_data)
         result_lm <- coef(mod1)["treatment"]
         
         #----------propensity score calculation----------
         the_form <- as.formula(
            paste("treatment ~ ", paste(paste0("V", vars_in), collapse = " + "))
         )
         
         # I considered comparing genetic matching but it is slower and not really the point
         # of today's piece, so just using the default nearest neighbour matching method
         match_model <- matchit(the_form, data = incomplete_data)
         match_data <- match.data(match_model)
         
         #---------------compare means of matched groups----------------
         result_psm <- with(match_data,
              mean(y[treatment == 1]) -
                 mean(y[treatment == 0]))
         
         #--------------regression with matched groups-----------------
         mod2 <- lm(y ~ ., data = match_data)
         result_psm_lm <- coef(mod2)["treatment"]

         #--------------------regression with full data and IPTW weights----------
         # IPTW - Inverse Probability of Treatment Weights
         # keep all the data but use propensity to generate weights.
         # has the advantage of keeping all the data rather than just a "matched" control sample.
         wgts <- incomplete_data %>%
            mutate(props = predict(match_model$model, newdata = incomplete_data, type = "response"),
                   wgts  = 1/ props * treatment + 1 / (1 - props) * ( 1 - treatment)) %$%
            wgts
         
         mod3 <- lm(y ~ ., data = incomplete_data, weights = wgts)
         result_iptw_lm <- coef(mod3)["treatment"]
         
         #--------------return results-------------
         c(result_lm, result_psm, result_psm_lm, result_iptw_lm)
      }  # end and repeat for each value of the number of explanatory variables to include in `var_options`
      
      tmp2 <- as.data.frame(tmp2)
      tmp2$n <- n_options[j]
      tmp2$vars_in <- var_options
      tmp2$dataset <- i
      row.names(tmp2) <- NULL
      tmp2
   }     # end and repeat for each of `reps` repetitions of new datasets of same sample size
   
   tmp1
}        # end and repeat for each of sample size options in `n_options``
# save(results, file = "results.rda")

names(results)[1:4] <- c("Straight regression", "Propensity matching",
                         "Propensity matching and regression", "Inverse weighting and regression")

#=============================presentation====================
palette <- brewer.pal(4, "Set1")
names(palette) <- names(results)[1:4]

svg("../img/0091-boxplot-maxvars.svg", 9, 6)
results %>%
   filter(vars_in == 100) %>%
   gather(method, value, -vars_in, -dataset, -n) %>%
   mutate(method = fct_reorder(method, value, fun = median, na.rm = TRUE, .desc = TRUE)) %>%
   ggplot(aes(x = as.factor(n), colour = method, y = value)) +
   geom_boxplot() +
   geom_hline(yintercept = 1) +
   scale_colour_manual(values = palette) +
   labs(x = "Sample size", y = "Estimated treatment effect", colour = "") +
   ggtitle("Distribution of estimated treatment effects",
           "30 repetitions for each sample size; all 100 variables observed and included in model")
dev.off()

svg("../img/0091-boxplot-80vars.svg", 9, 6)
results %>%
   filter(vars_in == 80) %>%
   gather(method, value, -vars_in, -dataset, -n) %>%
   mutate(method = fct_reorder(method, value, fun = median, na.rm = TRUE, .desc = TRUE)) %>%
   ggplot(aes(x = as.factor(n), colour = method, y = value)) +
   geom_boxplot() +
   geom_hline(yintercept = 1) +
   scale_colour_manual(values = palette) +
   labs(x = "Sample size", y = "Estimated treatment effect", colour = "") +
   ggtitle("Distribution of estimated treatment effects",
           "30 repetitions for each sample size; 80/100 variables observed and included in model")
dev.off()


svg("../img/0091-individual-datasets.svg", 9, 7)
results %>%
   filter(n %in% c(1000, 5000, 10000, 100000)) %>%
   # filter(dataset %in% 1:10) %>%
   gather(method, value, -vars_in, -dataset, -n) %>%
   mutate(n = paste0("n =", format(n, big.mark = ",", scientific = FALSE)),
          dataset = paste0("Data ", dataset),
          method = str_wrap(method, width = 20),
          method = fct_reorder(method, value, fun = min)) %>%
   ggplot(aes(x = vars_in, y = value, colour = dataset)) +
   geom_hline(yintercept = 1) +
   geom_line() +
   facet_grid(method~n) +
   scale_x_continuous(breaks = var_options) +
   scale_y_continuous(limits = c(-1, 3))  +
   theme(legend.position = "none",
         panel.grid.minor = element_blank()) +
   labs(x = "Number of variables included (out of 100)",
        y = "Estimate of treatment effect (true value is 1)",
        caption = "Each line represent a different simulated dataset.") +
   ggtitle("Different estimates of treatment effect", 
           "Different methods, number of variables included, sample sizes.
Note that missing 10 variables from the model is all that is needed for materially inaccurate estimates.")
dev.off()

svg("../img/0091-squared-error.svg", 11, 6)
results %>%
   filter(vars_in %in% c(10, 30, 50, 70, 90, 100)) %>%
   gather(method, value, -vars_in, -dataset, -n) %>%
   mutate(vars_in = factor(paste(vars_in, "variables"),
                           levels = paste(c(10, 30, 50, 70, 90, 100), "variables"))) %>%
   mutate(AbsError = abs(value - 1)) %>%
   mutate(method = factor(method, levels = c("Propensity matching", "Propensity matching and regression",
                                             "Inverse weighting and regression", "Straight regression"))) %>% 
   ggplot(aes(x = n, y = AbsError, colour = method)) +
   geom_smooth(se = FALSE, method = "loess", span = 1.5, size = 0.6) +
   geom_point(alpha = 0.3) +
   scale_x_log10(label = comma) +
   scale_y_log10(label = comma) +
   facet_wrap(~vars_in, ncol = 3) +
   scale_colour_manual(values = palette[c(1,2,4,3)]) +
   theme(legend.position = "right",
         panel.grid.minor = element_blank()) +
   ggtitle("Absolute error of estimated treatment effect",
           "Different methods, number of variables included (out of 100), sample sizes.
A sample size of 500 with all 100 important variables included is as good as 100,000 with only 90 variables.") +
   labs(x = "Sample size", colour = "") 
dev.off()

# - all methods are basically ok when all variables are included, but propensity matching is not efficient;
# - propensity matching without regression is much less efficient than propensity matching with regression,
# and both are not as good as either of the methods that fit regression models to the full data (with or 
# without weights)
# - missing 10 variables is enough for all methods to go awry (second plot)
# - missing variables is worse than small sample size.  Incredibly, a sample size of 500 but
# with all 100 variables observed is as good as a sample size of 100,000 with only 90 variables
# observed - so long as a regression of some sort is used

convert_pngs("0091")
