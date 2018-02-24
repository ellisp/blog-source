---
layout: post
title: Impact of omitted variables on estimating causal effects - simulations
date: 2017-04-15
tag: 
   - ModellingStrategy
   - R
description: It's much more important to get a well-specified model than worry about propensity score matching versus weighting, or either versus single stage regression, or increasing sample size.  A regression that includes all "true" 100 explanatory variables with only 500 observations performs better in estimating a treatment effect than any of those methods when only 90 of the 100 variables are observed, even with 100,000 observations.
image: /img/0091-squared-error.svg
socialimage: http://ellisp.github.io/img/0091-squared-error.png
category: R
---

## The story so far

[Last week](/blog/2017/04/09/propensity-v-regression) I looked at a few related methods of investigating causality with observational data, where 

- the treatment is expected to be received by observational units (people or firms)... 
- in a way that is structurally related to variables of importance (ie not a random controlled trial)... 
- which also impact on the response of interest.  

The real life data I used was originally [analysed by Lalonde](http://people.hbs.edu/nashraf/LaLonde_1986.pdf), and investigated the impact of a job training program on income.  People who received the job training program were disproportionately in need (ie low income at the time of the program), so we had to take this into account in the analysis.  The three methods I used were 

- *propensity score matching* - which creates a pseudo control group to compare to a treatment group, based on choosing non-treatment groups with a similar "propensity" to receive the treatment as those that actually did; hence discarding most of the data, but creating a nice, interpretable group.
- *inverse propensity treatment weighting* - in which the predicted propensity to receive the treatment is used to create weights for the whole data set, with more weight being given to points that look like they should have received the treatment but didn't, and vice versa.  Those weights can be used in a simple comparison of weighted means, or for a weighted regression.
- *regression* - in which the propensity to receive the treatment isn't explicitly modelled, but the various confounding variables (ethnicity, income at beginning of program, age, etc) are "controlled" for in the usual regression way.

The conclusion was:

> Compared to the older style propensity matching to create a pseudo control sample, it may be better to weight the full data by inverse propensity score because it doesn't discard data. Performing a regression (rather than simple cross tabs) after the weighting or matching is a good idea to handle inevitable imperfections. The whole family of methods doesn't necessarily deliver big gains over more straightforward single stage regressions. And if you have omitted material variables you're in trouble whatever you do.

## The omitted variables problem

Today I'm presenting some simulated data to explore that conclusion, with a particular focus on the omitted variables bias.  What proportion of the "real" explanatory variables need to be missing from our observed set (and hence from the propensity model and/or the regression) for the whole thing to go belly-up?

### Simulations

To explore this I created a bunch of datasets with the following characterstics:

- response variable `y` is independently and identically distributed in a normal distribution with the expected value a linear combination of 100 continuous explanatory variables and one binary `treatment` variable
- the true treatment effect - the coefficient for `treatment` when `y` is the response in a regression - is exactly 1.
- the true coefficients for the 100 other explanatory variables are standard normal variables (ie centered on zero with variance of one)
- the actual values of the explanatory variables are a multivariate normal distribution with non-trivial covariance, and individual means of zero and variances of one
- the propensity to get the treatment is related to the 100 explanatory variables, and in total 10% of cases get the treatment.

Here's the setup of the session in R and the function that I used to create those datasets:

{% highlight R %}
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
generate_data <- function(n = 5000, k = 100, p = 0.1, 
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
{% endhighlight %}

If observed in full, datasets generated this way should be ideal for analysis based either on propensity scores modelling or a simpler single stage regression; I haven't introduced any complications such as outliers in any of the variables, non-linear relationships, or non-normal distribution of the response variable.

Then:

- I generated 30 data sets for each of the following sample sizes: 500, 1,000, 2,000, 5,000, 10,000, 20,000 and 100,000.
- For each of the resulting 210 datasets I created 10 datasets representing various stages of complete observation - with only 10, 20, 30, ..., 100 of the 100 actual explanatory variables.  The idea was to mimic the real-life situation where we don't have access to all the variables that impact on the propensity to get the treatment, or on the eventual variable of interest.
- For each of the 2,100 datasets of various stages of observation I estimated the treatment effect (which has the true value of 1 in each case) with four different methods: 
  - comparison of means after propensity score matching
  - regression on explanatory variables after propensity score matching
  - weighted regression using the inverse propensity of treatment weights, using the full dataset
  - single stage regression without modelling the propensity of treatment explicitly

The whole exercise took about eight hours to run on my laptop, for most of which time it was fully utilising all available resources. 
  
### Results

Let's first look at just the situation where we had all 100 explanatory variables in the model:

<img src='/img/0091-boxplot-maxvars.svg' width = '100%'>

We see that propensity score matching (in blue) and then comparing the means always has a much bigger range of estimates than the three methods that use regression.  We also see (unsurprisingly) that increasing sample size helps a lot for all methods.

Now compare this to when we only observe (and hence include in the analysis) 80 of the 100 true explanatory variables of the same datasets:

<img src='/img/0091-boxplot-80vars.svg' width = '100%'>

Ouch.  The range and variance of estimates is much larger.  Increasing sample size seems to help only marginally.  And the advantage of the three regression methods over simple means comparison after propensity score matching is *much* less than before.  Omitted variables bias - even when only 20% of them are missing - is a killer and it impacts on both bias and [consistency](https://en.wikipedia.org/wiki/Consistent_estimator).  Note that the impact of bias doesn't show up well here because different samples have different data generating processes and the biases are in different directions according to which population the sample is generated from.

We can see the impact of omitting more variables from the next graphic, which shows the results of fitting the model with different numbers of explanatory variables included:

<img src='/img/0091-individual-datasets.svg' width = '100%'>

When all 100 variables are included (to the far right of each facet), all the models are pretty good, although simple means comparison after propensity score matching noticeably is a bit further away from the true value of 1 than the other methods.  But when only 90% of explanatory variables are observed and included - and much more for when fewer variables are in the model - the estimates of the treatment effect get systematically out, in a direction which varies for individual datasets (each represented by one colour line).  Missing 20% of the variables (ie including only 80) is enough for the effect to be bad; and increasing the sample size from 10,000 to 100,000 doesn't make much difference.

The final graphic shows how all these estimates are inconsistent (ie don't converge to correct answer with increasing sample size) when less than the full 100 variables are included:

<img src='/img/0091-squared-error.svg' width = '100%'>

Again, we also see that the regression methods (with or without propensity score matching or weighting) are far better than simple means comparison after propensity score matching when all variables are observed and included in the model, but this isn't anywhere as obviously the case when there is an omitted variables problem.

## Conclusions

- all four methods are basically ok when all variables are included, but simply comparing means (without a regression) after propensity matching is not efficient;
- propensity matching without regression is much less efficient than propensity matching with regression, and both are not as good as either of the methods that fit regression models to the full data (with or without weights)
- missing 10 variables out of 100 is enough for all methods to go awry
- missing variables is worse than small sample size.  At its most extreme, a sample size of 500 but with all 100 variables observed is pretty much as good as a sample size of 100,000 with only 90 variables observed - so long as a regression of some sort is used.

## R code

Here's the R code that does the actual simulations, analysis and prepares those charts; it takes up where the chunk of code earlier in this post finishes.

{% highlight R %}
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

names(results)[1:4] <- c("Straight regression", "Propensity matching",
                         "Propensity matching and regression", "Inverse weighting and regression")

#=============================presentation====================
palette <- brewer.pal(4, "Set1")
names(palette) <- names(results)[1:4]

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
{% endhighlight %}


