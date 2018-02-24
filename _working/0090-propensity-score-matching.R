library(tidyverse)
library(forcats)
library(scales)
library(MASS)              # for rlm
library(boot)              # for inv.logit
library(MatchIt)
library(boot)              # for bootstrapping
library(doParallel)        # for parallel processing
library(mgcv)              # for gam

#==============example from the MatchIt vignette=================
data(lalonde)

# naive comparison - people who got the job training program
# have lower incomes in 1978 - because the training was given
# to people with income problems.  So comparison is unfair:
lalonde %>%
   group_by(treat) %>%
   summarise(Income1978 = mean(re78),
             n = n())

# Choose one of the large variety of propensity score matching methods to model propensity
match_model <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                       data = lalonde, method = "nearest")
match_data <- match.data(match_model)
   
# Simple comparison is now much fairer
match_data %>%
   group_by(treat) %>%
   summarise(Income1978 = mean(re78),
             n = n())

# regression model estimate with matched data
round(coef(rlm(re78 ~ age + treat + educ + black + hispan + nodegree + married +  re74 + re75, 
        data = match_data))["treat"])

# regression model estimate with original data
round(coef(rlm(re78 ~ age + treat + educ + black + hispan + nodegree + married +  re74 + re75, 
        data = lalonde))["treat"])

#====================================weighting=================
mod <- gam(treat ~ s(age) + educ + black + hispan + nodegree + married + re74 + re75, 
           data = lalonde, family = "binomial")

svg("../img/0090-gam.svg", 7, 5)
par(family = "myfont", bty = "l", font.main = 1)
plot(mod, shade = TRUE, 
     main = "Non-linear function of age in treatment propensity")
dev.off()

lalonde <- lalonde %>%
   mutate(propensity_gam = predict(mod, type = "response"),
          weight_gam = 1 / propensity_gam * treat + 1 / (1 - propensity_gam) * (1 - treat))

svg("../img/0090-weights.svg", 7, 4)
ggplot(lalonde, aes(x = weight_gam, fill = as.factor(treat))) +
   geom_density(alpha = 0.5, colour = "grey50") +
   geom_rug() +
   scale_x_log10(breaks = c(1, 5, 10, 20, 40)) +
   ggtitle("Distribution of inverse probability of treatment weights")
dev.off()

lalonde %>%
   group_by(treat) %>%
   summarise(Income_allweights = round(weighted.mean(re78, weight_gam)),
             Income_truncweights = round(weighted.mean(re78, pmin(10, weight_gam))),
             n = n())
   
coef(rlm(re78 ~ age + treat + educ + black + hispan + nodegree + married +  re74 + re75, 
         data = lalonde, weights = as.vector(weight_gam)))["treat"]

#=================bootstrap for confidence intervals=================
# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(MatchIt)
   data(lalonde)
})

#' Function to estimate treatment effect three different methods
#' @return a vector of four estimates of treatment effect.  See comments
#' in function for description of which is which
my_function <- function(x, i){
   resampled_data <- x[i, ]
   match_data <- match.data(
      matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
              data = resampled_data, method = "nearest")
   )
   
   # simple mean of matched groups
   est1 <- with(match_data,
                mean(re78[treat == 1]) -
                   mean(re78[treat == 0]))
   
   
   # regression model estimate with matched groups
   est2 <- coef(rlm(re78 ~ treat + age + educ + black + hispan + nodegree + married +  re74 + re75, 
           data = match_data))["treat"]
   
   # regression model with IPTW
   mod <- gam(treat ~ s(age) + educ + black + hispan + nodegree + married + re74 + re75, 
              data = resampled_data, family = "binomial")
   
   resampled_data <- resampled_data %>%
      mutate(propensity_gam = predict(mod, type = "response"),
             weight_gam = 1 / propensity_gam * treat + 1 / (1 - propensity_gam) * (1 - treat))
   
   est3 <- coef(rlm(re78 ~ age + treat + educ + black + hispan + nodegree + married +  re74 + re75, 
            data = resampled_data, weights = as.vector(weight_gam)))["treat"]
   
   # regression model estimate with original data
   est4 <- coef(rlm(re78 ~ treat + age + educ + black + hispan + nodegree + married +  re74 + re75, 
                   data = resampled_data))["treat"]
   return(c(est1, est2, est3, est4))
}

my_function(lalonde, 1:nrow(lalonde))

booted <- boot(lalonde, statistic = my_function, R = 5000, 
     parallel = "snow", cl = cluster)

booted_df <- as.data.frame(booted$t)
names(booted_df) <- c("Simple difference with matched data", 
                      "Regression with matched data", 
                      "Regression with weighted data",
                      "Regression with original data")

booted_tidy <- booted_df %>%
   gather("Method", "Estimate") %>%
   mutate(Method = fct_reorder(Method, Estimate)) 

booted_summary <- booted_tidy %>%
   group_by(Method) %>%
   summarise(lower = quantile(Estimate, 0.025),
             upper = quantile(Estimate, 0.975)) %>%
   gather(type, Estimate, -Method)

svg("../img/0090-job-treatment.svg", 8, 5)
   ggplot(booted_tidy, aes(x = Estimate, fill = Method)) +
      geom_density(alpha = 0.4, colour = "grey50") +
      geom_segment(data = booted_summary, aes(xend = Estimate), y = 0, yend = 2e-04) +
      geom_text(data = booted_summary, aes(label = round(Estimate)), y = 2.5e-04, size = 3) +
      facet_wrap(~Method) +
      theme(legend.position = "none") +
      scale_x_continuous(label = dollar) +
      ggtitle("Treatment effects from four different modelling methods",
              "Distribution and 95% confidence intervals of estimated impact on income in 1978 of a job training program") +
      labs(x = "Estimated treatment effect",
           caption = "Lalonde's 1986 data on a job training program",
           fill = "")
dev.off()

# biases revealed by the bootstrap:
booted



convert_pngs("0090")
