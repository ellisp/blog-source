library(tidyverse)
library(scales)
library(fitdistrplus)
library(rstan)
library(truncdist)

# original data
set.seed(321)
a <- rpois(1000, 1.3)

# truncated version of data, only observations of at least 2 or more
b <- a[ a > 1]

svg("../img/0120-truncated-poisson.svg", 8, 5)
data_frame(value = c(a, b),
           variable = rep(c("Original data", "Truncated so only observations of 2 or more show up"), c(length(a), length(b)))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, colour = "white") +
  facet_wrap(~variable, ncol = 1) +
  ggtitle("Comparing full and truncated datasets from a Poisson distribution") +
  labs(y = "Number of observations")
dev.off()  

# fitting a model to original works well:
mean(a)
fitdistr(a, "Poisson")

# but obviously not if naively done to the truncated version:
mean(b)
fitdistr(b, "Poisson")

#-------------MLE fitting in R-------------------
# see https://stackoverflow.com/questions/16947799/fitting-a-lognormal-distribution-to-truncated-data-in-r
dtruncated_poisson <- function(x, lambda) {
  dtrunc(x, "pois", a = 1.5, b = Inf, lambda)
}
ptruncated_poisson <- function(x, lambda) {
  ptrunc(x, "pois", a = 1.5, b = Inf, lambda)
}

fitdist(b, "truncated_poisson", start = c(lambda = 0.5))  


#---------------Stan version----------------------

data <- list(
  x = b,
  lower_limit = 2,
  n = length(b),
  lambda_start_mu = 2,
  lambda_start_sigma = 1
)

fit <- stan("0120-trunc.stan", data = data, cores = 4)
fit

svg("../img/0120-stan-fit.svg",8, 2)
plot(fit) + 
  ggtitle("Credibility interval for lambda, estimated by Stan from truncated data",
                    "(Correct value is 1.3)") +
  labs(y = "Estimated parameters") +
  theme_minimal(base_family = "myfont")
dev.off()

convert_pngs("0120")

shoulders() %>% DT::datatable()
