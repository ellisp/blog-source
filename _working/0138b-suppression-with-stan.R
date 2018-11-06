
#===========================With Stan==========================
# If on windows, make sure stan was installed this way: 
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows.
# You can't just install.packages("rstan")


# This turned out to be a dead end.
# I thought I could model the misisng values as poisson parameters in their own way;
# but Stan can't get estimates for integer parameters. If we say they can be real,
# then we can't model y as poisson at all. If I then try to model y as normal but
# with variance equal to the mean, I have a non-linear transformation to deal with
# that is beyond my competence (but this does still seem to run)
library(rstan)
library(rstanarm)
# caution that the next line tells Stan to use all the cores available.  Makes sense on my
# laptop, may not if you're on a supercomputer...
options(mc.cores = parallel::detectCores(), auto_write = TRUE)

# get data into shape for Stan. First, the model matrix but without the intercept
stan_data <- list(
  X = mm,
  p = length(true_coefs), # number of slopes to estimate including intercept
  n = n,                  # number of observations
  y = data$count)

moda <- stan("0138-moda.stan", data = stan_data, control = list(max_treedepth = 10))

moda_coefs <- apply(as.array(moda), 3, mean)[1:12]


moda_glm <- glm(count ~ animals * region, family = poisson, data = data)
plot(coef(moda_glm), moda_coefs[1:12])

# diagnostics:
# launch_shinystan(moda)

modb <- stan("0138-modb.stan", data = stan_data, control = list(max_treedepth = 10))
modb

modb_coefs <- apply(as.array(modb), 3, mean)[1:12]
pairs(data.frame(true_coefs, stan1 = moda_coefs, stan2 = modb_coefs, glm = coef(moda_glm)))
