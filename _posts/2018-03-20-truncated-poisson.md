---
layout: post
title: Truncated Poisson distributions in R and Stan
date: 2018-03-20
tag: 
   - R
   - Stan
description: Two ways of fitting a model to truncated data.
image: /img/0120-truncated-poisson.svg
socialimage: https:/freerangestats.info/img/0120-truncated-poisson.png
category: R
---

## Data that's been only partially observed

I've been updating my skills in fitting models to truncated data and was pleased to find that, like so much else in statistics, it's easier than it used to be.

First, some definitions:

- *censored* data is where some observations are cut off at some maximum or minimum level; those data points are "off the scale" but at least we know they exist and we know which direction they are off the scale.  For example, if we were analysing the life span of people born in 1980, anyone who has survived to the end of 2017 has their age at death recorded as "37 or higher".  We know they're in the data, and their value is at least some minimum amount, but we don't know with precision what it will end up being.
- *truncated* data is where data beyond some maximum or minimum level is just missing.  Typically this is because of some feature of the measurement process eg anything smaller than X just doesn't show up.

I've got some future blog posts on a more substantive real life issue where I have count data for which, in some situations, I only see the observations with a count higher than some threshold.  Let's imagine, for example, we are looking at deaths per vehicle crash, and are dependent for measurement on newspapers that only report crashes with at least two deaths, even though many crashes have one or zero deaths.

Here's a greatly simplified example.  I generate 1,000 observations of counts, with an average value of 1.3.  Then I compare that original distribution with what I'd get if only those of two or higher were observed.  It looks like this:

<img src='/img/0120-truncated-poisson.svg' width = '100%'>

...generated by this code:

{% highlight R %}
library(tidyverse)
library(scales)
library(fitdistrplus)
library(rstan)
library(truncdist)

# original data:
set.seed(321)
a <- rpois(1000, 1.3)

# truncated version of data:
b <- a[ a > 1]

# graphic:
data_frame(value = c(a, b),
           variable = rep(c("Original data", "Truncated so only observations of 2 or more show up"), c(length(a), length(b)))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, colour = "white") +
  facet_wrap(~variable, ncol = 1) +
  ggtitle("Comparing full and truncated datasets from a Poisson distribution") +
  labs(y = "Number of observations")

# fitting a model to original works well:
mean(a)
fitdistr(a, "Poisson")

# but obviously not if naively done to the truncated version:
mean(b)
fitdistr(b, "Poisson")
{% endhighlight %}

Estimating the key parameter `lambda` for the full data (`a`) works well, giving an estimate of 1.347 that is just over one standard error from the true value of 1.3.  The `fitdistr` function from the `MASS` package distributed with base R does a nice job in such circumstances.

But the mean value of `b` is badly biased upwards if used to estimate `lambda`; at 2.6 the mean of `b` is roughly twice the correct value of the mean of the underlying distribution.  Obviously, removing a whole bunch of data at one end of the distribution is going to make naive estimation methods biased.  So we need specialist methods that try to estimate lambda on the assumption that the data come from a Poisson distribution, but only the right-most part of it.

## Maximum likelihood

The `fitdistrplus` package by Aurélie Siberchicot, Marie Laure Delignette-Muller and Christophe Dutang  in combination with `truncdist` by Frederick Novomestky and Saralees Nadarajah gives a straightforward way to implement maximum likelihood estimation of a truncated distribution.  Methods other than maximum likelihood are also available if required.

You need to make truncated versions of the `dpois` and `ppois` functions (or their equivalents for whatever distribution you are modelling) and use these within `fitdistrplus::fitdist`, which has some added functionality over `MASS::fitdistr` used in the previous chunk of code.

{% highlight R %}
#-------------MLE fitting in R-------------------
# adapted from https://stackoverflow.com/questions/16947799/fitting-a-lognormal-distribution-to-truncated-data-in-r
dtruncated_poisson <- function(x, lambda) {
  dtrunc(x, "pois", a = 1.5, b = Inf, lambda)
}
ptruncated_poisson <- function(x, lambda) {
  ptrunc(x, "pois", a = 1.5, b = Inf, lambda)
}

fitdist(b, "truncated_poisson", start = c(lambda = 0.5)) 
{% endhighlight %}

Note that to do this I specified the lower threshold as 1.5; as all the data are integers this effectively means we only observe the observations of 2 or more, as is the case.  We also needed to specify a reasonably starting value for the estimate of `lambda` - getting this too far out will lead to errors.

This method gives us an estimate of 1.34 with a standard error of 0.08, which is pretty good given we've only got 398 observations now.   Of course, we've got the luxury of knowing for sure the true data generating process was Poisson.

## Bayesian

For an alternative Bayesian method, Stan makes it easy to describe data and probability distributions as truncated.  The Stan manual has an entire chapter on truncated or censored data.  Here's an example Stan program to estimate the mean of the original Poisson distribution from  our truncated data.  As well as the original data, which I call `x` in this program, we need to tell it how many observations (`n`), the `lower_limit` that it was truncated by, and whatever is needed to characterise a prior distribution for the parameter we're estimating.

The key bits of the program below are:

- In the `data` chunk, specify that the data for `x` has a lower limit of `lower_limit` 
- In the `model` chunk, specify that distribution of `x` is truncated via `T[lower_limit, ]`

{% highlight Stan %}
data {
  int n;
  int lower_limit;
  int <lower = lower_limit> x[n];
  real lambda_start_mu;
  real lambda_start_sigma;
}

parameters {
  real<lower = 0>lambda;
}

model {
  lambda ~ normal(lambda_start_mu, lambda_start_sigma);
  
  for(i in 1:n){
    x[i] ~ poisson(lambda) T[lower_limit, ];
  }
}
{% endhighlight %}

With a little more effort it's possible to extend this by making Stan estimate `lower_limit` from the data; not necessary in my hypothetical example because I know where the minimum cut-off point of observed data lies.

Here's how the data are fed to Stan from R:

{% highlight R %}
#-------------Calling Stan from R--------------
data <- list(
  x = b,
  lower_limit = 2,
  n = length(b),
  lambda_start_mu = 2,
  lambda_start_sigma = 1
)

fit <- stan("0120-trunc.stan", data = data, cores = 4)
fit

plot(fit) + 
  ggtitle("Credibility interval for lambda, estimated by Stan from truncated data",
                    "(Correct value is 1.3)") +
  labs(y = "Estimated parameters") +
  theme_minimal(base_family = "myfont")
{% endhighlight %}

This gives us a posterior distribution for `lambda` that matches that from the `fitdistrplus` method: 1.35 with a standard deviation of 0.08.  The `rstan` package automatically turns this into a `ggplot2` image of a credibility interval:

<img src='/img/0120-stan-fit.svg' width = '100%'>

So, nice.  Two simple ways to estimate the original distribution from truncated data.




