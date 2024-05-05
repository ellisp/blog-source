---
layout: post
title: Estimating relative risk in a simulated complex survey
date: 2018-08-24
tag: 
   - Surveys
   - R
description: Simulating complex survey data in order to fit slightly mis-specified relative risk models, we find that confidence intervals' coverage is pretty much as advertised if we use appropriate methods that adjust for the complex survey data, but under-perform if the data is treated naively as coming from a simple random sample.
image: /img/0132-comparisons.svg
socialimage: https:/freerangestats.info/img/0131-comparisons.png
category: R
---

In this post I simulate a population and a complex survey of it.  The survey has stratification, two-stage sampling and post-collection calibration to marginal population totals.  The original idea was to follow up on [last week's post](https:/freerangestats.info/blog/2018/08/17/risk-ratios) on relative risk ratios and odds ratios and in particular to explore the use of the `quasipoisson(log)` family compared to the `quasibinomial(log)` family when modelling a binary outcome with categorical data.  

In the end, the more interesting thing (for me) was the general challenge of simulating a population and complex survey process and doing some basic comparison of the success of different estimation strategies.  In particular, I was interested in how different the results are if we treat the sample with appropriate complex survey methods (ie Thomas Lumley's `survey` R package) in contrast to naively fitting the same model while ignoring the cluster sampling, stratification, post-stratification, etc.

To cut to the chase, this chart compares those two approaches fit to 100 different samples of 2,100 individuals:

<img src='/img/0132-comparisons.svg' width='100%'>

What we see is that there isn't much to choose between them when it comes to point estimates.  The mean squared errors of the point estimates of effect sizes from both methods, compared to fitting the model to the full population of 1 million people, are similar.  However, the complex survey methods do better in terms of confidence intervals.  The 95% confidence intervals delivered by survey methods contained the true values 94% of the time, nearly as good as advertised; whereas the naive method's confidence intervals contained the true values only 89% of the time.

## Simulating data

I wanted to simulate data that resembled real-life survey data that I often work with - mostly categorical values, some unknown variables, some reasonable models of behaviour available but certainly at least partly mis-specified.  The one complication I left out for now was missing data.

I created data in 14 regions, with 100 neighbourhoods or PSUs (primary sampling units) in each region that were going to be used for cluster sampling.  I made variables for people's "shape" (circle, square, triangle or hexagon), "colour" (red, blue, etc), a mystery unobserved variable, an unobserved latent continuous variable that depends on all the other variables mentioned so far, and a single target or response variable of interest called `y` which is TRUE or FALSE with probability depending on that latent variable.  

In the end we are going to use these variables in these ways:

- We'll use `region` as strata and `psu` as primary sampling unit in a cluster sampling approach
- We'll use the marginal population counts by `region`, `shape` and `colour` in a weighting scheme
- We'll use `region`, `shape` and `colour` as explanatory variables in a model with `y`.  This model is slightly mis-specified because it doesn't include the "mystery variable" which is unobserved, and the relationship of the variables to y isn't exactly as such variables normally work together in a logistic regression.

Once I've generated the data, here is how it looks for the full population of one million:

<img src='/img/0132-proportions.svg' width='100%'>

Or here is the correlation matrix, showing the relationships of underlying unobserved continuous variables (rather than their categorical manifestations) with eachother and with `y`:

|            | mystery_var| region_y| psu_y| shape_y| colour_y| latent|    y|
|:-----------|-----------:|--------:|-----:|-------:|--------:|------:|----:|
|mystery_var |        1.00|     0.00|  0.02|    0.00|     0.01|   0.09| 0.01|
|region_y    |        0.00|     1.00|  0.00|    0.00|    -0.01|   0.31| 0.07|
|psu_y       |        0.02|     0.00|  1.00|    0.01|    -0.01|   0.73| 0.18|
|shape_y     |        0.00|     0.00|  0.01|    1.00|     0.07|   0.55| 0.11|
|colour_y    |        0.01|    -0.01| -0.01|    0.07|     1.00|   0.23| 0.05|
|latent      |        0.09|     0.31|  0.73|    0.55|     0.23|   1.00| 0.22|
|y           |        0.01|     0.07|  0.18|    0.11|     0.05|   0.22| 1.00|

For example, `region_y` in the above is a variable that takes a given continuous value for each of regions A, B, C, etc.  The value of `region_y` isn't observed, I just generate it as part of the simulation process.  The underlying value of `region_y` has a correlation of 0.31 with the unobserved underlying value of `latent`. `latent` itself is the direct driver of the probability of `y` being TRUE, but `y` is a random variable so the correlation of `region_y` with `y` is 0.07 (ie less than 0.31), still more than zero but not as strong as `region_y`'s correlation with the unobserved *probability* of y.

`psu_y` has the highest correlation with `y` and I did this by design - I wanted there to be a lot of intra-class correlation at the PSU level, to give distinction to the complex survey sampling and estimation strategy I'm exploring here.  So knowing the PSU (which I think of as a suburb or meshblock) tells you a lot about whether an individual has `y` or not - more so than the individual's shape or colour.  Perhaps `y` is some highly spatially correlated indicator like a contagious disease.

To sum up - in my simulated population, there is a definite relationship between each categorical variable and `y`, via unobserved latent continuous variables, but it's not dramatic.

I wrote an R function to generate this population; even though I only use it once in this blog post, it was very useful to have it as a function with clear parameter choices when I was playing around.

{% highlight R lineanchors %}
library(tidyverse)
library(boot)
library(testthat)
library(survey)
library(broom)
library(scales)
library(gridExtra)
library(kable)

#--------------------create a simulated population------------------------
#' @param N total population size
#' @param n_regions number of regions (which will be used in sampling as strata)
#' @param psu_sd standard deviation of random contribution to latent variable under y at the PSU 
#' (primary sampling unit eg suburb or meshblock) level.  Higher values lead to higher intra-class
#' correlation, and hence higher design effects when we use psu as the primary sampling unit in a
#' sampling design
#' @param region_sd as psu_sd but at region level
#' @param mystery_sd extra random continuous variable that contributes to the latent variable under y.
make_population <- function(N = 100000, 
                            n_regions = 14,
                            psu_sd = 1,
                            region_sd = 1,
                            colours_sd = 1,
                            shapes_sd = 1,
                            mystery_sd = 1,
                            colours_v = c("red", "blue", "green", "yellow", "orange", "grey"),
                            shapes = c("square", "circle", "triangle", "hexagon"),
                            seed = 123){

  set.seed(seed)
  
  
  population1 <- data_frame(
    region = sample(LETTERS[1:n_regions], size = N, replace = TRUE, prob = c(runif(n_regions)))
  ) %>%
    group_by(region) %>%
    mutate(psu = paste(region, sample(1:(n() / 100) , size = n(), replace = TRUE)),
           shape = sample(shapes, size = n(), replace = TRUE)) %>%
    ungroup() %>%
    # we want colours and shapes to be crosscorrelated, so we make generation of colour dependent on shape:
    mutate(colour = case_when(
      shape == shapes[1] ~ sample(colours_v, size = n(), prob =   c( 1, 1, 2, 2, 1, 4), replace = TRUE),
      shape == shapes[2] ~ sample(colours_v, size = n(), prob =   c( 2, 2, 1, 1, 2, 3), replace = TRUE),
      shape == shapes[3] ~ sample(colours_v, size = n(), prob = c( 4, 1, 3, 2, 3, 2), replace = TRUE),
      shape == shapes[4] ~ sample(colours_v, size = n(), prob =  c( 2, 5, 1, 2, 4, 1), replace = TRUE)
    )) %>%
    mutate(mystery_var = as.vector(scale(rnorm(n(), 0, 1) * as.numeric(as.factor(region))) * mystery_sd))
  
  # numeric values to add for each particular value of region, psu, colour, and shape:
  regions <- population1 %>%
    distinct(region) %>%
    mutate(region_y = as.vector(scale(runif(n())) * region_sd))
  
  psus <- population1 %>%
    distinct(psu) %>%
    mutate(psu_y = rnorm(n()) * psu_sd)
  
  shapes <- population1 %>%
    distinct(shape) %>%
    mutate(shape_y = as.vector(scale(rgamma(n(), 1, 0.1)) * shapes_sd))
  
  colours <- population1 %>%
    distinct(colour) %>%
    mutate(colour_y = as.vector(scale(rnorm(n())) * colours_sd))
  
  population <- population1 %>%
    left_join(regions, by = "region") %>%
    left_join(psus, by = "psu") %>%
    left_join(shapes, by = "shape") %>%
    left_join(colours, by = "colour") %>%
    mutate(latent = inv.logit(as.vector(scale(mystery_var + region_y + psu_y + shape_y + colour_y + 
                                               rnorm(n(), 0, 0.05)))) / 2) %>%
    mutate(y = as.logical(rbinom(n(), size = 1, prob = latent)))
  
  return(population)
}

# Simulate the actual population I'll be using for the blog:
population <-make_population(N = 10 ^ 6,
                             psu_sd = 5,
                             region_sd = 2,
                             mystery_sd = 0.5,
                             shapes_sd = 4,
                             colours_sd = 1.5)

# draw graphic
population %>%
  select(region, shape, colour, y) %>%
  gather(variable, value, -y) %>%
  ggplot(aes(x = value, fill = y)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_bar(position = "fill") +
  scale_y_continuous("Prevalence of 'y'", label = percent) +
  coord_flip() +
  labs(x = "") +
  ggtitle("Prevalence of y in simulated population")

# correlation matrix
population %>%
  select(mystery_var:y) %>%
  sample_n(10000) %>%
  cor %>%
  round(2) %>%
  kable()
{% endhighlight %}

## Sampling and processing

Next step is to write a function that takes a two-stage cluster-based sample from this population.  This means we first select a sample of clusters (in this case `psu`); and then from those clusters we select a sample of individuals. I did this in a way that selects a set number of PSUs (defaults to 10, but most of this blog I use 5) from each and every region, then a set random number of people (15 people if we have 10 PSUs per region, 30 people if we have 5) per PSU to bring the total sample size up to 2,100. 2,100 feels an unremarkable sample size for the sorts of surveys I've been looking at this year.

Because the regions have quite different sizes, and the PSUs have slightly different sizes, there are many different probabilities of an individual being selected.  Everyone has a non-zero chance; which we need to calculate for subsequent weighting purposes.  There are R packages that will perform this sort of sampling directly for you, but I've just done it from scratch in the code below for clarity. 

Once I've got the sample, I:

- use the inverse of the selection probability as initial weights
- create a set of jackknife replicate weights suitable for stratified sample designs
- calibrate each set of weights to match the marginal population counts for each value of region, shape and colour.

If replicate weights are new to you, they're worth getting on top of.  When we use the jackknife or bootstrap with simple random samples, we create new samples in quite a simple way, by resampling with replacement from the original sample.  When we have a complex survey, we instead create (and save) new sets of weights reflecting which points are in and which are out, and the new calibrated survey weights for that particular resample.  Compared to doing all the resampling from scratch each time you do an analysis, this puts a lot of the computational load up front in the creation of multiple sets of (expensively) calibrated weights.  These can be re-used repeatedly, for reproducibility and efficiency purposes.

Here's my function, which as well as containing my home-made sampling algorithm, uses Lumley's `survey` package to specify the design, replicate weights, and calibration to population totals for the analysis stage:

{% highlight R lineanchors %}
#==================sampling======================

#' Two stage sample from a "population" data frame
#' 
#' @param population a data frame to be sampled from, must include columns with name region, psu,
#' shape and colour
#' @param npsu number of PSUs (primary sampling units - think suburbs or meshblocks) per region (strata) to
#' saple
#' @param n total sample size
#' @param npeop number of people to sample per PSU
#' @value a survey design object, calibrated to population totals for region, shape and colour; with primary
#' selection by psu and stratified by region; with Jackknife replicate weights.
samp <- function(population, 
                 npsu = 10, 
                 n = 2100, 
                 npeop = round(n / npsu / length(unique(population$region))),
                 seed = 123){

  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # Population totals for margins, which we presume are known from a census or similar.  We are
  # pretending we know the marginal totals for these dimensions, but not a full cross tabulation
  # (ie we know the total number of circles, and of red people, but not of red circular people):
  shape_tots <- population %>%
    group_by(shape) %>%
    summarise(Freq = n())
  
  colour_tots <- population %>%
    group_by(colour) %>%
    summarise(Freq = n())
  
  region_tots <- population %>%
    group_by(region) %>%
    summarise(Freq = n())
  
  # number of people in each psu, which we will use for our weighting:
  region_psu_pops <- population %>%
    group_by(region) %>%
    mutate(psus_in_region = length(unique(psu))) %>%
    group_by(region, psu) %>%
    summarise(
              # how many people in each PSU:
              people_in_psu = n(),
              # given the PSU is sampled, what's the chance of an individual from that PSU being in sample:
              prob_from_psu = npeop / people_in_psu,
              # so what's the a total priori probability for an individual being sampled:
              prob = npsu / unique(psus_in_region) * prob_from_psu,
              # inverse of that probability for use as weight:
              wt = 1 / prob) %>%
    left_join(region_tots, by = "region") %>%
    rename(people_in_region = Freq)
    
  # check that the total sum of probabilities of being selected times people in psus 
  # equals our desired sample size:
  expect_equal(round(n, -1), 
               round(with(region_psu_pops, sum(prob * people_in_psu)), -1))
  
  # we sample PSUs with equal probability within each region (ie not proportionate to size, 
  # which would be a better sampling strategy):
  psu_sampled <- population %>%
    distinct(region, psu) %>%
    # group by region, so each region acts as a stratum and we get the same number of
    # of PSUs per region (again, this isn't efficient sampling, but part of the
    # point of this exercise is to have a complex non-optimal sample):
    group_by(region) %>%
    mutate(seq = rnorm(n())) %>%
    arrange(seq) %>%
    slice(1:npsu) %>%
    select(-seq)
    
  sampled <- population %>%
    select(region, psu, shape, colour, y) %>%
    # limit ourselves to just the PSUs we have chosen for our sample:
    inner_join(psu_sampled, by = c("region", "psu")) %>%
    # within each PSU, pick `npeop` people at random:
    group_by(psu) %>%
    mutate(seq = rnorm(n())) %>%
    slice(1:npeop) %>%
    left_join(region_psu_pops, by = c("region", "psu"))
  
  # Specify the survey design.
  # we will simplify things by ignoring any finite population corrections, either
  # for PSUs within region, or for individuals within PSUs.  This is ok if the sample
  # is small compared to population (which it is).
  
  # Sampling scheme:
  des <- svydesign(~psu, strata = ~region, data = sampled, weights = ~wt)
  
  # Replicate weights:
  des <- as.svrepdesign(des, type = "JKn")
  
  # Post stratification calibration to marginal population totals:
  des <- calibrate(des, list(~shape, ~colour, ~region),
              population = list(shape_tots, colour_tots, region_tots))

  return(des)
  }
{% endhighlight %}

Like most real world complex surveys, this sampling design has a couple of inefficiencies compared to a simple random sample: 

- Because we sample equally from each region, some points (from larger regions) are representing many more population individuals than others.  A typical reason for this strategy would be when we are prepared to sacrifice some efficiency in estimating population totals because we want good estimates of totals for each region, even smaller regions.
- Because we first pick a PSU (think of these as a suburb or meshblock), then sample individuals from that PSU, the individual values are correlated.  Once you've interviewed someone in the PSU, their neighbours are less valuable statistically than people chosen at random.  A typical reason for following this strategy is cost and convenience of moving interviewers around.

All this means that the variance of estimates from our complex survey sample is higher than if we had a sample of the same size that was selected by simple random sampling.  In fact, if the number of PSUs selected per region is the default of 10, an estimated "design effect" for this particular population and sampling strategy is 1.8.  That is, the variance of our estimates is 1.8 times larger than a hypothetical simple random sample.  Another way to look at this is that if a simple random sample were possible, we would need only 2,100 / 1.8 = 1,167 members in the sample to get the same quality estimate of total (population) `y` as from our cluster sample.

```
> des <- samp(population, npsu = 10, seed = 123)
> svymean(~y, des, deff = TRUE)
          mean      SE   DEff
yFALSE 0.74814 0.01282 1.8346
yTRUE  0.25186 0.01282 1.8346
```

The design effect comes mostly from the clustering of individuals in PSUs.  The more PSUs we select (and hence, less individuals per PSU for the same overall sample size), the lower the design effect, as seen in this chart:

<img src='/img/0132-design-effects.svg' width='100%'>

{% highlight R lineanchors %}
npsus <- rep(2:40, 6)

deffs_l <- lapply(npsus, function(i){
  des <- samp(population, npsu = i, seed = NULL)
  X <- svymean(~y, des, deff = TRUE)
  return(c(npsu = i, deff = attributes(X)$"deff"[1,1]))
  })
deffs <- as.data.frame(do.call("rbind", deffs_l))

ggplot(deffs, aes(x = npsu, y = deff)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Number of PSUs selected per stratum", 
       y = "Design effect for mean(y)") +
  ggtitle("Design effect is higher when we rely on fewer primary sampling units")
{% endhighlight %}


## Modelling

OK, so we've got a function to produce our complex sample.  Let's get into the business of modelling `y` based on `region`, `colour` and `shape`.  From now on, I'll be selecting 5 PSUs per region and 30 people per PSU, which gives us a relatively high design effect, but still a total of 14 x 5 = 70 PSUs for the whole sample.  This is a plausibly realistic number of PSUs if the sampling process involves moving around a whole country and interviewing people face to face.  By way of comparison, Lumley's book *Complex Surveys: A Guide to Analysis Using R* cites (page 40) the example of the National Health and Nutrition Examination Survey, NHANES II, which sampled 64 locations and 440 participants in each location, for a relatively intensive and expensive interview/examination process.

Because I'm following on from [last week's post](https:/freerangestats.info/blog/2018/08/17/risk-ratios), I'm interested in relative risks comparing one category to another (eg the probability of a "circle" having `y`, compared to the probability of a "square" having it).  So in the generalized linear model I'm going to fit, I need to use a logarithm link function connecting the expected value of the response variable with the linear predictor of the explanatory variables (*if asking why?  I don't have time to explain that now*).  Two of the best candidates for this are `family = quasibinomial(log)` and `family = quasipoisson(log)`. Taking e to the power of coefficients or other contrast in the linear predictor will give us relative risks in this case.  It's the `log` that's particularly important; the default link function for `quasibinomial` would be `logit`, which would give us odds ratios, not risk ratios.

Here's an example of the sort of outcome I get from two such risk models, fitted to a single sample of 2,100 drawn with my functions set out above.  We can see the relative risks of different levels of my categorical variables (compared to the base levels of the variable in each case).  "1" would mean the given risk level of the value is the same as for the reference level.  So we would conclude, for example, that "square" people have 1.4 to 2.5 times the relative risk of getting `y` as do "circle" people (circle is the reference level for shape). 

<img src='/img/0132-example-cis.svg' width='100%'>

I've marked the "true" values of the coefficients with dots, and shown the 95% confidence intervals from two `quasibinomial(log)` options.  One of these is what I'm calling "naive" (although it's still some fairly sophisticated stats) because it ignores the complex survey nature of the data and just uses R's `glm` function as though we had a simple random sample.  The other method uses Lumley's `svyglm` to appropriately take the sampling and weighting into account.  The clustering will tend to increase standard errors (and hence the width of confidence intervals); the use of auxiliary information in weighting to match population totals for shape, colour and region will decrease them.  In this case, the net effect of treating the survey design with respect is to increase standard errors and the width of confidence intervals, as seen by the longer blue lines compared to the red lines.

Here's the code that creates that one sample and fits the two models to it, comparing the results to the "correct" values from fitting a model to the full population of 1 million people:

{% highlight R lineanchors %}
#===========modelling============
# First, we model the full population to get the "true" values (note that these are not
# really "true" as the model is somewhat mis-specified, but they are as true as can be done
# given the imperfection of the model, and hence form a good benchmark for the same model
# fit to a sample).  As we have the full population, we don't need to worry about survey methods
# and can just use stats::glm rather than survey::svyglm
full_pop_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = population)

#-------------compare a single example-------
# Survey design, for use with svyglm:
des <- samp(population, npsu = 5, seed = 999)
# Extract just the data from the design for use with glm:
data <- des$variables

samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = data)

samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des)


# Combine results for graphic:
res <- mutate(tidy(exp(confint(samp_model_bin))), model = "naive") %>%
  rbind(mutate(tidy(exp(confint(samp_model_bin_svy))), model = "survey"))  %>%
  rename(lower = X2.5..,
         upper = X97.5..,
         names = .rownames) %>%
  left_join(tidy(exp(coef(full_pop_model_bin)), by = "names")) %>%
  mutate(name_number = as.numeric(as.factor(names)),
         true_value = x,
         y_pos = ifelse(model == "naive", 
                        name_number + 0.1,
                        name_number - 0.1),
         names = gsub("colour", "Colour: ", names),
         names = gsub("region", "Region: ", names),
         names = gsub("shape", "Shape: ", names)) %>%
  arrange(model, name_number)

# Draw graphic:
res %>%
  ggplot() +
  geom_segment(aes(yend = y_pos, y = y_pos,
                   x = lower, xend = upper, colour = model), size = 1.7) +
  geom_point(aes (y = as.numeric(as.factor(names)), x = x), size = 2) +
  scale_y_continuous(labels = res[1:22, ]$names, breaks = 1:22) +
  labs(x = "Relative risk ratios",
       y = "Variable",
       colour = "Estimation method",
       caption = "Complex sample of around 2,100 drawn from a population of a million.  Black points show the true values") +
  ggtitle("Example confidence intervals for risk ratios from a quasi-binomial model",
          "Two modelling methods from the same sample, compared to true values from total population.")
{% endhighlight %}

That's just one sample of course.  How about if we do this 100 times?  That brings us to the graphic I started the blog with.  We can see that both methods are fair performers when it comes to point estimates, and indeed aren't too bad even for confidence interval coverage (I would be pleased and surprised if 85% of one's "95%" time series prediction intervals contained the true value, by comparison, but even the naive method exceeds that for these confidence intervals).  But the coverage of the survey method very nearly matches the advertised 95% design, which is excellent, particularly as the model is slightly mis-specified compared to the data generation process.  

<img src='/img/0132-comparisons.svg' width='100%'>

Finally, the question I thought this blog was going to be all about - quasi-poisson versus quasi-binomial families with a log link when using logistic regression to get relative risks?  There's an oldish but good thread on this on [R-help](http://grokbase.com/t/r/r-help/109dad6km5/r-relative-risk-regression-with-survey-data).  

My own experience so far with these two options is that they give similar results, but that the `quasipoisson(log)` method is less likely to fall over (ie fail to converge to a solution) than is `quasibinomial(log)`.  I don't know why this is.  `quasibinomial` will be modelling variance as proportional to `mu(1 - mu)` (where `mu` is the expected value of the response, given the linear predictor) whereas `quasipoisson` will be modelling it as proportional to `mu`.  In effect, this will mean the `quasipoisson` expects high variance when the probability of `y` is close to 1 and will down-weight those points; whereas `quasibinomial` will do the opposite.  In data like that I've been using today, the expected values don't get that high so it makes very little difference.  

In the simulations I've done for this blog, I got around the problem of the occasional non-convergence of the `quasibinomial` model by first fitting it with the `quasipoisson` option and then using the estimated coefficient values from that model as starting values for the `quasibinomial` model.  This seems to work well; we get the natural (or so it seems to me) treatment of variance as being proportional to `mu(1-mu)` of the binomial approach while still being robust enough to work without human intervention on a wide range of samples.

[As an aside, the use of *quasi* Poisson is particularly important if we are modelling binary outcomes as we are here.  With the response constrained to being 0 or 1, the variance will often be less (and nearly always different) from being *exactly* `mu` which would be expected for the straight Poisson family.  So we will have under-dispersion (or less likely, over-dispersion) relative to the Poisson distribution.  Use of `quasipoisson` allows this dispersion parameter to be estimated rather than forced onto the model.]

Here's a comparison of the confidence interval coverage of those two methods; we can see it's virtually identical in the long run, with this particular population and model:

<img src='/img/0132-bin-pois.svg' width='100%'>

So, to conclude (bearing in mind that this is only with one set of simulated population data, but the principles seem plausible):

- ignoring the complex survey nature of the data and just using `glm()` out of the box can still mean that you get ok point estimates
- however, your standard errors (and hence confidence interval widths, and statistical inferences in general) run the risk of understating the uncertainty and overstating the information, particularly when the original data is clustered
- there's not much to choose from in weighing up the use of `quasibinomial(log)` or `quasipoisson(log)` when setting out get get relative risks from a generalized linear model with a binary response.  They give similar results, and similarly successful confidence intervals.

Here's the last chunk of code - for those more systematic checks of "naive" versus survey methods, and quasipoisson versus quasibinomial:

{% highlight R lineanchors %}
#------------systematically compare lots of different samples' point estimates--------

compare_mse <- function(npsu, seed = NULL){
  des <- samp(population, npsu = npsu, seed = seed)
  data <- des$variables

  # The quasibinomial response isn't robust enough for either glm or svyglm, so we get some starting
  # values with quasipoisson to ensure we can always get a response:
  samp_model_pois_svy <-  svyglm(y ~ region + shape + colour, family = quasipoisson(log), design = des)
  
    
  samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = data,
                         start = coef(samp_model_pois_svy))

  samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des,
                                start = coef(samp_model_pois_svy))
  
  return(
    c(survey = mean((coef(samp_model_bin_svy)[-1] - coef(full_pop_model_bin)[-1]) ^ 2),
      naive = mean((coef(samp_model_bin)[-1] - coef(full_pop_model_bin)[-1]) ^ 2))
  )}

mses <- lapply(1:100, function(i){compare_mse(npsu = 5, seed = i)})
mses_df <- as.data.frame(do.call("rbind", mses))

mse <- round(apply(mses_df, 2, mean), 3)

p1 <- ggplot(mses_df, aes(x = naive, y = survey)) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  geom_point() +
  coord_equal() +
  ggtitle("Mean square error of coefficients",
          "Two methods of modelling, compared to correct (full\npopulation) values for all coefficients except the intercept.") +
  labs(x = paste0("Complex survey methods\nAverage mean squared error: ", mse[1]),
       y = paste0("Naive methods.\nAverage mean squared error: ", mse[2]))


#========compare confidence interval coverage========

#------------systematically compare lots of different samples' point estimates--------

ci_rate <- function(correct_mod, ci_mod){
  X <- as.data.frame(cbind(coef(correct_mod), confint(ci_mod)))
  names(X) <- c("correct", "lower", "upper")
  return(sum(with(X, correct > lower & correct < upper)) / nrow(X))
}

compare_ci <- function(npsu, seed = NULL){
  des <- samp(population, npsu = npsu, seed = seed)
  data <- des$variables
  
  # The quasibinomial response isn't robust enough for either glm or svyglm, so we get some starting
  # values with quasipoisson to ensure we can always get a response:
  samp_model_pois_svy <-  svyglm(y ~ region + shape + colour, family = quasipoisson(log), design = des)
  
  
  samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = data,
                         start = coef(samp_model_pois_svy))
  
  samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des,
                                start = coef(samp_model_pois_svy))
  
    
  return(
    c(binomial_svy = ci_rate(full_pop_model_bin, samp_model_bin_svy),
      poisson_svy = ci_rate(full_pop_model_bin, samp_model_pois_svy),
      naive = ci_rate(full_pop_model_bin, samp_model_bin))
  )}

cis <- lapply(1:100, function(i){compare_ci(npsu = 5, seed = i)})
cis_df <- as.data.frame(do.call("rbind", cis))

successes <- round(apply(cis_df, 2, mean) * 100)

p2 <- ggplot(cis_df, aes(x = binomial_svy, y = naive)) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  annotate("rect", xmin = -Inf, xmax = 1, ymin = 0.95, ymax = 1, fill = "steelblue", alpha = 0.5) +
  annotate("rect", xmin = 0.95, xmax = 1, ymin = -Inf, ymax = 0.95, fill = "steelblue", alpha = 0.5) +
  geom_jitter(width = 0.01, height = 0.01) +
  coord_equal() +
  ggtitle("Confidence intervals' coverage",
          "Two methods of modelling, showing proportion that\ncontain the correct value (should be 95%).") +
  labs(x = paste0("Complex survey methods.\nAverage coverage: ", successes[1], "%"),
       y = paste0("Naive methods.\nAverage coverage: ", successes[3], "%"),
       caption = str_wrap("Source for both diagrams: complex samples of around 2,100 each, drawn from a 
population of 1 million stratified by 14 unequal regions with 
5 out of 100 potential primary sampling units chosen per region.  Model has three categorical explanatory variables and a single binary (quasi-binomial family) response.
Points have been jittered.", 90)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)

grid.arrange(p1, p2, ncol = 2)

ggplot(cis_df, aes(x = binomial_svy, y = poisson_svy)) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  annotate("rect", xmin = -Inf, xmax = 1, ymin = 0.95, ymax = 1, fill = "steelblue", alpha = 0.5) +
  annotate("rect", xmin = 0.95, xmax = 1, ymin = -Inf, ymax = 0.95, fill = "steelblue", alpha = 0.5) +
  geom_jitter(width = 0.01, height = 0.01) +
  coord_equal() +
  ggtitle("Confidence intervals' coverage",
          "Two methods of modelling, showing proportion that\ncontain the correct value (should be 95%).") +
  labs(x = paste0("Quasi-binomial family\nAverage coverage: ", successes[1], "%"),
       y = paste0("Quasi-poisson family\nAverage coverage: ", successes[2], "%"),
       caption = str_wrap("Source: complex samples of around 2,100 each, drawn from a 
population of 1 million stratified by 14 unequal regions with 
5 out of 100 potential primary sampling units chosen per region.  Model has three categorical 
explanatory variables and a single binary response from either the quasi-binomial or quasi-poisson family.
Points have been jittered.", 90)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)
{% endhighlight %}


Finally and slightly off the point, for a great example of the importance of reporting *absolute* risks as well as relative risks, check out [this blog](https://medium.com/wintoncentre/the-risks-of-alcohol-again-2ae8cb006a4a) on the recent Lancet article arguing "no safe level of alcohol usage".  The statistics in the original article look to be sound, but while the relative risk of any alcohol is indeed above 1, the actual increase in absolute risk is small.   For example:

> "So a total of 50,000 bottles of gin among these 1,600 people is associated with one extra health problem."

Read the full blog for more.
