---
layout: post
title: Pragmatic prediction intervals from a quasi-likelihood GLM 
date: 2020-04-18
tag: 
   - ModellingStrategy
   - Transformations
   - R
description: A pragmatic way of generating prediction intervals from a generalized linear model with a quasi-likelihood response, if you're prepared to make an additional assumption about the distribution of the response.
image: /img/0177-quasi-glm.svg
socialimage: http://freerangestats.info/img/0177-quasi-glm.png
category: R
---

Today's blog comes with two lessons: a statistical one, and one on troubleshooting. 

## Rubber-duck debugging

Troubleshooting lesson first. The problem described below has been causing me grief for several days. I have an important and time-critical use case at work where I need to impute lots of complex missing data using the sort of model I'm about to describe. But, due to tiredness causing me to not think straight, and the problem being embedded in a much larger set of complex computer programs, I was struggling to get the answer. After a while I correctly realised I needed to abstract the problem from the use case and make an example with simulated data to get my thinking straight. 

After *that* didn't work despite an embarrassing couple of hours of effort, come the weekend I decided to take the awesome responsibility of writing a new question on [Cross-Validated](https://stats.stackexchange.com/). I was reluctant to do this because I am *clearly* meant to know the answer to this problem (outlined below) so it seems an admission of professional failure to have to ask publicly. Only because I was genuinely baffled would I go this far; it seemed clear that what I thought I understood about generalized linear models as implemented in R was wrong in some important respect.

I spent about 30 minutes carefully preparing a minimal reproducible example, explaining clearly what I needed to do and had tried, what my thinking was and why that didn't seem to work, meticulously re-reading all the documentation. Which made me think, "oh, perhaps it means this...". Without much hope (for this was about the third time in the process, over several days, I had suddenly thought I saw the light) I made an example to show that I had tried this and why it didn't work. But (who would have seen this coming) it *did* work.

So never under-estimate the power of [rubber duck debugging](https://en.wikipedia.org/wiki/Rubber_duck_debugging). That is, the troubleshooting super-power that is writing a reproducible example and carefully explaining exactly what your problem is in a way that you think someone else can answer it for you. If you explain it clearly enough, that someone else is just as likely to be you.

I actually own four rubber ducks expressly for the purpose of helping me solve stubborn problems that should "surely" be in my own resources to solve - one for each of my work stations at home, one for work (now sitting as a spare at home), and a fourth just for travel. But I hadn't followed my own advice of explaining the problem to one of the ducks. So, lesson learned.

## Prediction intervals for variables with exponential relationships and increasing variance 

On to the substantive statistical problem. I have data that when log-transformed, has a linear relationship to other data. And my main variable clearly has increasing variance as its expected value increased. In my real world example, I have many of these variables and the motivation is to impute missing values.

I want to model the data in a way that doesn't throw away individual negative values, so taking a log transformation is sub-optimal. Instead, I want to use a generalized linear model with logarithm link function, which means the expected value of the response will never go below zero but individual values can (the original data includes financial variables which closely follow this pattern). I need the variance of the response variable to increase with the expected value though, so I can't use `family = gaussian(link = log)`, which assumes constant variance. 

The individual-level variance of the response is crucial because I am going to use the model to simulate individual values in a home-made implementation of [multiple imputation with chained equations](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3074241/) (for various complicated reasons, I can't just use the `mice` package, which would have been my preference, and it's simpler to hand-roll my own system). I'm prepared to pretend the data is normally distributed (conditional on an expected value that changes with the other variables in the system) so long as I have variance increasing proportional to the mean. So an important end result of all this is a vector that I can use for the `sd = ` argument of `rnorm()`.

The two modelling strategies under discussion are 

- to log-transform the y and use ordinary least squares; or 
- a quasi-likelihood GLM with a log link function and variance proportionate to the mean of the response. (As discussed above, this is the purpose I want to use. Down the track I might want to instead use "to the square of the mean of the response" instead, but that's not important for illustrative purposes.)

These strategies give similar coefficient estimates. Let's look at some simulated data (held over from my aborted question to Cross-Validated)

{% highlight R lineanchors %}
library(tidyverse)
library(scales)

n <- 50
set.seed(123)

d <- tibble(x = rnorm(n, 4, 1)) %>%
  mutate(mean = 2 + 0.25 * x,
		 sd = sqrt(mean) / 20,
		 y = exp(mean + rnorm(n, 0, sd))
  )

mod1 <- lm(log(y) ~ x, data = d)
mod2 <- glm(y ~ x, family = quasi(link = log, variance = mu), data = d)

# similar coefficients (one is minimising squares on the log scale, 
# the other is minimising weighted squares on the original)
rbind(coef(mod1), coef(mod2))
{% endhighlight %}

Which gives us:

```         
         (Intercept)         x
    [1,]    2.021225 0.2478562
    [2,]    2.016842 0.2496386
```

So just as a curve-fitting exercise, both methods are pretty good at recovering the original true values of 2 and 0.25 for the intercept and slope respectively in the linear predictor of my data generating process.

A prediction interval differs from a confidence interval in that it includes individual-level randomness *in addition to* the uncertainty in parameter estimates of the model. For example, `geom_smooth()` draws a confidence interval, expressing confidence/uncertainty in the position and shape of the line of best fit, but not the range where 95% of new values might be expected to turn up.

### Prediction interval with transformation and ordinary least squares

Creating a prediction interval from the log-transformed OLS model is a standard Regression 101 exercise. The variance of the estimate for any individual is the sum of the variance of our estimated curve at that point and the residual variance of individuals. Take the square root and you get the standard deviation; assume normality and multiply by 1.96 to get a 95% prediction interval; back transform to the original scale (note that there are bias-adjustment complications we could worry about here but will ignore for today's purposes).

<img src='/img/0177-log-transform.svg' width='100%' align='right'>

... generated with this code:

{% highlight R lineanchors %}
#------------Prediction interval from the log transform version--------------------
sx <- tibble(x = seq(from = 1, to = 7, length.out = 100)) 
pred1 <- predict(mod1, se.fit = TRUE, newdata = sx)

sx1 <- sx %>%
  # calculate standard deviation for the prediction interval and create lower and upper bounds:
  mutate(se_pi = sqrt(pred1$se.fit ^ 2 + pred1$residual.scale ^ 2),
         lower_pi = exp(pred1$fit  - 1.96 * se_pi),
         upper_pi =exp(pred1$fit +  1.96 * se_pi))

p1 <- ggplot(d, aes(x = x)) +
  geom_ribbon(data = sx1, aes(ymin= lower_pi, ymax = upper_pi), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y))+
  labs(title = "Original data with prediction interval from log transform model")
{% endhighlight %}

### Prediction interval with GLM and a log link function

To do something similar with the generalized linear model with the quasi likelihood response, we need to understand the `residual.scale` argument returned by `predict.glm()`. Whether we use `predict(..., type = "response")` or `predict(..., type = "link")`, the `residual.scale` value is the same (0.361 in our case). But 0.361 of what?

`residual.scale` is described in the help file as "a scalar giving the square root of the dispersion used in computing the standard errors". How do we use this to make a prediction interval? In our specification of `mod2`, I told `glm` that the variance of y was going to be proportional to its mean (`family = quasi(link = log, variance = mu)`). We can recover the estimated variance at an individual point by the predicted value of y multiplied by the dispersion factor. So this means that in this particular case we can generate the prediction interval as follows. The key part for our discussion is `+ pred2$fit * pred2$residual.scale ^ 2)` - because `pred2$fit` is the mean of y at the given point, and `pred2$residual_scale ^ 2` is the dispersion factor that scales the mean to become its variance.

{% highlight R lineanchors %}
#-----------------prediction interval from the quasi family GLM------------

pred2 <- predict(mod2, se.fit = TRUE, newdata = sx, type = "response")
sx2 <- sx %>%
  mutate(se_pi = sqrt(pred2$se.fit ^ 2 + pred2$fit * pred2$residual.scale ^ 2),
         lower_pi = pred2$fit - 1.96 * se_pi,
         upper_pi = pred2$fit + 1.96 * se_pi)

ggplot(d, aes(x = x)) +
  geom_ribbon(data = sx2, aes(ymin= lower_pi, ymax = upper_pi), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y))+
  labs(title = "Original data with prediction interval from quasi likelihood glm with log link")
{% endhighlight %}

And here we go:

<img src='/img/0177-quasi-glm.svg' width='100%' align='right'>

Seems simple once you know.

There are subtle but important differences between the prediction intervals from our two methods. Because I wrote the original data generating process, I know that the second one is closer to the "real" model. The log transform model actually increases the variance too quickly as the mean increases, compared to the true process. For higher values of y, the prediction interval is wider than it needs to be. Of course, whether this will be the case with real data will depend on the particular situation.

Note that this method won't work for all GLMs. In particular, anything with a non-continuous response would definitely need another think. And to use those `+/- 1.96` values for the prediction interval, I've relied on being prepared to make a simplifying assumption about the normal distribution of response variable for the purposes of the prediction interval, which I hadn't made in the original estimation of the quasi-likelihood model. There's no central limit theorem or anything to rely on here, because these are *individual* values of y, and they'll have whatever shape y usually has.

What matters most here is understanding how that `residual.scale` disperson estimate works, in relationship to the specification of variance in the original call to `quasi()`. This relationship to the variance *doesn't* depend on any additional normality assumption.

Anyway, use this approach with caution. Obviously, if I *really* were prepared to say that y is normally distributed (but with variance increasing proportional to the mean), I could model it explicitly as such rather than mucking around with quasi-likelihoods. So what's outlined here is very much about pragmatism.


