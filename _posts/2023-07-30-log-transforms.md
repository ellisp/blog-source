---
layout: post
title: Log transforms, geometric means and estimating population totals
date: 2023-07-30
tag: 
   - Transformations
   - Tools
description: A model that is 'improved' (in terms of making standard assumptions more plausible) by using a logarithm transform of the response will not necessarily be improved for estimating population totals.
image: /img/0204-model-comparison-density.svg
socialimage: https:/freerangestats.info/img/0204-model-comparison-density.png
category: R
---

## Motivation

I usually talk about statistical inference as having one of three purposes:

- estimating some finite and defined population parameter, like the mean income of mechanical apprentices in 1890s London or the number of 5-9 year olds living in Melbourne. Typically the results could in principle be measured directly by a census and the statistical modelling and inference comes in as a way to deal with sampling, measurement, and other sources of error.
- predicting or forecasting some value. This might be a straight out forecast ("80% chance of between 3 and 6mm of rain tomorrow") or some kind of hypothetical scenario modelling ("if you do A, you will probably see X happen; but if you do B you are more likely to get Y")
- generating insight into some underlying physical or social process ("women are paid less than men because of systemic sexism, not their ability to do the job")

Of course a bit of probing shows that the categories aren't as clear cut as I'm making out. For example, estimating a population parameter for an official statistic will often be more effective if we have a good model of the underlying processes; and we would want to test that model by making predictions and forecasts if we can. One epistemological school of thought insists that all three methods are actually variants on predictive inference.

But I find it useful schema to think through why I am ever doing some particular bit of inference.

Today's blog is about a situation where it particularly helps to think through *why* we're doing our inference. Because we're trying to estimate a population total we need to make some different modelling choices from what we'd do if we were after understanding and describing an underlying process.

Specifically, it's also how we need to not just reflexively do a log transform on the data even when it clearly helps the assumptions justifying ordinary least squares estimation look better. If we're just after an unbiased population estimate, it might be simpler and less risky to use the original scale of the data.

## Estimating average diamond price

### Exploratory data-analysis

Imagine we own (or know someone who owns...) the 53,940 diamonds described in the `diamonds` dataset that comes built into the `ggplot2` R package (or has it been moved now? either way, everyone has it I'm sure). Unfortunately our copy of `ggplot2` is corrupted so we don't have the price column in the data. Pricing diamonds is time-consuming and expensive so we can only do it for a sample of 1,000 of the diamonds. Our task - a classic piece of statistical inference - is to estimate the total value on the market of our diamonds collection from that sample - or equivalently, the mean price which we can multiply by 53,940 to get the total.

We take our sample of 1,000 diamonds and plot the statistical density of price. This gives us this right-skewed distribution, similar to what we'd see with many economic and financial variables.

<object type="image/svg+xml" data='/img/0204-distribution.svg' width='100%'><img src='/img/0204-distribution.png' width='100%'></object>

The right skew is an important part of what follows because it is both part of the reason for doing a log transform in a common model, and the reason why it is problematic for our main inference purpose.

That plot was produced with this code

{% highlight R lineanchors %}
library(tidyverse)
library(patchwork)
library(scales)

the_caption = "Source: example analysis of the diamonds dataset in the ggplot2 R package"

set.seed(321)
samp <- sample_n(diamonds, 1000)

ggplot(samp, aes(x = price)) +
  geom_density(fill = "steelblue", alpha = 0.5, colour = NA) +
  geom_rug() +
  labs(title = "Distribution of diamond price is skewed rightwards",
       subtitle = "(Like many economic and financial variables)",
       caption = the_caption,
       x = "Price of an individual diamond") +
  scale_x_continuous(label = dollar)
{% endhighlight %}

We've got the information on weight (in carats) and colour on our whole population of diamonds; it's only price that's missing. It makes a lot of sense to use this to create a model of price using our sample diamonds and apply that model to the full dataset to get a set of predicted prices. 

Following the methods taught in any statistics course introducing regression modelling, we start with a scatter plot of weight on the horizontal axis and price on the vertical axis.

<object type="image/svg+xml" data='/img/0204-untransformed.svg' width='100%'><img src='/img/0204-untransformed.png' width='100%'></object>

I've actually drawn a likely model in place here. Anyone who paid attention in that statistics course will recognise that the data set suffers from heteroskedasticity. As noted in the plot's subtitle this breaches the Gauss-Markov assumptions, one of which is that variance is constant regardless of the expected value of the response variable. In this case - as is the case for many economic variables - the variance increases as the expected price increases, causing a familiar characteristic fanning out of the data. This means we can't rely on Gauss-Markov to show that ordinary least squares (OLS) is the best linear unbiased estimator. In fact, we can be confident that OLS *isn't*, and that even if we kept the same structure of our model we would be better off with some kind of weighted least squares estimator.

What you do here will depend on exactly what kind of statistics course taught you regression. Some people would reach for the weighted-least squares estimator. Others like me will instinctively say "there's no particular reason for the relationship between weight and price to be linear in the original scale. Perhaps the marginal return for an extra carat of weight changes as we go along. If we take logarithms of both weight and price we might get something that is at least as good a model, and is likely to deal with the heteroscedasticity as a fortunate side effect".

If you were trained in this school of thought you are likely to replot the data with transformed axes, and fit a different model that is linear in the logs. Illustrated by this:

<object type="image/svg+xml" data='/img/0204-transformed.svg' width='100%'><img src='/img/0204-transformed.png' width='100%'></object>

Boom, the heteroscedasticity is gone; and not only that the lines for each colour are nearly parallel. For various reasons, some philosophical and some more aesthetic, I am actually pretty sure this "linear in the logs" model is a better model for the purpose of understanding the relationship between price and weight. 

Those charts were produced with this chunk of code.

{% highlight R lineanchors %}
ggplot(samp, aes(y = price, x = carat, colour = color)) +
  geom_smooth(method = "lm") +
  geom_point()  +
  scale_y_continuous(label = dollar_format(accuracy = 1))+
  labs(title = "A linear model of the untransformed price has heteroskedasticity challenges",
       subtitle = "Model of form price ~ carat * cut.\nResiduals are right-skewed and variance increases as expected price increases, 
breaking the Gauss-Markov assumptions.",
       caption = the_caption, 
       x = "Weight of the diamond (carats)",
       y = "Price of an individual diamond")

ggplot(samp, aes(y = price, x = carat, colour = color)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10(label = dollar_format(accuracy = 1))  +
  geom_smooth(method = "lm") +
  labs(title = "A linear model after log transforms is better from a model-building perspective",
       subtitle = "Model of form log(price) ~ log(carat) + cut.\nStandard assumptions justifying ordinary least squares make more sense after transformation.",
       caption = the_caption, 
       x = "Weight of the diamond (carats)",
       y = "Price of an individual diamond")
{% endhighlight %}

### Fitting a model and choosing transformation based on diagnostics

The conviction that the log transform is a good one is reinforced when we look at the diagnostic plots of the two models (or rather, of two similar models - in the charts below I have simplified the models to remove an interaction between colour and weight).  Here are the four most common diagnostic plots based on the residuals of the untransformed linear model:

<object type="image/svg+xml" data='/img/0204-diagnostics-untransformed.svg' width='100%'><img src='/img/0204-diagnostics-untransformed.png' width='100%'></object>

The code for fitting the model and drawing these diagnostics is going to come a bit later

If you were trained the way I was, you see plenty of red flags here suggesting the linear model isn't a great fit, the residuals don't have the homoskedasticity needed to guarantee ordinary least squares being the best estimator, and the data isn't normal which will cause a problem for conventional statistical tests: 
- the 'residuals versus fitted' plot in the top left shows clear structure in the residuals, not the white noise we hope for. They curve up at the left (so some non-linearity going on), plus they fan out to the right (heteroskedasticity)
- the absolute scale of the residuals definitely increases as the expeted value increases (scale-location plot on bottom left), a definite indicator of heteroskedasticity
- the normal quantile-quantile plot strongly indicates the residuals aren't normal, but has fat tails (e.g. when they theoretically would be about 3 on the standardised scale, they are about 5 - much higher) 

Quick digression - in case you want a visual key of how things look in a quantile-quantile plot, here's one I knocked up earlier and might as well have here as anywhere. The thing we are looking for in a Q-Q plot is that if the observed data is similar to the reference distribution (in this case a normal distribution), we'd see a basically straight line.
<object type="image/svg+xml" data='/img/0204-qqnorm-demo.svg' width='100%'><img src='/img/0204-qqnorm-demo.png' width='100%'></object>

Produced with this quick excursion into base graphics:
{% highlight R lineanchors %}
x <- list()
n <- 300
x[[1]] <- rnorm(n)
x[[2]] <- exp(rnorm(n))
x[[3]] <- -exp(rnorm(n))

par(mfrow = c(2,3), bty = "l", family = "Roboto")

qqnorm(x[[1]], main = "Normal")
qqnorm(x[[2]], main = "Right-skewed")
qqnorm(x[[3]], main = "Left-skewed")
lapply(x, function(x){plot(density(x), main = "")})
{% endhighlight %}

OK, back to our diamonds; so we have fat-tailed, heteroskedastic residuals, a right-skewed original variable, and a bit of a curve in the data. We reach for our log-transform and refit the model and get this *much* better set of diagnostic plots:

<object type="image/svg+xml" data='/img/0204-diagnostics-transformed.svg' width='100%'><img src='/img/0204-diagnostics-transformed.png' width='100%'></object>

The issues of concern are fixed:
- the curved shape and the fanning in the residuals-v-fitted plot has gone and we're left with something looking much more like white noise
- the scale-location plot looks like solid homoskedasticity
- The normal quantile-quantile plot looks a lot more "normal" (ie straight line) and apart from a few outliers the values of the standardised residuals are what you'd expect them to be for a normal distribution

Conventionally, this looks way better. And if we wanted a model to understand the relationship of diamonds' price to their weight and colour, I would say this was a better model.

### Using a model to predict total price

But understanding that isn't the primary purpose of the exercise. We want the total expected price of the actual collection of 53,490 diamonds based on the model we've fit to 1,000 of them. So I'm going to compare three ways of doing this

- 'Model with no transformations' - this is the first model described above, the one with diagnostic plots suggesting heteroskedasticity
- 'Model with log transformation' - this is the second model described above, where I have taken the logarithm of price and of weight in carats before using ordinary least squares again for parameter estimates
- 'Simple average' - I just take the average price of my sample of 1,000 and extrapolate that to the full population.

The 'simple average' approach doesn't use the information we have on the full population's weight and colour at all, so we expect it to be a lot less efficient, but it should still be unbiased. Formally it's equivalent to the 'Model with no transformations' but with the regression coefficients other than the intercept all constrained to be zero.

For each of the three approaches I take the model that has been fit on my sample and apply it to the full population of 53,490 to estimate each diamond's price, and add them up to get the average price I can expect for *all* my diamonds (remembering that's the point of the exercise). Then, I'm looking at the answers, and comparing that result to the true average price. Then I do this again with a different sample of 1,000 diamonds; and plot the overall results:

<object type="image/svg+xml" data='/img/0204-model-comparison-density.svg' width='100%'><img src='/img/0204-model-comparison-density.png' width='100%'></object>

Well, I sort of gave this away at the beginning; but the model with the *untransformed* price variable in the sample performs much better. Basically because it's unbiased. All that heteroskedasticity and non-normality and a bit of curve doesn't matter; if fixing them means you bias the total result substantially downwards.

Note that taking the simple average of the sample is of course also unbiased like the untransformed model, but as expected is inefficient and has a much wider sampling error than either of the models which make better use of the auxiliary information available.

Here's a scatter plot of those results. Each black point is one of the estimates from one of these estimation methods (multiple methods fit to a given sample of 1,000 diamonds) and the red point is the true value. 
- The chart on the left compares the simple average (of an untransformed price variable) to the model also with the untransformed price variable. Both methods on average get the right result, but the 'simple average' method has lots of points far too low or far too high (which is why the aspect ratio of that plot is the way it is)
- The chart on the right compares the two models, with untransformed and transformed price variables. We see that given the same sample of 1,000 diamonds, one method is much better than the other.

<object type="image/svg+xml" data='/img/0204-model-comparison-scatter.svg' width='100%'><img src='/img/0204-model-comparison-scatter.png' width='100%'></object>

Of course as soon as you think about what's happening here it's obvious. With the log-transformed price variable, when I predict new values for the out-of-sample diamonds on the log scale, those are unbiased estimates *of the logarithm of the unobserved diamonds' prices*. When I say e to the power of those estimates, I get biased estimates of the price on their original scale. It's actually exactly as though I took the geometric mean of the sample - which in a right-skewed variable will be on average lower than the arithmetic mean - and tried to scale it up to the populatio level just by multiplying it by N/n.

Of course you could implement some fancy method to adjust for that bias, but I would say why bother? Look at the nice properties of the predictions from the model with no transformations. It turns out that a bit of curvature and non-normal non-heteroskedastic residuals may not matter as much as one might think - if your model is for a very particular, constrained purpose.

So it seems obvious - but twice now, ten years apart, I've found myself nearly making this mistake in important circumstances. I had a sample, and needed to estimate a population total for which I had auxiliary information. I fit a model, which was obviously greatly improved (in terms of removing heteroskedasticity and non-normality of residuals) by taking log transforms. And I reflexively did this, and used the model to predict population totals, before (luckily) something at the back of my mind warned me of pretty much the content of this blog, before I went and made a fool of myself (or just as likely the mistake wouldn't have been found - peer review is never as good as it should be within government and consulting, which were the two environments here - so we would have just blithely under estimated the variable of interest and never known). And this happened exactly the same way, twice (one example was international visitor spend in New Zealand, the other was revenue of space companies in Australia).

So we always found the mistake and did it right and the last time was a few years ago. But I'm writing this in the hope I don't head down the wrong path again in another 10 years or so.

Here's the code that did all that sampling, model fitting and population estimation. I did this with 1,000 samples of 1,000 diamonds each. The sets of diagnostic plots shown earlier are from just the last version of `mod1` and `mod2` fit to the last of those 1,000 samples (which is why I've delayed the code for them down to here).

{% highlight R lineanchors %}
set.seed(123)
reps <- 1000
results <- tibble(
  log_trans = numeric(reps),
  untransformed = numeric(reps),
  naive = numeric(reps)
)

for(i in 1:reps){
  samp <- sample_n(diamonds, 1000)
  
  mod1 <- lm(log(price) ~ log(carat) + color, data = samp)
  preds1 <- predict(mod1, newdata = diamonds)
  
  mod2 <- lm(price ~ carat + color, data = samp)
  preds2 <- predict(mod2, newdata = diamonds)
  
  mod3 <- mean(samp$price)
  
  results[i, ] <- cbind(mean(exp(preds1)), mean(preds2), mod3)
}

# Plot the standard diagnostics of the two models for the last example

p7 <- function(){
  par(mfrow = c(2, 2), bty = "l")
  plot(mod1)
  } # conventionally good

p8 <- function(){
  par(mfrow = c(2, 2), bty = "l")
  plot(mod2)
  } # unsatisfactory - residuals with curvature, non-normal, and increase as values increase

true_value <- mean(diamonds$price)

p4 <- ggplot(results, aes(x = naive, y = untransformed)) +
  geom_point() +
  coord_equal() +
  annotate("point", x = true_value, y = true_value, size = 3, colour = "red") +
  annotate("label", x = true_value, y = true_value - 10, 
           label = "True value", hjust = 1, size = 3, colour = "red") +
  labs(x = "Based on simple average from sample",
       y = "Based on untransformed model",
       subtitle = "Simple average leads to higher variance than model-based estimate",
       title = "Comparison of different estimation methods from a simple random sample")

p5 <- ggplot(results, aes(x = log_trans, y = untransformed)) +
  geom_point() +
  coord_equal() +
  annotate("point", x = true_value, y = true_value, size = 3, colour = "red") +
  annotate("text", x = true_value, y = true_value - 10, 
           label = "True value", hjust = 1, size = 3, colour = "red") +
  labs(x = "Based on log-log model",
       y = "Based on untransformed model",
       subtitle = "Log-log model leads to underestimates")


p6 <- results %>%
  gather(variable, value) %>%
  mutate(variable = case_when(
    variable == "log_trans" ~ "Model with log transformation",
    variable == "naive" ~ "Simple average of sample",
    variable == "untransformed" ~ "Model with no transformation"
  )) %>%
  ggplot(aes(x = value, colour = variable, fill = variable)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = true_value, colour = "red") +
  annotate("text", x = true_value, y = 0.0005, 
           label = "True value", hjust = 1, vjust = 0, size = 3, colour = "red") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparison of different estimation methods from a simple random sample",
    x = "Estimated value",
    colour = "",
    fill = "",
    subtitle = "log-log model is biased low; simple average has higher variance; model with no transformation is best.")
{% endhighlight %}

Now, if was doing this for real, I probably wouldn't stop here. Because I'm a bit worried about the heteroskedasticity, even though I don't want the complications that come from taking a log transformation of price to make it go away. Ordinary least squares isn't the best estimator when the residuals have variance that varies with the expected value of price (in this case, as price goes up the variance of the residuals go up). I'd probably want to down-weight the observations with higher expected values. An elegant way to do this is to move from ordinary least squares to a generalized linear model with an identity link function, normal family response, but a variance function that increases as the mean does (rather than staying constant). But I actually don't think it would make that much difference, and becomes a different point for this blog anyway.

And I have other stuff to do so OK, that's all for now.

