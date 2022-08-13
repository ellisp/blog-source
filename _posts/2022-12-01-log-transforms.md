---
layout: post
title: Log transforms, geometric means and estimating population totals
date: 2022-12-01
tag: 
   - Transformations
   - Tools
description: A model that is conventionally improved by using log transforms will probably not be the best to use for estimating population totals.
image: /img/0204-model-comparison-scatter.svg
socialimage: http://freerangestats.info/img/0204-model-comparison-scatter.png
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



## Estimating average diamond price

Imagine we own (or know someone who owns...) the 53,940 diamonds described in the `diamonds` dataset that comes built into the `ggplot2` R package. Unfortunately our copy of `ggplot2` is corrupted so we don't have the price column in the data. Pricing diamonds is time-consuming and expensive so we can only do it for a sample of 1,000 of the diamonds. Our task - a classic piece of statistical inference - is to estimate the total value on the market of our diamonds collection from that sample - or equivalently, the mean price which we can multiply by 53,940 to get the total.

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

Say we've got the information on weight (in carats) and colour on our whole population of diamonds. It makes a lot of sense to use this to create a model of price using our sample diamonds and apply that model to the full dataset to get a set of predicted prices. 

Following the methods taught in any statistics course introducing regression modelling, we start with a scatter plot of weight on the horizontal axis and price on the vertical axis.

<object type="image/svg+xml" data='/img/0204-untransformed.svg' width='100%'><img src='/img/0204-untransformed.png' width='100%'></object>

I've actually drawn a likely model in place here. Anyone who paid attention in that statistics course will recognise that the data set suffers from heteroskedasticity. As noted in the plot's subtitle this breaches the Gauss-Markov assumptions, one of which is that variance is constant regardless of the expected value of the response variable. In this case - as is the case for many economic variables - the variance increases as the expected price increases, causing a familiar characteristic fanning out of the data. This means we can't rely on Gauss-Markov to show that ordinary least squares (OLS) is the best linear unbiased estimator. In fact, we can be confident that OLS *isn't*, and that even if we kept the same structure of our model we would be better off with some kind of weighted least squares estimator.

What you do here will depend on exactly what kind of statistics course taught you regression. Some people would reach for the weighted-least squares estimator and be done with it. Others like me will instinctively say "there's no particular reason for the relationship between weight and price to be linear in the original scale. Perhaps the marginal return for an extra carat of weight changes as we go along. If we take logarithms of both weight and price we might get something that is at least as good a model, and is likely to deal with the heteroscedasticity as a fortunate side effect".

If you were trained in this school of thought you are likely to replot the data with transformed axes, and fit a different model that is linear in the logs. Illustrated by this:

<object type="image/svg+xml" data='/img/0204-transformed.svg' width='100%'><img src='/img/0204-transformed.png' width='100%'></object>

Boom, the heteroscedasticity is gone; and not only that the lines for each colour are parallel. For various reasons, some philosophical and some more aesthetic, I am actually pretty sure this "linear in the logs" model is a better model for the purpose of understanding the relationship between price and weight. 

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

The conviction that the log transform is a good one is reinforced when we look at the diagnostic plots of the two models (or rather, of two similar models - in the charts below I have simplified the models to remove an interaction between colour and weight).  Here are the four most common diagnostic plots based on the residuals of the untransformed linear model:

<object type="image/svg+xml" data='/img/0204-diagnostics-untransformed.svg' width='100%'><img src='/img/0204-diagnostics-untransformed.png' width='100%'></object>

The trained eye picks up plenty of red flags here.


<object type="image/svg+xml" data='/img/0204-diagnostics-transformed.svg' width='100%'><img src='/img/0204-diagnostics-transformed.png' width='100%'></object>




{% highlight R lineanchors %}



{% endhighlight %}

<object type="image/svg+xml" data='/img/0204-model-comparison-scatter.svg' width='100%'><img src='/img/0204-model-comparison-scatter.png' width='100%'></object>


{% highlight R lineanchors %}



{% endhighlight %}




