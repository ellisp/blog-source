---
layout: post
title: Sample size needed for central limit theorem to kick in
date: 2026-09-16
tag: 
   - Simulations
   - Distributions
description: It takes a larger sample size than you might hope for the mean of a sample to have a distribution that's close enough to 'normal' for common inference methods to work.
image: /img/0332-ad-stat.svg
socialimage: https:/freerangestats.info/img/0332-ad-stat.png
category: R
---
Frank Harrell made this [enlightening comment on BlueSky](https://bsky.app/profile/f2harrell.bsky.social/post/3mvdhe2ky522h) the other day:

> "At the heart of the failure of the CLT (central limit theorem) is its need for the mean and standard deviation to be independent.  With asymmetric distributions they are far from independent.  N=50,000 may be far too small for the CLT to work well enough."

Like [some other people](https://bsky.app/profile/stephenjwild.bsky.social/post/3mvdirlory22n), it immediately occurred to me that wow, 50,000 is quite a bit more than 30. I was aware of course that the central limit theorem can need fairly large sample sizes for means from skewed distributions to become "approximately normal", for some pragmatic definition of "approximately". But I hadn't been thinking of quite that large. So I did some simulations to help stamp this impression on my mind.

I started with building this `sim_clt()` function in R. Well actually, it started a lot simpler than this, but here is the final version that lets me do several things I want to with these simulations.

The core task of this function is to take a population (generated separately), draw a sample of a specified size from that population, and calculate the sample mean. Then to do this many times (default 10,000) to examine the distribution of those means. Outputs include a qq-normal plot; the [Anderson-Darling statistic](https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test) generated as though to be used for a test of normality (the higher its value in this case, the less normal the distribution of the sample mean); and the proportion of confidence intervals, constructed assuming normality of the sample mean (i.e. the core claim of the central limit theorem), that actually do contain the true mean.

{% highlight R lineanchors %}
library(nortest)
library(tidyverse)
library(actuar)
library(glue)
library(scales)

set.seed(123)

today_reps <- 10000
N <- 1e6

sim_clt <- function(
  x,
  n = 30,
  reps = today_reps,
  replace = TRUE,
  plot = TRUE,
  conf = 0.95,
  ...
) {
  true_mean <- mean(x)

  samples <- replicate(
    reps,
    sample(x = x, size = n, replace = replace),
    simplify = FALSE
  )
  means <- sapply(samples, mean)

  covered <- sapply(samples, function(s) {
    # rely on asymptotic normality to estimate a confidence interval and check
    # for coverage for each sample
    se <- stats::sd(s) / sqrt(length(s))
    ci <- mean(s) + c(-1, 1) * qnorm((1 - conf) / 2 + conf) * se
    ci[1] <= true_mean & true_mean <= ci[2]
  })

  if (plot) {
    qqnorm(means, bty = "l", ...)
    qqline(means, col = "steelblue")
    grid()
  }

  return(list(
    ad_stat = nortest::ad.test(means)$statistic,
    coverage = mean(covered)
  ))
}
{% endhighlight %}

Usage of the function is something like this. In this line of code, I create 10,000 simulated samples from a population of a million points from a standard normal distribution. Each sample is only 5 in size:

{% highlight R lineanchors %}
sim_clt(rnorm(N), n = 5, main = "Mean from normal dist, n = 5")
{% endhighlight %}

That gives me the info that the Anderson Darling statistic of the 10,000 resulting sample means is 0.45, that a 95% confidence interval created with a normal distribution and using the sample standard deviaion contains the true mean 87% of the time, and this QQ-normal graphic:

<object type="image/svg+xml" data='/img/0332-qqplot1.svg' width='100%'><img src='/img/0332-qqplot1.png' width='100%'></object>

I'm quite deliberately using a normal distribution for my sample mean statistic in producing that confidence interval, rather than a t distribution. The t distribution would be appropriate in this particular instance (where the population is known to be normally distributed) and would give a wider confidence interval with coverage much closer to 95%. But I'm pretending to have to rely on asymptotic normality instead. My aim is to test how good (in pragmatic terms) is the assumption that the statistic itself (ie the sample mean) is normally distributed because of the central limit theorem. Inference using the sample variance as an estimate of the population variance will work in constructing a standard error as the sample size gets bigger; using a t distribution when we happen to know it applies would be cheating.

That's why the coverage is only 87%,not the 95% we'd get if we'd constructed a confidence interval with a t distribution.

One thing we do see from the QQ-normal plot above is that the straight line tells us the distribution of the sample mean, even from this tiny sample of 5, is normally distributed. Of course we know this theoretically. It turns out to be quite different with a sample size of 5 from a skewed, log-normal distribution:

<object type="image/svg+xml" data='/img/0332-qqplot2.svg' width='100%'><img src='/img/0332-qqplot2.png' width='100%'></object>

Indeed, even when we increase the sample size to 100, we still don't get a satisfactory normal distribution of the sample means:

<object type="image/svg+xml" data='/img/0332-qqplot3.svg' width='100%'><img src='/img/0332-qqplot3.png' width='100%'></object>

Those two plots were made with this:
{% highlight R lineanchors %}
lnpop <- exp(rnorm(N))
sim_clt(lnpop, n = 5, main = "Mean from log-normal dist, n = 5")
sim_clt(lnpop, n = 100, main = "Mean from log-normal dist, n = 100")
{% endhighlight %}

That's ok for looking at one experiment at a time, but what I really wanted was a range of different distributions, tried out systematically for different sample sizes, each combination run 10,000 times. So that's what this next chunk of code does&mdash;defines some sample sizes, makes a list of functions that generates descriptions and labels them, and runs the simulation for each combination.

The distributions I'm using are:
* standard (ie mean 0, standard deviation 1) normal distribution
* standard (ie mean 0, standard deviation 1 for the underlying normal distribution) log normal
* a higher variance log normal (underlying normal distribution has mean 0, standard deviation of 2)
* a Cauchy distribution with infinite variance&mdash;this is a known case where the central limit theorem doesn't avail, and whatever your sample size the distribution of the mean never tends towards normal. This is the far extreme opposite of the case where the population distribution is a simple normal.
* a Pareto distribution with shape of 2.1 and scale of 1&mdash;this was chosen as a challenging but not impossible case for the central limit theorem
* a standard exponential distribution
* a 60/40 mixture of a standard exponential distribution and standard normal distribution
* a 90/10 mixture of a standard normal distribution and an outlier normal distribution&mdash;to test the case of a mixture of two normals, which I've generally found an interesting and sometimes troubling case.

Here's the code for all that:
{% highlight R lineanchors %}
ns <- c(5:30, 50, 100, 300, 1000, 2000, 5000, 10000, 20000, 40000)
sims <- list()

pop_dists <- list(
  "rnorm(N)" = function() rnorm(N),
  "exp(rnorm(N))" = function() exp(rnorm(N)),
  "exp(rnorm(N, sd = 2))" = function() exp(rnorm(N, sd = 2)),
  "rcauchy(N)" = function() rcauchy(N),
  "rpareto(N, shape = 2.1, scale = 1)" = function() {
    rpareto(N, shape = 2.1, scale = 1)
  },
  "rexp(N)" = function() rexp(N),
  "c(rexp(N*0.6), exp(rnorm(N*0.4)))" = function() {
    c(rexp(N * 0.6), exp(rnorm(N * 0.4)))
  },
  "c(rnorm(N*0.9,0,1), rnorm(N*0.1,3,3))" = function() {
    c(rnorm(N * 0.9, 0, 1), rnorm(N * 0.1, 3, 3))
  }
)


all_sims <- lapply(names(pop_dists), function(lbl) {
  pop <- pop_dists[[lbl]]()

  results <- lapply(ns, function(n) sim_clt(pop, n = n, plot = FALSE))

  tibble(
    n = ns,
    ad_stat = sapply(results, `[[`, "ad_stat"),
    coverage = sapply(results, `[[`, "coverage"),
    pop_dist = lbl
  )
}) |>
  bind_rows()
{% endhighlight %}

Now we're obviously not going to look at all the QQ-normal plots of all those distributions and sample sizes, but rather at some summary statistics. My first idea was to look at the Anderson-Darling statistics, which measure how different a distribution is from some given reference distribution. By having the same number of repetitions of each simulation, I reasoned these would be comparable; and in particular we could compare them to the case where the underlying population is itself normal, so we know that regardless of sample size, the sample mean will also be normal.

This next chart shows that comparison, ordering the distributions from best-behaved (by central limit theorem standards) to worst (which we knew in advance would be the Cauchy):

<object type="image/svg+xml" data='/img/0332-ad-stat.svg' width='100%'><img src='/img/0332-ad-stat.png' width='100%'></object>

So already we can see that sample sizes for many of these distributions do indeed need to be in the thousands or tens of thousands for the sample mean to be as well behaved, in terms of having a normal distribution, as the best case. The two worst cases are the Pareto distribution and the higher variance log-normal.

How bad is this pragmatically? We use the central limit theorem not because we really care direclty about the shape of the distribution of our statistic but because of the inferences we want to draw. So the function I started this blog post with also calculates a confidence interval based on asymptotic normality. 

So here we see the results of that. Basically not that great. For our 95% confidence intervals to have something approximating 95% coverage, in some of these distributions we do indeed need thousands or tens of thousands of observations in our sample:

<object type="image/svg+xml" data='/img/0332-coverage.svg' width='100%'><img src='/img/0332-coverage.png' width='100%'></object>

Here's the same chart zoomed in to just 80% and above. Note that the sampled mean from the Cauchy distribution doesn't even get on the chart:

<object type="image/svg+xml" data='/img/0332-coverage-trunc.svg' width='100%'><img src='/img/0332-coverage-trunc.png' width='100%'></object>

Here's the code that produced those charts:

{% highlight R lineanchors %}
normal_band <- all_sims |>
  filter(pop_dist == "rnorm(N)") |>
  pull(ad_stat) |>
  quantile(probs = c(0.025, 0.975))

p1 <- all_sims |>
  mutate(pop_dist = fct_reorder(pop_dist, ad_stat)) |>
  ggplot(aes(x = n, y = ad_stat)) +
  facet_wrap(~pop_dist, nrow = 2) +
  annotate(
    "rect",
    xmin = min(ns),
    xmax = Inf,
    ymin = normal_band[1],
    ymax = normal_band[2],
    alpha = 0.5,
    fill = "orange"
  ) +
  geom_point() +
  scale_x_log10(label = comma, breaks = ns[ns >= 30 | ns %in% c(5, 10, 20)]) +
  scale_y_log10(label = comma) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "plain", size = 9)
  ) +
  labs(
    x = "Sample size",
    y = "Anderson-Darling statistic",
    subtitle = glue(
      "Points represent average from {comma(today_reps)} simulations of given sample size. Shaded area covers 95% of values from a normal distribution."
    ),
    title = "Increasing sample size and growing effectiveness of the central limit theorem - Anderson-Darling statistic"
  )

print(p1)


p2 <- all_sims |>
  mutate(pop_dist = fct_reorder(pop_dist, ad_stat)) |>
  ggplot(aes(x = n, y = coverage)) +
  facet_wrap(~pop_dist, nrow = 2) +
  annotate(
    "rect",
    xmin = min(ns),
    xmax = Inf,
    ymin = 0.95,
    ymax = 1,
    alpha = 0.5,
    fill = "orange"
  ) +
  geom_point() +
  scale_x_log10(label = comma, breaks = ns[ns >= 30 | ns %in% c(5, 10, 20)]) +
  scale_y_continuous(label = percent) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "plain", size = 9)
  ) +
  labs(
    x = "Sample size",
    y = "Coverage of a 95% confidence interval based on t distribution",
    subtitle = glue(
      "Points represent average from {comma(today_reps)} simulations of given sample size. Shaded area shows 95% and higher, as desired."
    ),
    title = "Increasing sample size and growing effectiveness of the central limit theorem - confidence interval coverage"
  )

print(p2)

p3 <- p2 +
  labs(
    y = "Coverage of a 95% confidence interval based on t distribution\nY axis truncated to only start at 80%; some points excluded because of that."
  ) +
  coord_cartesian(ylim = c(0.8, 1))

print(p3)
{% endhighlight %}

So that's it really. Of course, Professor Harrell was correct as I knew he would be. And it wasn't hard to come up with a few skewed distributions that were problematic. Even the mixture of two normal distributions didn't perform brilliantly. So, yeah, take care out there with your sample sizes when drawing from difficult distributions of the underlying population.

