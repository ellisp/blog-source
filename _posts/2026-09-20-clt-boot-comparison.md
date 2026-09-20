---
layout: post
title: Bootstrap v traditional asymptotic normal assumptions
date: 2026-09-20
tag: 
   - Simulations
   - Distributions
description: Bias-corrected and adjusted bootstrap does an ok job at confidence intervals of the mean  from some example skewed distributions. Better than does relying on the traditional methods of just assuming normality from the central limit theorem. But for particularly awkward distributions, sample sizes are still needed in the thousands to get coverage that resembles the claimed coverage.
image: /img/0333-sims-results.svg
socialimage: https:/freerangestats.info/img/0333-sims-results.png
category: R
---

Today's just a very short sequel to [last week's post](/blog/2026/09/16/clt-simulations), where I had a look at some very skewed distributions to test the idea that sample sizes sometimes need to be in the tens of thousands for the sample mean to have a normal distribution. Turns out they do.

I had a bit of unfinished business at the back of my mind, which was "would a [bootstrap](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)) confidence interval do any better?". Hence today's new set of simulations.

I compared the coverage of a 95% confidence interval for the mean constructed the traditional way&mdash;like they teach it in basic stats courses&mdash;from a few heavily skewed distributions. I also constructed a 95% confidence interval using the bias-corrected and adjusted bootstrap method, which I believe is the best candidate to work in a wide variety of bias and skew situations.

To skip to the chase, here's the results. Turns out that a) the bootstrap does indeed do considerably better than just relying on the central limit theorem, particularly with smaller sample sizes; and b) it's still got coverage a lot less than the 95% we wanted:

<object type="image/svg+xml" data='/img/0333-sims-results.svg' width='100%'><img src='/img/0333-sims-results.png' width='100%'></object>

No surprise here; from what I understand of the history, this is pretty much exactly what the BCa bootstrap was developed for. So we're on it's home ground, and it does (relatively) well. But those actual coverage numbers are still well below 95%, for both methods.

Here's the code that did that. It's very similar to the one from a few days ago.

{% highlight R lineanchors %}
library(tidyverse)
library(actuar)
library(glue)
library(scales)
library(boot)

# set the below to TRUE if running for the first time
run_sims <- FALSE

set.seed(123)

# Number of repeats for each combination of sample size and population:
today_reps <- 1000

# Population size:
N <- 1e6

# Modified version of the function we used last week, this time just looking at
# coverage of confidence intervals and using a BCa bootstrap to compare to the
# traditional asuymptotic CLT/normal assumed one:
sim_clt2 <- function(
  x,
  n = 30,
  reps = today_reps,
  replace = TRUE,
  conf = 0.95,
  boot_R = 3001,
  ...
) {
  true_mean <- mean(x)

  samples <- replicate(
    reps,
    sample(x = x, size = n, replace = replace),
    simplify = FALSE
  )
  means <- sapply(samples, mean)

  covered_clt <- sapply(samples, function(s) {
    # rely on asymptotic normality to estimate a confidence interval and check
    # for coverage for each sample
    se <- stats::sd(s) / sqrt(length(s))
    ci <- mean(s) + c(-1, 1) * qnorm((1 - conf) / 2 + conf) * se
    ci[1] <= true_mean & true_mean <= ci[2]
  })

  covered_boot <- sapply(samples, function(s) {
    b <- boot::boot(
      data = s,
      statistic = function(x, w) {
        mean(x[w])
      },
      R = boot_R
    )
    ci_boot_res <- boot::boot.ci(b, conf = conf, type = "bca")
    ci_boot <- ci_boot_res$bca[4:5]
    ci_boot[1] <= true_mean & true_mean <= ci_boot[2]
  })

  return(list(
    coverage_clt = mean(covered_clt),
    coverage_boot = mean(covered_boot)
  ))
}

# Populations we're going to use
pops <- list(
  exp(rnorm(N)),
  exp(rnorm(N, sd = 2)),
  exp(rexp(N, rate = 2))
)

# Sample sizes we're going to use
ns <- c(10, 30, 200, 1000)

# Run simulations:
if (run_sims) {
  results <- expand_grid(pop = 1:3, n = ns) |>
    mutate(coverage_clt = NA, coverage_boot = NA)

  # this - obviously when you think about what it's doing - will take a long time
  # (~2 hours) to run. It's embarassingly parallel so could consider parallelising
  # it easily enough, but there is a lot of demands on memory so for my laptop is
  # probably not going to be worth trying this as the machine wouldn't be able to
  # do multiple goes of the 3000 rep bootstrap, 1000 rep simulation from a 1e6
  # population at once.
  for (i in 1:nrow(results)) {
    cat(i)
    param <- results[i, ]
    tmp <- sim_clt2(pops[[param$pop]], n = param$n)
    results[i, ]$coverage_clt <- tmp$coverage_clt
    results[i, ]$coverage_boot <- tmp$coverage_boot
  }

  save(results, file = glue("0333-boot-results-{Sys.Date()}.rda"))
} else {
  lf <- sort(
    list.files(pattern = "0333-boot-results.*\\.rda$"),
    decreasing = TRUE
  )
  load(lf[1])
}

# labels for the populations:
pop_labs <- c("log normal(0,1)", "log normal(0,2)", "exponential(2)")

# Draw plot:
p <- results |>
  mutate(lab = pop_labs[pop]) |>
  mutate(lab = fct_reorder(lab, coverage_boot)) |>
  ggplot(aes(x = coverage_clt, y = coverage_boot, colour = lab)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_text_repel(aes(label = comma(n)), seed = 123, alpha = 0.5) +
  coord_equal() +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  labs(
    x = "Confidence interval from asymptotic normality includes the mean",
    y = "Confidence interval from BCa bootstrap includes the mean",
    title = "Bootstrap outperforms asymptotic normality assumption with smaller n.",
    subtitle = "Proportion of time the 95% confidence interval actually contains the true value.
Labelled numbers indicate sample sizes. Diagonal line shows equal performance.",
    colour = "Population distribution:"
  )

 print(p)
{% endhighlight %}

I still haven't looked at the point&mdash;raised by Professor Harrell himself after my last post&mdash;of the assymetry of these confidence intervals, which causes a whole new set of problems. I think I've run out of oomph for looking at that, but it is actually an important point to remember. Maybe some time later.

That's it for today really. I still think the bootstrap is a close to magic as you get in frequentist statistics, and I thoroughly recommend it. It's good stuff. But when you've got a sample size of 10, 30, 200&mdash;sometimes even when you've got 1,000, 10,000 or 50,000&mdash;there's just limits to what you can do.

