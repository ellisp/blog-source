---
layout: post
title: Sampling distribution of weighted Gini coefficient
date: 2017-08-05
tag: 
   - Inequality
   - Transformations
   - R
description: I play around with the sampling distribution of Gini coefficients calculated with weighted data; and verify that the Gini calculation method in a recent Stats NZ working paper is the one in the acid R package.
image: /img/0106-sample-distributions.svg
socialimage: http://ellisp.github.io/img/0106-sample-distributions.png
category: R
---

## Calculating Gini coefficients 

Stats NZ release a series of working papers, and a recent one caught my eye because of my interest in inequality statistics (disclaimer - I am working at Stats NZ at the moment, but on completely different things). Working Paper No 17–02 by Vic Duoba and Nairn MacGibbon is on [Accurate calculation of a Gini index using SAS and R](http://www.stats.govt.nz/methods/research-papers/working-papers-original/calc-gini-index-17-02.aspx).  While the conceptual definition of a Gini coefficient is well known, there are a few different methods out there for estimating one from finite data.  

To refresh memories, a Gini coefficient is defined as the ratio A / (A +B) in the diagram below, which is drawn with simulated data I'll introduce later in the post:

<img src='/img/0106-lorenz.svg' width='100%'>

In a situation of perfect equality (everyone has the same income, or whatever it is we are measuring inequality of) the Lorenz curve *is* the diagonal line, and A is 0 so the Gini coefficient is 0.  In a situation where the richest person has *all* the income and everyone else none, the Lorenz curve is horizontal along the bottom of the chart and spikes up to 1 at the extreme right - so B is 0 and the Gini coefficient is 1.  Income Gini coefficients are usually calculated on household incomes, and according to Wikipedia they range from [0.24 (Slovenia) to 0.63 (Lesotho)](https://en.wikipedia.org/wiki/List_of_countries_by_income_equality).  My simulated data above is meant to represent *individual* income, which is higher than household income in situations where significant numbers of low income individuals live with higher income people.

Duoba and MacGibbon do a brief review of algorithms and choose a preferred one, noting along the way that the existing Stats NZ method will underestimate inequality (very, very slightly - this isn't something that should make the news please, it's the fourth decimal place).  They helpfully provide a SAS program to implement the algorithm which is literally two pages long, and a much shorter R program that is half a page.

### Q1.  Is `acid::weighted.gini()` using the best method?

Of course, shorter, more readable, and more mathematically-oriented (ie vectorized) code is one reason I prefer R to SAS.  But another reason is that nearly any functionality one can imagine for published techniques already has an implementation out there we can borrow.  In this respect, pitting R against SAS is simply no contest; it's like [Wikipedia versus an encyclopedia written by a single organisation](https://en.wikipedia.org/wiki/Wikipedia:Size_comparisons). Wikipedia has 5.5 million articles in English, Britannica's last hard copy had 40,000 (and the online edition has 120,000, but I think the hard copy is somehow more like SAS, if I'm not pushing the metaphor too far).  The R universe quite simply has vastly more functionality available to it than is ready off the shelf for SAS users.

So while Duoba and MacGibbon's 20 effective lines of R is preferable to 90 lines of SAS, even more convenient is to just use the one liner `acid::weighted.gini(x, w)`, using free user contributions to the R universe.

With choice comes uncertainty.  I did want to check that the method I've been using for calculating Gini coefficients from weighted data (like in [this post on inter-country inequality](/blog/2017/07/22/inter-country-inequality)) matched the one recommended by Duoba and MacGibbon.  I used the `weighted.gini` function from [Alexander Sohn (2016). acid: Analysing Conditional Income Distributions. R package version
  1.1.](https://CRAN.R-project.org/package=acid).

First, here's code setting up the session and creating Duoba and MacGibbon's `StatsGini` R function:  
  
{% highlight R %}
library(tidyverse)
library(acid)
library(microbenchmark)
library(forcats)
# Peter's Stats Stuff miscellaneous stuff package, install with: 
devtools::install_github("ellisp/pssmisc-r-package/pkg")
library(pssmisc) 

# Duoba and MacGibbon's function, reproduced as in the original paper:
StatsGini <- function(x, w = rep(1, length(x))){
  # x and w are vectors
  # w can be left blank when calling the fn (i.e. no weighting)
  # Examples:
  # x <- c(3, 1, 7, 2, 5)
  # w <- c(1, 2, 3, 4, 5)
  # StatsGini(x, w) should yield 0.2983050847
  # StatsGini(c(0.25, 0.75), c(1, 1)) should yield 0.25
  n <- length(x)
  wxsum <- sum(w * x)
  wsum <- sum(w)
  sxw <- order(x, w) # Ascending order sort
  sx <- w[sxw] * x[sxw]
  sw <- w[sxw]
  pxi <- vector(mode = "numeric", length = n)
  pci <- vector(mode = "numeric", length = n)
  pxi <- cumsum(sx) / wxsum
  pci <- cumsum(sw) / wsum
  G <- 0.0
  for (i in 2:n){
    G <- G - (pci[i] * pxi[i - 1] - pxi[i] * pci[i - 1] )
  }
  return(G)
}
{% endhighlight %}

And here's some checks.  We see to 10 decimal places, using the toy examples provided by Stats NZ, we get identical results:

```
> options(digits = 10)
> x <- c(3, 1, 7, 2, 5)
> w <- c(1, 2, 3, 4, 5)
> StatsGini(x, w) # should yield 0.2983050847
[1] 0.2983050847
> StatsGini(c(0.25, 0.75), c(1, 1)) # should yield 0.25
[1] 0.25
> weighted.gini(x, w)
$Gini
             [,1]
[1,] 0.2983050847

> weighted.gini(c(0.25, 0.75), c(1, 1))
$Gini
     [,1]
[1,] 0.25

```
Tests with more complicated data showed identical results too but I won't bore the reader with them.

### A realistic individual income simulation

Next, I wanted to try out the two functions on some realistic data.  Individual income data typically has a very complex distribution that is best thought of as a mixture of at least three distributions.  There is usually a spike at zero for zero income individuals, a smattering of people with negative incomes, and then a mixture of skewed distributions for positive incomes.  Typically, *heavily* skewed, like a log-Normal distribution with bonus outliers - there are a small number of very high income individuals out there.

To simulate the data in the Lorenz curve graphic I started with, I used the function below.  It implements a mixture of log-t distributions (using t rather than Normal because of the longer tails) ie e to the power of a variable from a mix of t distributions.  The mean of those t distributions itself is a random variable, in this case a non-transformed t distribution.  Then each value is multiplied at random by -1, 0 or 1 to simulate the idiosyncratic negative, zero, positive nature of incomes.

When I create the data frame with 100,000 incomes in it, I also create a column called `prob` with randomly generated numbers between 0 and 1 following a Beta(2, 2) distribution.  I'm going to use that later to simulate the experience of sampling individuals for a complex survey.

{% highlight R %}
#' Generate a realistic mixed distribution of incomes
#'
#' alpha ~ t(d_alpha) scaled to mean, sd of (log(mu), sigma_alpha)
#' beta ~ t(d_beta) scaled to mean, sd of (alpha, sigma_beta)
#' x = exp(beta)
#' y = x * I(-1, 0, 1)
#'
#' @param n sample size to generate
#' @param mu median income
#' @param sigma_alpha standard deviation of underlying latent means
#' @param sigma_beta standard deviation of scaled t distribution underlying the population's log-t distribution
#' @param d_alpha degrees of freedom in th
#' @param d_beta degrees of freedom in the t distribution underlying the log-t
#' @param pr vector of 2 elements, the probability of negative income and of zero income respectively
gen_incomes <- function(n, mu = 30000, 
                        sigma_alpha = 1, sigma_beta = 1, 
                        d_alpha = 15, d_beta = 15,
                        pr = c(0.01, 0.2)){
  alpha <- rt(n, d_alpha) * sigma_alpha + log(mu)
  beta <- exp(rt(n, d_beta) * sigma_beta + alpha)
  y <- beta * sample(c(-1, 0, 1), n, replace = TRUE, prob = c(pr, 1 - sum(pr)))
  return(y)
}

set.seed(123)
N <- 100000
population <- data.frame(income = gen_incomes(N), sigma_alpha = 1, d_alpha = 10, d_beta = 150,
                         sigma_beta = 2,
                         prob = rbeta(N, 2, 2))

# draw density plot with a slice of 1000 people:
population %>%
  slice(1:1000) %>%
  ggplot(aes( x = income)) + 
  scale_x_continuous("Annual income (modulus transform with power of 0.02)",
                     label = dollar, 
                     trans = modulus_trans(0.02),
                     breaks = modulus_breaks(lambda = 0.02)) + 
  geom_density(fill = "grey", alpha = 0.2)  +
  geom_rug() +
  ggtitle("Density of simulated annual incomes",
          "First 1,000 values")
{% endhighlight %}

This is what that looks like:

<img src='/img/0106-density.svg' width='100%'>

I wrote about the modulus transform, used in the horizontal axis in that chart, in [earlier blog posts](/blog/2015/09/05/creating-a-scale-transformation).  It's an idea put forward by John and Draper in 1980 that I'm surprised hasn't taken off more.  It's very similar to a Box-Cox power transform, but it works for values of 0 and negative values too.  It does something very similar to a Box-Cox transform to the absolute value of the data, then returns the sign to it.  It's a much, much better answer than the commonly used alternatives to what to do with data that looks like it needs a log transformation but inconveniently has some zeros and negatives.  I've implemented methods to do this in my new `pssmisc` ad hoc R package, referenced earlier in the code.

For completeness, here's the code that drew the Lorenz curve diagram at the top of the post:

{% highlight R %}
population %>%
  # thin down to just a sample of 10,000 to save on vector graphic file size:
  sample_n(10000) %>%
  arrange(income) %>%
  mutate(cum_prop_inc = cumsum(income) / sum(income),
         seq = 1:n() / n()) %>%
  ggplot(aes(x = seq, y = cum_prop_inc)) +
  geom_line() +
  geom_ribbon(aes(ymax = seq, ymin = cum_prop_inc), fill = "steelblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  labs(x = "Cumulative proportion of population, from poorest to richest",
       y = "Cumulative proportion of income") +
  annotate("text", x = c(0.6, 0.95), y = c(0.4, 0.05), label = c("A", "B"), size = 7) +
  annotate("text", x = 0.5, y = 0.6, label = "Line of equality", angle = 45) +
  annotate("text", x = 0.85, y = 0.15, label = "Actual distribution", angle = 45) +
  coord_equal() +
  ggtitle("Lorenz curve with simulated income data",
          paste("Gini coefficient is", round(gini(population$income)$Gini, 2)))
{% endhighlight %}


### Q2. Is acid:weighted.gini more or less efficient than the StatsNZ alternative?

Next I wanted to try out the efficiency of the two functions.  To do this I gave them a tougher job - estimate the Gini coefficient for the whole population of 100,000, but using the inverse of the `prob` column as weights.  The `microbenchmark` package making it easy to do this 1,000 times for a reliable estimate, and even gives an automatic `ggplot2` method.

{% highlight R %}
y <- population$income
w <- 1/ population$prob
mb <- microbenchmark(
  weighted.gini(y, w)$Gini,
  StatsGini(y, w),
  times = 1000)

  autoplot(mb, log = FALSE) +
    ggtitle("Time taken to estimated Gini coefficient from 100,000 observations",
            "Microbenchmark results") 
{% endhighlight %}

With these results:

<img src='/img/0106-benchmark.svg' width='100%'>

The method in `acid::weighted.gini` is slightly faster than is `StatsGini`.  `acid` uses matrix multiplication for a calculation that `StatsGini` does through a `for` loop, which explains the difference.  It's not a big deal though, and both functions reliably taking less than 50 millionths of a second to do the calculation on a sample size of 100,000.

### Q3. What does the sampling distribution of Gini from a weighted complex survey sample look like?

Finally, I had a more substantive question of interest from a statistical point of view.  In the real world, we have finite samples to estimate inequality from and our estimate of Gini coefficient will be random; it will have a sampling distribution.  The samples often come from a complex survey design, which complicates the estimation process and changes the sampling distribtuion.  Allowing for data from sources like a complex survey is why both calculation methods allow for a vector of unequal weights, which in this context would normally be calculated by a method such as the inverse probability of an population member being in the sample.  

I've looked previously at the [sampling distribution of Gini coefficient](http://ellisp.github.io/blog/2015/09/12/inequality-stats-distributions), but only with unweighted data.  Reading Duoba and MacGibbon's paper reminded me that I should probably look at the realistic situation of estimating Gini coefficient with weighted data from a complex survey.

To do this in a pragmatic way, I simulated surveys with a range of sample sizes by creating multiple unequally probability random samples from my population of 100,000.  This doesn't really re-create the experience of a complex survey because I haven't got primary sampling units and intra-class correlation (which make estimation harder and sampling distributions wider), but it is a start for illustrative/exploratory purposes.  I calculated the weighted Gini coefficient for each and kept the results.  When we plot the distributions of all these efforts to estimate the population total (which we know is really 0.82) we get the following:

<img src='/img/0106-sample-distributions.svg' width='100%'>

I truncated the horizontal axes (but not the density estimations) because there were a few very bad estimates from the smaller sample sizes - as high as 1.5 - that made the visual comparison difficult.  How can a Gini coefficient be greater than 1?  When there are enough negative incomes.

We see that a sample size of 500 isn't too bad, with most estimates coming out between 0.7 and 0.9.  But even with a sample of 10,000 there's quite a bit of variation in the Gini coefficient of the sample.  Lesson - don't rely too much on the decimal points in inequality data coming from samples (and that's without even getting into the other formidable measurement problems with economic data).

Here's the code for the simulation:

{% highlight R %}
# number of repeats at each sample size:
reps <- 100
# sample sizes, chosen to resemble realistic survey sizes:
ns <- c(30, 100, 500, 1000, 3000, 10000)

# matrix of zeros that we'll replace with the estimates of Gini
res <- matrix(0, nrow = reps, ncol = length(ns))

# obviously this next bit could be made much more efficient by parallel processing,
# but it's only a few minutes of running 
set.seed(666)
for(i in 1:length(ns)){
  n <- ns[i]
  for(j in 1:reps){
    the_sample <- sample_n(population, size = n, replace = FALSE, weight = population$prob)    
    
    res[j, i] <- weighted.gini(the_sample$income, w = 1 / the_sample$prob)$Gini
   }
}

res_df <- as.data.frame(res)
names(res_df) <- format(ns, big.mark = ",")

res_df %>%
  gather(sample_size, value) %>%
  mutate(sample_size = fct_reorder(sample_size, -value)) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~sample_size) +
  geom_density(alpha = 0.3, fill = "grey") +
  # a few values of > 1 make no sense and warp the graph:
  coord_cartesian(xlim = c(0.6, 1)) +
  labs(x = "Estimated weighted Gini") +
  ggtitle("Sampling distribution of weighted Gini point estimate from a complex survey",
          paste0("Various sample sizes; true value is ", round(weighted.gini(population$income)$Gini, 2)))
{% endhighlight %}

Finished for today.