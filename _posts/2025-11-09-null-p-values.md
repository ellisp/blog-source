---
layout: post
title: Distribution of p-values under the null hypothesis for discrete data 
date: 2025-11-09
tag: 
   - Distributions
   - Reproducibility
   - Simulations
description: p-values under the null hypothesis do not necessarily have a uniform distribution.
image: /img/0299-histogram-sig-only-1e+05.png
socialimage: https:/freerangestats.info/img/0299-histogram-sig-only-1e+05.png
category: R
---

## Motivation

A few months back in a side skirmish during the great p-curve controversy, [Richard McElreath mentioned](https://bsky.app/profile/rmcelreath.bsky.social/post/3lvx73m6jc22o) that p-values under the null hypothesis are not always uniformly distributed, as is sometimes claimed. This prompted me to check out the phenomenon. I'll admit I had in my head the basic idea that p-values are indeed uniformly distributed if the null hypothesis is true. It turns out this is only 'often' the case, not always.

As is commonly the case for this blog, the main motivation was to make sure I understood something myself, so there's nothing particularly new for the world in this blog. But it might be interesting for some. There were a few odd fishhooks.

I wanted to check the case where we are comparing the number of "successes" in a relatively small sample of binary success/failure outcomes, split into two groups. The null hypothesis is that the underlying probability for success is the same in each group. I wanted to see the distribution of the p-value for a test of this null hypothesis with sample sizes of 10, 100 or 1000 observations in each group; for different values of the underlying probability of success, and for when the groups' sample size is random but mean 100.

## Calculating the p value

The fishhooks were in how to calculate the p value. I actually went for three different ways:
* My lazy approximation way is to estimate the variance of the difference between the two sample proportions under the null hypothesis and rely on asymptotic normality to get a probability of the value being as extreme as it actually is. This has the disadvantage of being known to be not particularly right particularly when the sample is small or the probability of success is close to 1 or 0. The advantage is I didn't need to look anything up and it was easy to vectorise. This method is called `pval_hand` in the plot below.
* A better method is to use the [Fisher exact test](https://en.wikipedia.org/wiki/Fisher's_exact_test), as per the famous lady-tasting-tea analysis, but the out of the box method I was using (from the `corpora` R package by Stephanie Evert) doesn't work when the observed successes in both samples is zero. This method is called `pval_fisher` in the plot below.
* The most out-of-the-box method of all is to use the `prop.test()` function from the `stats` package built into R. The disadvantage here was the bother in vectorising this function to run efficiently with large numbers of simulations. This method is called `pval_proptest` in the plot below.

Because I was nervous about whether these methods might give materially different results I started by comparing the results they gave, with different sample sizes and underlying parameters, with just 1,000 repetitions for each combination of sample size and parameter. This gives this comparison:

<object type="image/svg+xml" data='/img/0299-pairs-1000.svg' width='100%'><img src='/img/0299-pairs-1000.png' width='100%'></object>

So we can see that the `pval_fisher` and `pval_proptest` methods give effectively the same results, whereas my hand-made method has a significant number of discrepancies. Because of this I decided to stick with Evert's `corpora::fisher.pval`. I just hardened it up with a wrapper function to define the p value (chance of seeing data as extreme as this, if the null hypothesis is true) to be 1 if the observed successes are 0 in both samples:

{% highlight R lineanchors %}
library(tidyverse)
library(corpora)
library(GGally)
library(glue)
library(scales)
library(frs) # for svg_png()

#' Version of fisher.pval that won't break if k1 and k2 both 0
tough_fisher <- function(k1, n1, k2, n2, set_both_zero = 1, ...){
  problems <- k1 + k2 == 0
  k1[problems] <- 1
  temp <- corpora::fisher.pval(k1, n1, k2, n2)
  temp[problems] <- set_both_zero
  return(temp)
}
# note needed a decision on what to do when k1 and k2 are both zero. I think
# p value is 1 here, as p is the "probability of seeing data at least as extreme
# as this if the null hypothesis of no difference is true". Not sure why Fisher's
# exact test returns an error, worth looking into that. 
{% endhighlight %}

## Results

So with that problem out of the way, I set out to calculate lots of p-values. I did this in the situations 
* where the two groups were equally sized with 10, 100, or 1,000 observations each; or where they are a random number of observations, mean 100, still the same for each of the two groups; and
* with an underlying probability of success of 0.2, 0.5, or 0.8.

The intution for some of the reason of why the p-values aren't uniformly distributed is that with a finite sample and a a discrete outcome, there are only a finite number of possible values for the p-value. So of course it can't have a perfectly uniform distribution, which implies a p-value taking any value between 0 and 1 with equal probability. Further down we see (approximately) how many possible values the p-value takes. The smaller the sample sizes, the more we would expect to see divergence from normality.

And that is exactly what we do see. Here is the full distribution of p-values for 100,000 simulations of each combination:

<object type="image/svg+xml" data='/img/0299-histogram-1e+05.svg' width='100%'><img src='/img/0299-histogram-1e+05.png' width='100%'></object>

... and here is the distribution just of the p-values that are conventionally "significant" ie below 0.05:

<object type="image/svg+xml" data='/img/0299-histogram-sig-only-1e+05.svg' width='100%'><img src='/img/0299-histogram-sig-only-1e+05.png' width='100%'></object>

I think there are a few interesting subtleties here. In particular:

* When the size of the two groups is a random variable (but still equal) then the distribution of p-values is much more uniform (but still not exactly uniform). Basically there are many more possibiliities for the p-values to take with this extra randomness.
* The p-values are more non-uniform when the underlying probability for success is 0.5 in each case rather than 0.2 or 0.8
* The p-values can still be very non-uniform even with a large sample size of 1,000 observations per group (if underlying probability of success is close to 0.5)

Here's a potentialy interesting little insight into one of the reasons why this works that way - the *number* of different unique p-values that we get for each combination of sample size and underlying probabilities:

```
  size_lab    `Prob=0.2` `Prob=0.5` `Prob=0.8`
  <chr>            <int>      <int>      <int>
1 n=10                32         38         31
2 n=100              391        533        403
3 n=1000            2915       3853       2903
4 n=Pois(100)       9812      12582       9898
```

I'm not drawing any conclusions in any metascience debates here. Just noting this interesting phenomenon in the distribution of p-values.

Here's the rest of the code for doing these simulations. 

{% highlight R lineanchors %}
# Takes about 30 seconds with 100,000 reps
st <- system.time({
  for(reps in c(1e3, 1e5)){
    set.seed(42)
    
    d <- expand_grid(
      prob = rep(c(0.2, 0.5, 0.8), each = reps),
      size = c(10, 100, 1000, NA)
    ) |> 
      mutate(size_lab = ifelse(is.na(size), "n=Pois(100)", glue("n={size}")),
             prob_lab = glue("Prob={prob}")) |> 
      mutate(size = ifelse(is.na(size), rpois(n(), 100), size)) |> 
      mutate(x1 = rbinom(n(), size = size, prob = prob),
            x2 = rbinom(n(), size = size, prob = prob),
            # observed proportions p1 and p2 from the two populations
            p1 = x1 / size,
            p2 = x2 / size,
            # under null hypothesis, the equal probability of both pops:
            pmid = (p1 + p2) / 2,
            # observed difference of the two proportions:
            delt = abs(p1 - p2),
            # variance of each of p1 and p2: p(1-p)/n
            s1 = pmid * (1 - pmid) / size,
            # standard deviation of the sum of two of those variances
            sddelt = sqrt(s1 + s1)) |> 
      # calculate p values
      mutate(pval_hand = 2 * (1 - pnorm(delt / sddelt)),
             pval_fisher = tough_fisher(x1, size, x2, size),
             pval_proptest = NA)
  
    if(reps < 10000){
      # when the number of reps is fairly small, I draw a pairs plot just to
      # compare the different ways of calculating p values:
      # - prop.test (out of the box R method). I couldn't find an easy way to vectorize this, 
      #   is why it is only done here, via a loop. when reps is small
      # - pval_hand (my home made approximate method)
      # - pval_fisher (Fisher exact test, but toughened up as above to give 1 when both k1 and k2 are 0)
      # The main conclusion from this is that my by-hand aproximation isn't great!
      system.time({
        for (i in 1:nrow(d)){
          x <- d[i, ]
          d[i, "pval_proptest"] <- prop.test(c(x$x1, x$x2), c(x$size, x$size))$p.value
        }
      })
      # 5 seconds for reps=1000
      
      plot1 <- function(){
        d |> 
          select(pval_hand:pval_proptest, size_lab) |> 
          ggpairs() |> 
          print()
      }
      
      svg_png(plot1, glue("0299-pairs-{reps}"), w = 10, h = 8)      
    }
  
  
    plot2 <- d |> 
      ggplot(aes(x = pval_fisher)) +
      facet_grid(size_lab  ~ prob_lab, scales = "free_y") +
      geom_histogram(fill = "steelblue") +
      scale_y_continuous(label = comma) +
      labs(title = "Distribution of all p values when a null hypothesis is true",
          subtitle = "Equal size binomial samples drawn from two populations with same underying probability",
          x = "P value from Fisher's exact test
    (when zero positive cases in both samples, p value is set to one)",
          y = glue("Count of simulations (out of {comma(reps)})"))
  
    plot3 <- d |>
    filter(pval_fisher < 0.05) |> 
    ggplot(aes(x = pval_fisher)) +
    facet_grid(size_lab  ~ prob_lab, scales = "free_y") +
    geom_histogram(fill = "steelblue") +
    scale_y_continuous(label = comma) +
    labs(title = "Distribution of significant (<0.05) p values when a null hypothesis is true",
         subtitle = "Equal size binomial samples drawn from two populations with same underying probability",
         x = "P value from Fisher's exact test
  (when zero positive cases in both samples, p value is set to one)",
         y = glue("Count of simulations (out of {comma(reps)})"))
  
      
    svg_png(plot2, glue("0299-histogram-{reps}"), w = 9, h = 6)     
    svg_png(plot3, glue("0299-histogram-sig-only-{reps}"), w = 9, h = 6)      
  
  }
})

print(st)

# Number of unique p-values
d |> 
  group_by(prob_lab, size_lab) |> 
  summarise(number_p_values = length(unique(pval_fisher))) |> 
  spread(prob_lab, number_p_values)
{% endhighlight %}

