---
layout: post
title: Power and 'fragile' p-values
date: 2025-06-08
tag: 
   - Simulations
   - Reproducibility
description: What proportion of significant <i>p</i> values should be between 0.01 and 0.05? Turns out the answer is 'it depends'.
image: /img/0295-fragile-diff.svg
socialimage: https:/freerangestats.info/img/0295-fragile-diff.png
category: R
---

### Do 'fragile' <i>p</i> values tell us anything? ##
I was interested recently to see [this article on <i>p</i> values in the psychology literature](https://journals.sagepub.com/doi/10.1177/25152459251323480) float across my social media feed. Paul C Bogdan makes the case that the severity of the [replication crisis](https://en.wikipedia.org/wiki/Replication_crisis) in science can be judged in part by the proportion of <i>p</i> values that are 'fragile',which he defines as between 0.01 and 0.05. 

Of course, concern at the proportion of <i>p</i> values that are 'significant but only just' is a stable feature of the replication crisis. One of the standing concerns with science is that researchers use questionable research practices to somehow nudge the <i>p</i> values down to just below the threshold deemed to be "signficant" evidence. Another standing concern is that researchers who might not use those practices in the analysis themselves will not publish or not be able to publish their null results, leaving a bias towards positive results in the published literature (the ["file-drawer" problem](https://en.wikipedia.org/wiki/Publication_bias#:~:text=This%20term%20suggests%20that%20negative,a%20bias%20in%20published%20research.)).

Bogdan argues that for studies with 80% power (defined as 1 minus the probability of accepting the null hypothesis when there is in fact a real effect in the data), 26% of <i>p</i> values that are significant should be in this "fragile" range, based on simulations. 

The research Bogdan describes in the article linked above is a clever data processing exercise of published psychology literature to see what proportion of <i>p</i> values are in fact, "fragile" and how this changes over time. He finds that "From before the replication crisis (2004–2011) to today (2024), the overall percentage of significant <i>p</i> values in the fragile range has dropped from 32% to nearly 26%". As 26% is about what we'd expect, if all the studies had power of 80%, then this is seen as good news. 

Is the replication crisis over? (to be fair, I don't think Bogdan claims this last point).

One of Bogdan's own citations is [this piece by Daniel Lakens](https://peerj.com/articles/1142/), which itself is a critique of a similar attempt at this earlier. Lakens argues "the changes in the ratio of fractions of p-values between 0.041–0.049 over the years are better explained by assuming the average power has decreased over time" rather than by changes in questionable research practices. I think I agree with Lakens on this.

I just don't think the 26% of significant <i>p</i> values to be 'fragile' is a solid enough benchmark to judge research pracices on.

Anyway, all this intrigued me enough when it was discussed first in [Science](https://www.science.org/content/article/big-win-dubious-statistical-results-are-becoming-less-common-psychology) (as "a big win") and then on [Bluesky](https://bsky.app/profile/jbakcoleman.bsky.social/post/3lqyuqimtq22a) for me to want to do my own simulations to see how changes in effect sizes and sample sizes would change that 26%. My hunch was 26% was based on assumptions that all studies have 80% power and (given power has to be calculated for some assumed but unobserved true effect size) that the actual difference in the real world is close to the difference assumed in making that power calculation. Both these assumptions are obviously extremely brittle, but what is the impact if they are wrong?

From my rough playing out below, the impact is pretty material. We shouldn't think that changes in the proportion of signficant <i>p</i> values that are between 0.01 and 0.05 tells us much about questionable research practices, because there is just too much else going on &mdash; pre-calculated power, how much power calculations and indeed the research that is chosen are based on a good reflection of reality, the size of differences we're looking for, and sample sizes &mdash; confounding the whole thing.

### Do your own <s>research</s> simulations
To do this, I wrote a simple function `experiment` which draws two independent samples from two populations, all observations normally distributed. For my purposes the two sample sizes are going to be the same and the standard deviations the same in both populations; only the means differ by population. But this function is set up for a more general exploration if I'm ever motivated.


#### The ideal situation - researcher's power calculation matches the real world
With this function I first played around a bit to get a situation where the power is very close to 80%. I got this with sample sizes of 53 each and a difference in the means of the two populations of 0.55 (remembering each population has a standard distribtuion of N(0, 1)).

I then checked this with a published power package, <i>Bulus, M. (2023). `pwrss`: Statistical Power and Sample Size Calculation Tools. R package version 0.3.1. https://CRAN.R-project.org/package=pwrss</i>. I've never used this before and just downloaded it to check I hadn't made mistakes in my own calculations, and later I will use it to speed up some stuff.

{% highlight R lineanchors %}
library(pwrss)
library(tidyverse)

experiment <- function(d, m1 = 0, sd1 = 1, sd2 = 1, n1 = 50, n2 = n1, seed = NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  x1 <- rnorm(n1, m1, sd1)
  x2 <- rnorm(n2 ,m1 + d, sd2)
  t.test(x1, x2)$p.value
}

reps <- 10000
res <- numeric(reps)

for(i in 1:reps){
  res[i] <- experiment(d = 0.55, n1 = 53)
}
{% endhighlight %}

Yes, that's right, I'm using a `for` loop here. Why? Because it's very readable, and very easy to write.

Here's what that gives us. My simulated power is 80%, Bulus' package agrees with 80%, and 27% of the 'signficant' (at alpha = 0.05) <i>p</i> values are in the fragile range. This isn't the same as 26% but it's not a million miles away; it's easy to imagine a few changes in the experiment that would lead to his 26% figure.

```
> # power from simulation
> 1 - mean(res > 0.05)
[1] 0.7964
> 
> # power from Bulus' package
> pwrss.t.2means(mu1 = 0.55, sd1 = 1, sd2 = 1, n2 = 53)
 Difference between Two means 
 (Independent Samples t Test) 
 H0: mu1 = mu2 
 HA: mu1 != mu2 
 ------------------------------ 
  Statistical power = 0.801 
  n1 = 53 
  n2 = 53 
 ------------------------------ 
 Alternative = “not equal” 
 Degrees of freedom = 104 
 Non-centrality parameter = 2.831 
 Type I error rate = 0.05 
 Type II error rate = 0.199 
> 
> # Of those experiments that have 'significant' results, what proportion are in 
> # the so-called fragile range (i.e. betwen 0.01 and 0.05)
> summ1 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)
> print(summ1)
[1] 0.2746107
```

#### Changes in difference and in sample size

I made some arbitrary calls in that first run &mdash; sample size about 50 observations in each group, and the difference about 0.5 standard deviations. What if I let the difference between the two populations be smaller or larger than this, and just set the number of observations to whatever is necessary to get 80% power? What change does this make to the proportion of <i>p</i> values that are 'fragile'?

It turns out it makes a *big* difference, as we see in these two charts:

<object type="image/svg+xml" data='/img/0295-fragile-diff.svg' width='100%'><img src='/img/0295-fragile-diff.png' width='100%'></object>
<object type="image/svg+xml" data='/img/0295-fragile-n.svg' width='100%'><img src='/img/0295-fragile-n.png' width='100%'></object>

These are simulations, still in the world where the researcher happens to guess the real world exactly right when they do their power calculation and determine a sample size to get 80% power. We see in the top chart that as the real world difference gets bigger, with constant power, the proportion of significant but 'fragile' <i>p</i> values goes up markedly. And the second chart shows the same simulations, but focusing on the variation in sample size which changes in compensation for the real world difference in populations, to maintain the same power. Bigger samples with the same power mean that you are looking for relatively smaller real world differences, and the proportion of significant <i>p</i> values that are 'fragile' gets smaller. 

Here's the code that did these simulations:

{% highlight R lineanchors %}
#--------------varying difference and sample sizes---------------
possible_diffs <- 10:200 / 100 # measured in standard deviations

# what sample size do we need to have 80% power
n_for_power <- sapply(possible_diffs, function(d){
  as.numeric(pwrss.t.2means(mu1 = d, power = 0.8, verbose = FALSE)$n[1])
})

prop_fragile <- numeric(length(possible_diffs))

# This takes some minutes to run, could be better if parallelized or done in
# Julia if we thought saving those minutes was important:
for(j in 1:length(possible_diffs)){
  for(i in 1:reps){
    res[i] <- experiment(d = possible_diffs[j], n1 = n_for_power[j])
  }
  prop_fragile[j] <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)
}

# Plot 1
tibble(prop_fragile, possible_diffs) |> 
  ggplot(aes(x = possible_diffs,y= prop_fragile)) +
  geom_point()+
  scale_y_continuous(label = percent) +
  labs(x = "Difference (in standard deviations) between two means",
       y = "Proportion of significant p values \nthat are between 0.01 and 0.05",
       title = "Two sample tests for difference between two means with power = 80%",
       subtitle = "t test for independent samples at a combination of sample size and population difference\nneeded to give the desired power. Both populations are standard normal distributions.")

# Plot 2
tibble(prop_fragile, n_for_power) |> 
  ggplot(aes(x = n_for_power,y = prop_fragile)) +
  geom_point() +
  scale_x_sqrt() +
  scale_y_continuous(label = percent) +
  labs(x = "Sample size needed to get 80% power for given difference of means",
       y = "Proportion of significant p values \nthat are between 0.01 and 0.05",
       title = "Two sample tests for difference between two means with power = 80%",
       subtitle = "t test for independent samples at a combination of sample size and population difference\nneeded to give the desired power. Both populations are standard normal distributions.")

{% endhighlight %}

#### Relaxing assumptions
OK, so that was what we get when the power calculation was based on a true representation of the world, known before we did the experiment. Obviously this is never the case (or we'd not need to do experiments) &mdash; the actual difference between two populations might be bigger or smaller than we expected, it might actually be exactly zero, the shape and spread of the populations will differ from what we thought when we calculated the power, etc.

I decided to try three simple breaks of the assumptions to see what impact they have on the 27% of <i>p</i> values that were fragile:
* The actual difference between populations is a random number, albeit on average is what is expected during the power calculation
* the actual difference between populations is a coin flip between exactly what was expected (when the power calculation was made) and zero (ie null hypothesis turns out to be true)
* the actual difference between population is a coin flip between a random number with average the same as expected and zero (ie a combination of the first two scenarios)


{% highlight R lineanchors %}
#------------------when true d isn't what was expected---------------

reps <- 10000
res <- numeric(reps)

# we are going to let the actual difference deviate from that which was used
# in the power calculation, but say that on average the planned-for difference
# was correct
for(i in 1:reps){
  res[i] <- experiment(d = rnorm(1, 0.55, 0.5), n1 = 53)
}

# "actual" power:
1 - mean(res > 0.05)

# proportion of so-called fragile p values is much less
summ2 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)

#---------when true d is same as expected except half the time H0 is true---------

for(i in 1:reps){
  res[i] <- experiment(d = sample(c(0, 0.55), 1), n1 = 53)
}


# proportion of so-called fragile p values is now *more*
summ3 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)

#---------when true d is random, AND half the time H0 is true---------

for(i in 1:reps){
  res[i] <- experiment(d = sample(c(0, rnorm(1, 0.55, 0.5)), 1), n1 = 53)
}


# proportion of so-called fragile p values is now less
summ4 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)

tibble(`Context` = c(
  "Difference is as expected during power calculation",
  "Difference is random, but on average is as expected",
  "Difference is as expected, except half the time null hypothesis is true",
  "Difference is random, AND null hypothesis true half the time"
), `Proportion of p-values that are fragile` = c(summ1, summ2, summ3, summ4)) |> 
  mutate(across(where(is.numeric), \(x) percent(x, accuracy = 1))) 
{% endhighlight %}

That gets us these interesting results:

<table class=" lightable-material" style='font-family: "Source Sans Pro", helvetica, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;"> Context </th>
   <th style="text-align:left;"> Proportion of p-values that are fragile </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Difference is as expected during power calculation </td>
   <td style="text-align:left;"> 27% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Difference is random, but on average is as expected </td>
   <td style="text-align:left;"> 16% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Difference is as expected, except half the time null hypothesis is true </td>
   <td style="text-align:left;"> 29% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Difference is random, AND null hypothesis true half the time </td>
   <td style="text-align:left;"> 20% </td>
  </tr>
</tbody>
</table>

<hr>

There's a marked variation here in what proportion of <i>p</i> values is fragile. Arguably, the fourth of these scenarios is the closest approximation to the real world (although there is a lot of debate about this, how much are exactly-zero differences really plausible?) Either this, or the other realistic scenario ('difference is random but on average is as expected') gives a proportion of fragile <i>p</i> values well below the 27% we saw in our base scenario.

### Conclusion

There's just too many factors impacting on the proportion of <i>p</i> values that will be between 0.01 and 0.05 to assume that variations in it are either an improvement or a worsening in research practices. These things include:

* When expected differences change and sample sizes change to go with them for a given level of power, it impacts materially on the proportion of fragile <i>p</i> values we'd expect to see
* When the real world differs from that expected by the researcher when they did their power calculation, it impacts materially on the proportion of fragile <i>p</i> values we'd expect to see
* Anyway, researchers don't all set their sample sizes to give 80% power, for various reasons, some of them good and some not so good

Final thought &mdash; none of the above tells us whether we have a replication crisis or not, and if so if it's getting better or getting worse. As it happens, I tend to think we do have one and that it's very serious. I think the peer review process works very poorly and [could be improved](/blog/2020/06/13/publication-reform), and academic publishing in general sets up terrible &mdash; and perhaps worsening &mdash; incentives. However, I think criticism in the past decade or so has led to improvements (such as more access to reproducible code and data, more pre-registration, general raised awareness), which is consistent really with Bogdan's substantive argument here. I just don't think the 'fragile' <i>p</i> values are much evidence either way, and if we monitor them at all we should do so with great caution.
