---
layout: post
title: More on power and 'fragile' p-values
date: 2025-06-14
tag: 
   - Simulations
   - Reproducibility
description: Some more comprehensive simulations of what happens to 'fragile' <i>p</i> values (those between 0.01 and 0.05), when the actual power differs from the minimum detectable difference that an 80% power calculation was depended upon to set the sample size.
image: /img/0296-lines.svg
socialimage: https:/freerangestats.info/img/0296-lines.png
category: R
---

Last week I posted about the proportion of <i>p</i> values that will be 'fragile' under various types of scientific experiments or other inferences. The proportion of <i>p</i> values that is fragile had been defined as the proportion that are between 0.01 and 005. I was reacting to the question of whether it's a good thing that the proportion fragile in the psychology literature seems to have declined over a decade or two from about 32% to about 26%. I had concluded that this didn't really tell us much about progress in response to the replication crisis.

There was some good discussion on BlueSky when I posted on this and I would now qualify my views a little. The key points that people made that I hadn't adequately taken into account in my presentation were:
* When scientists determine a suitable sample size by a power calculation, they are most often basing it on the sample size needed to give a certain amount of power for the "minimum difference we want to detect", rather than the difference they are actually expecting
* One of my key plots showed an apparent increase in fragile <i>p</i> values even with constant power when the planned-for difference between means was large &mdash; when what was actually happening was probably an artefact of the tiny sample sizes in this very artificial situation (to be honest, still not sure as I write this exactly what was happening, but am pretty satisfied that it's not terribly important whatever it is, and certainly doesn't deserve to be my main plot on the topic)
* More general thinking about "if this decrease in 'fragile' <i>p</i> values isn't showing something positive, then what is it showing?" which gives me a slightly more nuanced view.

This made me decide I should look more systematically into the difference between "planned for" difference between two samples and the "actual" difference in the population. I had done some simulations of random variation of the actual difference from that planned for, but not systematic and comprehensive enough.

What I've done now is systematically compare every combination of a sampling strategy to give 80% power for "minimum detectable differences" from 0.05 to 0.60 (12 different values, spaced 0.05 apart), and actual differences of mean between 0.0 and 1.0 (11 different values, spaced 0.10 apart). With 10,000 simulations at each combination we have 12 x 11 x 10,000 = 1.32 million simulations

<object type="image/svg+xml" data='/img/0296-lines.svg' width='100%'><img src='/img/0296-lines.png' width='100%'></object>

We can see that many reasonable combinations of "planned for" and "actual" differences between the two populations give fragile proportions of <i>p</i> values that are quite different from 26%. In particular, in the situation where the actual difference is more than the "minimum detectable difference" that the sample size would have been calculated for 80% power (which is probably what most researchers are aiming for), the proportion fragile quickly gets well below 26%.

That proportion ranges from 80% when the real difference is zero (i.e. the null hypothesis is true) and the <i>p</i> value distribution is uniform over [0,1]; to well below 10% when the sample size is ample for high (much greater than 80%) power, sufficient to pick up the real difference between the two populations.

I think this is better at presenting the issues than my blog last week. Here's how I think about this issue now:

* Around 26% of <i>p</i> values will indeed be 'fragile' when the sample size has been set to give 80% power on the basis of a detectable difference between two populations which is indeed roughly what the actual difference turns out to be.
* In general, a proportion of fragile <i>p</i> values that is higher than this suggests that experiments are under-powered by design, or decent designs with 80% power turned out to be based on minimum-detectalbe differences that are often not actual differences in reality, or something else sinister is going on.
* If in fact many or most experiments are based on realities where the actual difference between means is more than the minimum-detectable difference the samples size was chosen for at 80% power, we'd expect a proportion of <i>p</i> values that are fragile to be somewhat less than 26%.
* Taking these together, it *is* reasonable to say that <i>p</i> values declining from 32% to 26% is a good thing; but that 26% probably is still a bit too high and should not be appreciated to be a very artificial benchmark; and that we can't be sure what's driving the decline.

Here's the R code to run those 1.32 million simulations, making using of the `foreach` and `doParallel` R packages for parallel computing to speed things up a bit:

{% highlight R lineanchors %}
library(pwrss)
library(tidyverse)
library(glue)
library(foreach)
library(doParallel)

#' Function to run a two sample experiment with t test on difference of means
#' @returns a single p value
#' @param d difference in means of the two populations that samples are drawn
#'   from. If sd1 and sd2 are both 1, then d is a proportion of that sd and
#'   everything is scaleless.
experiment <- function(d, m1 = 0, sd1 = 1, sd2 = 1, n1 = 50, n2 = n1, seed = NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  x1 <- rnorm(n1, m1, sd1)
  x2 <- rnorm(n2, m1 + d, sd2)
  t.test(x1, x2)$p.value
}


#--------------varying difference and sample sizes---------------
pd1 <- tibble(planned_diff = seq(from = 0.05, to = 0.60, by = 0.05)) |> 
  # what sample size do we need to have 80% power, based on that planned
  # "minimum difference to detect"?:
  mutate(n_for_power = sapply(planned_diff, function(d){
    as.numeric(pwrss.t.2means(mu1 = d, power = 0.8, verbose = FALSE)$n[1])
  }))

# the actual differences, which can be from zero to much bigger than the minimum
# we planned power to detect:
pd2 <- tibble(actual_diff = seq(from = 0, to = 1.0, by = 0.1))

# Number of simulations we do for each combination of planned power (based on a
# given 'minimum difference to detect') and actual power (based on the real
# difference). when the real difference is zero in particular, there will only
# be 1/20 of reps that come up with a 'significant' difference, so 10000 reps in
# total gives us a sample of 500 signficant tests to get the proportion that are
# fragile from, so still not huge. If I'd bothered I could have changed the
# number of reps to do for each combination based on some number that's really
# needed, but I didn't bother.:
reps_each <- 10000

# combine the planned-for and actual differences in a data frame with a row
# for each repeated sim we are going to do:
data <- expand_grid(pd1, pd2) |> 
  mutate(link = 1) |> 
  full_join(tibble(link = 1,
                   rep = 1:reps_each),
            relationship = "many-to-many", by = "link") |> 
  select(-link) |> 
  mutate(p = NA)


print(glue("Running {nrow(data)} simulations. This will take a while."))

# set up parallel processing cluster
cluster <- makeCluster(7) 
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(foreach)
})

clusterExport(cluster, c("data", "experiment"))

results <- foreach(i = 1:nrow(data), .combine = rbind) %dopar% {
  set.seed(i)
  
  # the row of data for just this simulation:
  d <- data[i, ]
  
  # perform the simulation and capture the p value:
  d$p <- experiment(d = d$actual_diff, 
             n1 = d$n_for_power,
             seed = d$rep)
  
  # return the result as a row of data, which will be rbinded into a single data
  # frame of all the parallel processes:
  return(d)
}

stopCluster(cluster)

#--------------summarise and present results--------------------------

# Summarise and calculate the proportions:
res <- results |> 
  group_by(planned_diff, n_for_power, actual_diff) |> 
  summarise(number_sig = sum(p < 0.05),
            prop_fragile = sum(p > 0.01 & p < 0.05) / number_sig)
{% endhighlight %}

Here's how I draw two plots to summarise that. I have the line plot shown above, and a heatmap that is shown below the code. Overall I think the line plot is easier and clearer to read.

{% highlight R lineanchors %}
# Line chart plot:
res |> 
  mutate(pd_lab = glue("80% power planned for diff of {planned_diff}")) |> 
  ggplot(aes(x = actual_diff, y = prop_fragile)) +
  facet_wrap(~pd_lab) +
  geom_vline(aes(xintercept = planned_diff), colour = "steelblue") +
  geom_hline(yintercept = 0.26, colour = "orange") +
  geom_point() +
  geom_line() +
  scale_y_continuous(label = percent) +
  labs(y = "Proportion of significant <i>p</i> values that are between 0.01 and 0.05",
       x = "Actual difference (in standard deviations)",
       subtitle = "Vertical blue line shows where the actual difference equals the minimum difference to detect that the 80% power calculation was based upon.
Horizontal orange line shows the observed average proportion of 'fragile' p values in the recent psychology literature.",
       title = "Fragility of p values in relation to actual and planned differences in a two sample t test.")


# Heatmap:
res |> 
  ggplot(aes(x = actual_diff, y = as.ordered(planned_diff), fill = prop_fragile)) +
  geom_tile() +
  geom_tile(data = filter(res, prop_fragile > 0.25 & prop_fragile < 0.31),
            fill = "white", alpha = 0.1, colour = "white", linewidth = 2) +
  scale_fill_viridis_c(label = percent, direction = -1) +
  theme(panel.grid.minor = element_blank()) +
  labs(y= "Smallest detectable difference for 80% power",
       x = "Actual difference (in standard deviations)",
       fill = "Proportion of significant p values that are between 0.01 and 0.05:",
       subtitle = "Sample size is based on 80% power for the difference on the vertical axis. White boxes indicate where the proportion of fragile significant p values is between 25% and 31%.",
       title = "Fragility of p values in relation to actual and planned differences in a two sample t test.")
{% endhighlight %}

Here's the heatmap we get from that. It's prettier but I think not actually as clear as the simpler, faceted line plot.

<object type="image/svg+xml" data='/img/0296-heatmap.svg' width='100%'><img src='/img/0296-heatmap.png' width='100%'></object>

That's all for now. I don't think there's any big implications of this. Just a better understanding of what proportion of <i>p</i> values we might expect to be in this fragile range and what impacts on it.

I'm planning on looking at some real life data, on a completely different topic, in my next post.
