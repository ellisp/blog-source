library(pwrss)
library(tidyverse)
library(kableExtra)

set.seed(123)

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

# power from simulation
1 - mean(res > 0.05)

# power from Bulus' package
pwrss.t.2means(mu1 = 0.55, sd1 = 1, sd2 = 1, n2 = 53)

# Of those experiments that have 'significant' results, what proportion are in 
# the so-called fragile range (i.e. betwen 0.01 and 0.05)
summ1 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)
print(summ1)

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
  prop_fragile[j] <- sum(res > 0.01 & res < 0.05) / sum(res < 0.05)
}

pwrss.t.2means(mu1 = possible_diffs[10], n2 = n_for_power[10])
pwrss.t.2means(mu1 = possible_diffs, n2 = n_for_power)


p1 <- tibble(prop_fragile, possible_diffs) |> 
  ggplot(aes(x = possible_diffs,y= prop_fragile)) +
  geom_point()+
  scale_y_continuous(label = percent) +
  labs(x = "Difference (in standard deviations) between two means",
       y = "Proportion of significant p values \nthat are between 0.01 and 0.05",
       title = "Two sample tests for difference between two means with power = 80%",
       subtitle = "t test for independent samples at a combination of sample size and population difference\nneeded to give the desired power. Both populations are standard normal distributions.")


p2 <- tibble(prop_fragile, n_for_power) |> 
  ggplot(aes(x = n_for_power,y = prop_fragile)) +
  geom_point() +
  scale_x_sqrt() +
  scale_y_continuous(label = percent) +
  labs(x = "Sample size needed to get 80% power for given difference of means",
       y = "Proportion of significant p values \nthat are between 0.01 and 0.05",
       title = "Two sample tests for difference between two means with power = 80%",
       subtitle = "t test for independent samples at a combination of sample size and population difference\nneeded to give the desired power. Both populations are standard normal distributions.")


# svg_png(p1, "../img/0295-fragile-diff", w = 8, h = 5)
# svg_png(p2, "../img/0295-fragile-n", w = 8, h = 5)

#------------------when true d isn't what was expected---------------

reps <- reps
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
print(summ2)


#---------when true d is same as expected except half the time H0 is true---------

for(i in 1:reps){
  res[i] <- experiment(d = sample(c(0, 0.55), 1), n1 = 53)
}


# proportion of so-called fragile p values is now *more*
summ3 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)
print(summ3)


#---------when true d is random, AND half the time H0 is true---------

for(i in 1:reps){
  res[i] <- experiment(d = sample(c(0, rnorm(1, 0.55, 0.5)), 1), n1 = 53)
}


# proportion of so-called fragile p values is now less
summ4 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)
print(summ4)

#----------as above but power was chosen for minimum difference of 0.1


min_detectable_diff <- 0.2
stopifnot(min_detectable_diff %in% possible_diffs)

for(i in 1:reps){
  res[i] <- experiment(d = sample(c(0, rnorm(n = 1, 
                                             mean = min_detectable_diff * 2, 
                                             0.5)), 
                                  size = 1, prob = c(0.5, 0.5)), 
                       n1 = n_for_power[possible_diffs == min_detectable_diff])
}


# proportion of so-called fragile p values is now less
summ5 <- mean(res > 0.01 & res < 0.05) / mean(res < 0.05)
print(summ5)

#----------------print summary---------------

tibble(`Context` = c(
  "Difference is as expected during power calculation",
  "Difference is random, but on average is as expected",
  "Difference is as expected, except half the time null hypothesis is true",
  "Difference is random, AND null hypothesis true half the time",
  "Difference is random, larger than planned, and null hypothesis true half the time"
), `Proportion of p-values that are fragile` = c(summ1, summ2, summ3, summ4, summ5)) |> 
  mutate(across(where(is.numeric), \(x) percent(x, accuracy = 1))) |> 
  kable() |> 
  kable_material() |> 
  writeClipboard()

