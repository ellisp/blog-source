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

# Line chart plot:
p1 <- res |> 
  mutate(pd_lab = glue("80% power planned for diff of {planned_diff}")) |> 
  ggplot(aes(x = actual_diff, y = prop_fragile)) +
  facet_wrap(~pd_lab) +
  geom_vline(aes(xintercept = planned_diff), colour = "steelblue") +
  geom_hline(yintercept = 0.26, colour = "orange") +
  geom_point() +
  geom_line() +
  scale_y_continuous(label = percent) +
  labs(y = "Proportion of significant p values that are between 0.01 and 0.05",
       x = "Actual difference (in standard deviations)",
       subtitle = "Vertical blue line shows where the actual difference equals the minimum difference to detect that the 80% power calculation was based upon.
Horizontal orange line shows the observed average proportion of 'fragile' p values in the recent psychology literature.",
       title = "Fragility of p values in relation to actual and planned differences in a two sample t test.")


# Heatmap:
p2 <- res |> 
  ggplot(aes(x = actual_diff, y = as.ordered(planned_diff), fill = prop_fragile)) +
  geom_tile() +
  geom_tile(data = filter(res, prop_fragile > 0.25 & prop_fragile < 0.31),
            fill = "white", alpha = 0.1, colour = "white", linewidth = 2) +
  scale_fill_viridis_c(label = percent, direction = -1) +
  theme(panel.grid.minor = element_blank()) +
  labs(y= "Smallest detectable difference for 80% power",
       x = "Actual difference (in standard deviations)",
       fill = "Proportion of significant p values that are between 0.01 and 0.05:",
       subtitle = "Sample size is based on 80% power for the difference on the vertical axis. White boxes indicate where the proportion of fragile significant p values
is between 25% and 31%.",
       title = "Fragility of p values in relation to actual and planned differences in a two sample t test.")


svg_png(p1, "../img/0296-lines", w = 11, h = 7)

svg_png(p2, "../img/0296-heatmap", w = 11, h = 7)
