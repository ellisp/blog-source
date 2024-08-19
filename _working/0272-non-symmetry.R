library(tidyverse)
# based on https://mastodon.social/@tslumley@fediscience.org/112980359413593554
#-------------perturbing a skewed discrete distribution-------------
set.seed(123)
n <- 1e7
x <- rbinom(n, 8, 0.25)
e <- rnorm(n, 0, 0.1)
y <- x + e

round(c(mean(x), median(x)), 3)
round(c(mean(y), median(y)), 3)

# ------------------perturbing a skewed continues distribution
# Make a mixture distribution, 1 part normal and 10 parts standard exponential
# normal, and return the difference between the median and mean
mixture <- function(params, n = 1e6){
  set.seed(123)
  x1 <- exp(rnorm(n))
  x2 <- rnorm(round(n /10), params[1], params[2])
  x3 <- c(x1, x2)
  y <- abs(median(x3)- mean(x3))
  return(y)
}

# Find the set of parameters for the Normal part of a mixture
# that results in median and mean being as close as possible:
best <- optim(c(0.5, 1), mixture)
best

# Generate data from that micture
n <- 1e6
x <- c(rnorm(n/10, best$par[1], best$par[2]), exp(rnorm(n)))

# Mean and median are very close
c(mean(x), median(x))

tibble(x = x) |>
  sample_n(1000) |>
  ggplot(aes(x = x)) +
  geom_rug() +
  geom_density(fill = "steelblue", alpha = 0.5)

tibble(x = x) |>
  sample_n(1000) |>
  ggplot(aes(x = x)) +
  geom_rug() +
  geom_density(fill = "steelblue", alpha = 0.5) +
  scale_x_continuous(transform = scales::modulus_trans(p=0))

# perturb it a little
y <- x + rnorm(length(x))
# now the median has shifted but mean has stayed the same:
c(mean(y), median(y))
# compared to original:
c(mean(x), median(x))

# "Basically, to keep mean and median equal you need at least local symmetry
# around the mean. Otherwise the mean stays the same and the median moves"

set.seed(124)
p3 <- tibble(`Original skewed variable` = x, `With extra jitter` = y) |>
  sample_n(10000) |>
  gather(variable, value) |>
  ggplot(aes(x = value, colour = variable, fill = variable)) +
  #geom_rug() +
  geom_density(alpha = 0.5) +
  scale_x_continuous(transform = scales::modulus_trans(p=0),
                     breaks = c(-40, -20, -10, -5, -2, -1,  0, 1, 2, 5, 10, 20, 40)) +
  geom_vline(xintercept = mean(x), colour = "red") +
  geom_vline(xintercept = median(y), colour = "steelblue")  +
  annotate("text", x = 1.3, y = 0.6, label = "Post-jitter median", hjust = 0,
           colour = "steelblue") +
  annotate("text", x = 0.86, y = 0.6, 
           label = "Original equal mean and median, also post-jitter mean", 
           hjust = 1, colour = "red") +
  labs(x = "Value (modulus transformed scale)", y = "Density", colour = "", fill = "",
       title = "Adding jitter to a mixture of a skewed and symmetrical distributions",
       subtitle = "The mean stays the same with the jitter, but the median moves if the distribution wasn't symmetrical around the original mean.",
       caption = "Based on an idea in a toot by Thomas Lumley") 


svg_png(p3, "../img/0272-both", w = 9.1, h = 5)


p4 <- p3 + 
  scale_x_continuous(limits = c(-10, 10)) +
  labs(x = "Value (untransformed, axis truncated)")
svg_png(p4, "../img/0272-both-untransformed", w = 9.1, h = 5)

# It's easy to think "doesn't the jitter, which is symmetrical, just make some
# observations go up and some go down by on average 0, so why is the 50% point
# moving? The new distribution is *less skewed* - more normal, because of the CLT
# - although still with the strange mixtrue that dragged the mean to the left