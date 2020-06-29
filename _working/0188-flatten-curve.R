library(tidyverse)
library(matrixStats)

?dgamma

# The mean and variance are E(X) = a*s and Var(X) = a*s^2.

so 
a = m/s
a = v / s^2
v / s^2 = m / s
v / s = m
s = v/m
a = m/(v/m) = m^2 /v


dgammamm <- function(x, m, sd){
  v <- sd ^ 2
  s <- v / m
  a <- m ^ 2 / v
  dgamma(x, scale = s, shape = a)
}

# see https://twitter.com/NateSilver538/status/1277348469028642818

d <- tibble(x = as.numeric(1:100)) %>%
  mutate(red = dgammamm(x, 25, 12),
         blue = dgammamm(x, 50, 24)) 
#health care capacity
hcc <- max(d$blue * 1.05)

anots <- tibble(
  txt = c("Healthcare system capacity",
          "Without\nProtective\nMeasures",
          "With Protective\nMeasures"),
  x = c(75, weightedMedian(d$x, w = d$red), weightedMedian(d$x, w = d$blue) + 6),
  y = c(hcc * 1.1, hcc * 1.1, hcc / 2),
  col = c("steelblue", "white", "white"))

d %>%
  gather(variable, y, -x)  %>%
  ggplot(aes(x = x, y = y)) +
  geom_area(aes(fill = variable, colour = variable), alpha = 0.5, position = "identity") +
  geom_text(data = anots, aes(label = txt, colour = col)) +
  geom_hline(yintercept = hcc, linetype = 2, colour = "steelblue") +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void(14) +
  theme(axis.title.x = element_text()) +
  labs(x = "Time since first case")
