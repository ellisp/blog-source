


library(tidyverse)
library(patchwork)

n <- 100
countries <- 9
d1 <- tibble(country = rep(LETTERS[1:countries], each = n),
             x = rnorm(n * countries) + as.numeric(countries) * .8,
             y = 1 + 0.5 * x + rnorm(n * countries) - as.numeric(countries) * .8)

d1_sum <- d1 |>
  group_by(country) |>
  summarise(x = mean(x),
            y = mean(y))

p1 <- ggplot(d1, aes(x = x, y = y)) +
  facet_wrap(~country)  +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")
             
p2 <- d1_sum |>
  ggplot(aes(x = x, y = y, label = country)) +
  geom_smooth(method = "lm") +
  geom_text()

p1 + p2
