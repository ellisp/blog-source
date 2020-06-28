library(tidyverse)

# see https://twitter.com/NateSilver538/status/1277348469028642818

d <- tibble(x = as.numeric(0:100)) %>%
  mutate(red = dnorm(x, 25, 10),
         blue = dnorm(x, 50, 20)) %>%
  gather(variable, y, -x) 

#health care capacity
hcc <- max(filter(d, variable == "blue")$y * 1.05)

anots <- tibble(
  txt = c("Healthcare system capacity",
          "Without\nProtective\nMeasures",
          "With Protective\nMeasures"),
  x = c(75, 25, 55),
  y = c(hcc * 1.1, hcc * 1.1, hcc / 2),
  col = c("steelblue", "white", "white"))

ggplot(d, aes(x = x, y = y)) +
  geom_area(aes(fill = variable, colour = variable), alpha = 0.5, position = "identity") +
  geom_hline(yintercept = hcc, linetype = 2, colour = "steelblue") +
  geom_text(data = anots, aes(label = txt, colour = col)) +
  scale_colour_identity() +
  scale_fill_identity() +
  theme_void(14) +
  theme(axis.title.x = element_text()) +
  labs(x = "Time since first case")
