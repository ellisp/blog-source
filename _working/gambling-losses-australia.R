library(Cairo)
CairoWin()

library(tidyverse)
library(ggrepel)

pokies <- tibble(
  state = c('NSW', 'ACT', 'Qld', 'SA', 'Tas', 'Vic', 'WA'),
  machines = c(11.54, 9.14, 9.05, 7.44, 6.71, 4.46, 0.95),
  losses = c(1600, 768.97, 1100, 887.47, 737.92, 1100, 655.53)
)

ggplot(pokies, aes(x = machines, y = losses)) +
  geom_smooth(method = "lm", colour = "white") +
  geom_point() +
  geom_text_repel(colour = "blue", aes(label = state)) +
  labs(y = "Gambling expenditure per capita, 2018-19",
        x = "Poker machines per 1k population, 2018-19") +
  scale_y_continuous(label = dollar)
