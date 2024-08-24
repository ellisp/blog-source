library(tidyverse)
library(mgcv)

dr <- read_csv("duckplyr-results.csv")

count(dr, n, use_duckplyr, summary_op)

dr |>
  group_by(n, use_duckplyr) |>
  summarise(mean(elapsed), mean(user), mean(system))

dr |>
  group_by(n, , summary_op, use_duckplyr) |>
  summarise(elapsed = mean(elapsed, tr = 0.2))  |>
  spread(use_duckplyr, elapsed)

mod1 <- lm(log(elapsed + 0.0001) ~ log(n) + use_duckplyr + summary_op +as.factor(i), data = dr)
mod2 <- gam(log(elapsed + 0.0001) ~ s(log(n), k = 4) + use_duckplyr + as.factor(i), data = dr)

summary(mod1)
par(mfrow=c(2,2)); plot(mod1)
confint(mod1)

summary(mod2)
anova(mod2)

ggplot(dr, aes(x = n, y = elapsed, colour = use_duckplyr)) +
  facet_wrap(~summary_op) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
  geom_point(aes(colour = use_duckplyr)) +
  scale_x_log10() +
  scale_y_log10()
