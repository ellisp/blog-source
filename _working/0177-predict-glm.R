library(tidyverse)
library(scales)

n <- 50
set.seed(123)

d <- tibble(x = rnorm(n, 4, 1)) %>%
  mutate(mean = 2 + 0.25 * x,
         sd = sqrt(mean) / 20,
         y = exp(mean + rnorm(n, 0, sd))
  )

mod1 <- lm(log(y) ~ x, data = d)
mod2 <- glm(y ~ x, family = quasi(link = log, variance = mu), data = d)

# similar coefficients (one is minimising squares on the log scale, 
# the other is minimising weighted squares on the original)
rbind(coef(mod1), coef(mod2))


#------------Prediction interval from the log transform version--------------------
sx <- tibble(x = seq(from = 1, to = 7, length.out = 100)) 
pred1 <- predict(mod1, se.fit = TRUE, newdata = sx)

sx1 <- sx %>%
  mutate(se_pi = sqrt(pred1$se.fit ^ 2 + pred1$residual.scale ^ 2),
         lower_pi = exp(pred1$fit  - 1.96 * se_pi),
         upper_pi =exp(pred1$fit +  1.96 * se_pi))

p1 <- ggplot(d, aes(x = x)) +
  geom_ribbon(data = sx1, aes(ymin= lower_pi, ymax = upper_pi), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y))+
  labs(title = "Original data with prediction interval from log transform model")

#-----------------prediction interval from the quasi family GLM------------

pred2 <- predict(mod2, se.fit = TRUE, newdata = sx, type = "response")
sx2 <- sx %>%
  mutate(se_pi = sqrt(pred2$se.fit ^ 2 + pred2$fit * pred2$residual.scale ^ 2),
         lower_pi = pred2$fit - 1.96 * se_pi,
         upper_pi = pred2$fit + 1.96 * se_pi)

p2 <- ggplot(d, aes(x = x)) +
  geom_ribbon(data = sx2, aes(ymin= lower_pi, ymax = upper_pi), fill = "steelblue", alpha = 0.1) +
  geom_point(aes(y = y))+
  labs(title = "Original data with prediction interval from quasi likelihood glm with log link")


svg_png(p1, "../img/0177-log-transform", w = 8, h = 4)
svg_png(p2, "../img/0177-quasi-glm", w = 8, h = 4)


