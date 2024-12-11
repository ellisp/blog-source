library(tidyverse)
library(scales)
library(mgcv)

the_caption = "Source: example analysis of the diamonds dataset in the ggplot2 R package"

set.seed(123)
reps <- 200
results <- tibble(
  log_trans_adj = numeric(reps),
  untransformed = numeric(reps),
  irls1 = numeric(reps),
  irls2 = numeric(reps)
)
i <- 1
for(i in i:reps){
  samp <- sample_n(diamonds, 1000)
  
  mod1 <- lm(log(price) ~ log(carat) + color, data = samp)
  preds1 <- exp(predict(mod1, newdata = diamonds) + summary(mod1)$sigma ^ 2 / 2)
  
  mod2 <- lm(price ~ carat * color, data = samp)
  preds2 <- predict(mod2, newdata = diamonds)
  
  # this way biases it down because it gives too little weight to the
  # big values
  mod4 <- lm(price ~ carat * color, data = samp, 
              weights = 1 / pmax(min(samp$price), fitted(mod2)))
  
  preds4 <- predict(mod4, newdata = diamonds)

  mod5a <- lm(I(residuals(mod2) ^ 2) ~ carat * color, data = samp)
  mod5 <- lm(price ~ carat * color, data = samp, 
             weights = 1 / pmax(min(samp$price), fitted(mod5a)))
  
  preds5 <- predict(mod5, newdata = diamonds)
  results[i, ] <- cbind(mean(preds1), mean(preds2), mean(preds4), mean(preds5))
}

# Plot the standard diagnostics of the two models for the last example

true_value <- mean(diamonds$price)

p6 <- results %>%
  gather(variable, value) %>%
  mutate(variable = case_when(
    variable == "log_trans_adj" ~ "Model with log transformation and bias adjustment",
    variable == "untransformed" ~ "OLS",
    variable == "irls1" ~ "Reweighted individually",
    variable == "irls2" ~ "Reweighted proportionate to expected value"
  )) %>%
  ggplot(aes(x = value, colour = variable, fill = variable)) +
  facet_wrap(~variable, ncol = 1) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = true_value, colour = "red") +
  annotate("text", x = true_value, y = 0.0005, 
           label = "True value", hjust = 1, vjust = 0, size = 3, colour = "red") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparison of different estimation methods from a simple random sample",
    x = "Estimated value",
    colour = "",
    fill = "",
    subtitle = "log-log model is biased low; simple average has higher variance; model with no transformation is best.")

p6
svg_png(p6, "../img/0204-follow-up-model-comparison-density")

