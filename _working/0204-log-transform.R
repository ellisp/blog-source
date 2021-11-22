library(tidyverse)
library(patchwork)

ggplot(diamonds, aes(x = price)) +
  geom_density() +
  labs(title = "Price has a nicely skewed rightwards distribution")

ggplot(samp, aes(y = price, x = carat, colour = color)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(title = "A linear model of the untransformed has heteroskedasticity challenges")

ggplot(samp, aes(y = price, x = carat, colour = color)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()  +
  geom_smooth(method = "lm") +
  labs(title = "A linear model after log transforms is better from a model-building perspective",
       sutbile = "Standard assumptions justifiung ordinary least squares make more sense after transformation")


set.seed(123)
reps <- 1000
results <- tibble(
  log_trans = numeric(reps),
  untransformed = numeric(reps),
  naive = numeric(reps)
)

for(i in 1:reps){
  samp <- sample_n(diamonds, 1000)
  
  mod1 <- lm(log(price) ~ log(carat) + color, data = samp)
  preds1 <- predict(mod1, newdata = diamonds)
  
  mod2 <- lm(price ~ carat + color, data = samp)
  preds2 <- predict(mod2, newdata = diamonds)
  
  mod3 <- mean(samp$price)
  
  results[i, ] <- cbind(mean(exp(preds1)), mean(preds2), mod3)
}

true_value <- mean(diamonds$price)

p1 <- ggplot(results, aes(x = naive, y = untransformed)) +
  geom_point() +
  coord_equal() +
  annotate("point", x = true_value, y = true_value, size = 3, colour = "red") +
  annotate("label", x = true_value, y = true_value - 10, 
           label = "True value", hjust = 1, size = 3, colour = "red") +
  labs(x = "Based on simple average from sample",
       y = "Based on untransformed model",
       subtitle = "Simple average leads to higher variance than model-based estimate",
       title = "Comparison of different estimation methods from a simple random sample")

p2 <- ggplot(results, aes(x = log_trans, y = untransformed)) +
  geom_point() +
  coord_equal() +
  annotate("point", x = true_value, y = true_value, size = 3, colour = "red") +
  annotate("text", x = true_value, y = true_value - 10, 
           label = "True value", hjust = 1, size = 3, colour = "red") +
  labs(x = "Based on log-log model",
       y = "Based on untransformed model",
       subtitle = "Log-log model leads to underestimates")


p3 <- results %>%
  gather(variable, value) %>%
  mutate(variable = case_when(
    variable == "log_trans" ~ "Model with log transformation",
    variable == "naive" ~ "Simple average of sample",
    variable == "untransformed" ~ "Model with no transformation"
  )) %>%
  ggplot(aes(x = value, colour = variable, fill = variable)) +
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
p3

p1 + p2
