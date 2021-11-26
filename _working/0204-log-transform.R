library(tidyverse)
library(patchwork)
library(scales)

the_caption = "Source: example analysis of the diamonds dataset in the ggplot2 R package"

set.seed(321)
samp <- sample_n(diamonds, 1000)

p1 <- ggplot(samp, aes(x = price)) +
  geom_density(fill = "steelblue", alpha = 0.5, colour = NA) +
  geom_rug() +
  labs(title = "Distribution of diamond price is skewed rightwards",
       subtitle = "(Like many economic and financial variables)",
       caption = the_caption,
       x = "Price of an individual diamond") +
  scale_x_continuous(label = dollar)

p2 <- ggplot(samp, aes(y = price, x = carat, colour = color)) +
  geom_smooth(method = "lm") +
  geom_point()  +
  scale_y_continuous(label = dollar_format(accuracy = 1))+
  labs(title = "A linear model of the untransformed price has heteroskedasticity challenges",
       subtitle = "Model of form price ~ carat * cut.\nREsiduals are right-skewed and variance increases as expected price increases, breaking the Gauss-Markov assumptions.",
       caption = the_caption, 
       x = "Weight of the diamond (carats)",
       y = "Price of an individual diamond")

p3 <- ggplot(samp, aes(y = price, x = carat, colour = color)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10(label = dollar_format(accuracy = 1))  +
  geom_smooth(method = "lm") +
  labs(title = "A linear model after log transforms is better from a model-building perspective",
       subtitle = "Model of form log(price) ~ log(carat) + cut.\nStandard assumptions justifying ordinary least squares make more sense after transformation.",
       caption = the_caption, 
       x = "Weight of the diamond (carats)",
       y = "Price of an individual diamond")


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

# Plot the standard diagnostics of the two models for the last example

p7 <- function(){
  par(mfrow = c(2, 2), bty = "l")
  plot(mod1)
  } # conventionally good

p8 <- function(){
  par(mfrow = c(2, 2), bty = "l")
  plot(mod2)
  } # unsatisfactory - residuals with curvature, non-normal, and increase as values increase

true_value <- mean(diamonds$price)

p4 <- ggplot(results, aes(x = naive, y = untransformed)) +
  geom_point() +
  coord_equal() +
  annotate("point", x = true_value, y = true_value, size = 3, colour = "red") +
  annotate("label", x = true_value, y = true_value - 10, 
           label = "True value", hjust = 1, size = 3, colour = "red") +
  labs(x = "Based on simple average from sample",
       y = "Based on untransformed model",
       subtitle = "Simple average leads to higher variance than model-based estimate",
       title = "Comparison of different estimation methods from a simple random sample")

p5 <- ggplot(results, aes(x = log_trans, y = untransformed)) +
  geom_point() +
  coord_equal() +
  annotate("point", x = true_value, y = true_value, size = 3, colour = "red") +
  annotate("text", x = true_value, y = true_value - 10, 
           label = "True value", hjust = 1, size = 3, colour = "red") +
  labs(x = "Based on log-log model",
       y = "Based on untransformed model",
       subtitle = "Log-log model leads to underestimates")


p6 <- results %>%
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


svg_png(p1, "../img/0204-distribution")
svg_png(p2, "../img/0204-untransformed")
svg_png(p3, "../img/0204-transformed")
svg_png(p7, "../img/0204-diagnostics-transformed")
svg_png(p8, "../img/0204-diagnostics-untransformed")
svg_png(p4 + p5, "../img/0204-model-comparison-scatter")
svg_png(p6, "../img/0204-model-comparison-scatter")

