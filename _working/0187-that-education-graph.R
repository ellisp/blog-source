library(MASS)
library(tidyverse)
library(scales)

# see the tweet at https://twitter.com/HaydenMaths/status/1275640960081375232
# by eye, the Deloitteline is about y = 19000 -9.5x
# 19000 + 1000 * b1 = 9500
# b1= (9500 - 19000) / 1000 = -9.5
# 
# so target values for b0 and b1 are 19000, -9.5

xbreaks <- 0:6 * 200
ybreaks <- 0:6 * 10000

n <- 100
set.seed(123)

x <- MASS::rnegbin(n * 10, 70, .5) + 1
x <- sort(c(sample(x[x < 350], 98, replace = TRUE), 460, 1150))

# Density of the x variable (not shown in blog)
ggplot(data.frame(x = x), aes(x = x)) + 
  geom_rug() + 
  geom_density() + 
  scale_x_continuous(breaks = xbreaks) +
  theme(panel.grid.minor = element_blank())


#' Simulate data and compare its OLS fit to a desired OLS fit
#' 
#' @param par vector of slope and intercept of log(y) ~ log(x)
#' @param df degrees of freedom for the t distribution of residuals of y on the log scale
#' @param sigma scaling value for residuals
#' @param infl amount that residuals are inflated when x is less than 100
fn <- function(par, df, sigma, infl){

  log_b0 <- par[1]
  log_b1 <- par[2]
  
  
  set.seed(123)
  eps <- rt(n, df)
  eps <- eps / sd(eps) * sigma * ifelse(x < 100, infl, 1)
  y <- exp(log_b0 + log_b1 * log(x) +  eps)
  #y[c(n, n-1)] <- c(9700, 14000)
  
  mod <- lm(y ~ x)
  how_bad <- 
    # intercept should be about 19000
    20 * ((coef(mod)[1] - 19000) / 19000) ^ 2 + 
    # slope should be about -9.5
    20 * ((coef(mod)[2] - -9.5) / 9.5) ^ 2 + 
    # maximum should be about 50000
    ((max(y) - 48000) / 48000) ^ 2  + 
    # 5th percentile should be about 10000
    ((quantile(y, 0.05) - 10000) / 10000) ^ 2 +
    # median should be about 15000:
    ((quantile(y, 0.5) - 15000) / 15000) ^ 2

  return(how_bad)
}

infl <- 1.8
sigma = 0.20
df = 13
best <- optim(c(10, -2), fn = fn, df = df, sigma = sigma, infl = infl)

# simulate data with the best set of parameters
set.seed(123)
eps <- rt(n, df)
eps <- eps / sd(eps) * sigma * ifelse(x < 100, infl, 1)
y <- exp(best$par[1] + best$par[2] * log(x) + eps)
y[c(n, n-1)] <- c(9700, 14000)

the_data <- data.frame(x, y)

p1 <- ggplot(the_data, aes(x = x, y = y)) +
  geom_point()  + 
  geom_smooth(method = "lm", formula = "y ~ x", colour = "white") +
  scale_x_continuous(breaks = xbreaks, label = comma) +
  scale_y_continuous(breaks = ybreaks, label = comma, limits = c(0, 60000)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Simulated data with various lines of best fit",
       subtitle = "Ordinary least squares")

svg_png(p1, "../img/0187-ols")

mod <- lm(y ~ x)
coef(mod) # should be close to 19000, -9.5
summary(mod)

p2 <- ggplot(the_data, aes(x = x, y = y)) +
  geom_point()  + 
  geom_smooth(method = "gam", colour = "white") +
  scale_x_continuous(breaks = xbreaks, label = comma) +
  scale_y_continuous(breaks = ybreaks, label = comma, limits = c(0, 60000)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Simulated data with various lines of best fit",
       subtitle = "Generalized Additive Model")

svg_png(p2, "../img/0187-gam")

p3 <- ggplot(the_data, aes(x = x, y = y)) +
  geom_point()  + 
  geom_smooth(method = "rlm", method.args = list(method = "MM"), colour = "white") +
  scale_x_continuous(breaks = xbreaks, label = comma) +
  scale_y_continuous(breaks = ybreaks, label = comma, limits = c(0, 60000)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Simulated data with various lines of best fit",
       subtitle = "MM-estimation of straight line")

svg_png(p3, "../img/0187-mm")


mod2 <- lm(log(y) ~ log(x), data = the_data)
pred2 <- predict(mod2, se.fit = TRUE)
the_data <- the_data %>%
  mutate(fit_log = exp(pred2$fit),
         lower_log = exp(pred2$fit - 1.96 * pred2$se.fit),
         upper_log = exp(pred2$fit + 1.96 * pred2$se.fit))

p4 <- ggplot(the_data, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lower_log, ymax = upper_log), fill = "grey60", alpha = 0.7) +
  geom_point()  + 
  geom_line(aes(y = fit_log), colour = "white") +
  scale_x_continuous(breaks = xbreaks, label = comma) +
  scale_y_continuous(breaks = ybreaks, label = comma, limits = c(0, 60000)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Simulated data with various lines of best fit",
       subtitle = "Ordinary Least Squares after log transforms, shown on original scale")

svg_png(p4, "../img/0187-log")


p5 <- ggplot(the_data, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lower_log, ymax = upper_log), fill = "grey60", alpha = 0.7) +
  geom_point()  + 
  geom_line(aes(y = fit_log), colour = "white") +
  scale_x_log10(breaks = xbreaks, label = comma) +
  scale_y_log10(breaks = ybreaks, label = comma) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Simulated data with various lines of best fit",
       subtitle = "Ordinary Least Squares after log transforms, shown with log scales")

svg_png(p5, "../img/0187-log-log")

