# to install gganimate:
devtools::install_github('thomasp85/gganimate')
library(tidyverse)
library(ineq)
library(gganimate)
library(GGally)
library(WDI)
library(MASS)
library(mgcv)
library(knitr)

#===================Real data=================================
# Find the indicators we need with commands like:
# WDIsearch("gini") %>% View
# This gives us:
#  - GINI index (World Bank estimate)
#  - GNI per capita (constant 2010 US$)
#  - Life expectancy at birth, total (years)

# Download data:
country_data_orig <- WDI(indicator = c("SI.POV.GINI", "NY.GNP.PCAP.KD", "SP.DYN.LE00.IN"), 
                         country = "all", start = 1960, end = 2020)


country_data <- country_data_orig %>%
  as_tibble() %>%
  rename(inequality =SI.POV.GINI, income = NY.GNP.PCAP.KD, life =  SP.DYN.LE00.IN) %>%
  mutate(log_income = log(income),
         log_life = log(life),
         log_inequality = log(inequality)) %>%
  filter(iso2c != "" & !is.na(income) & !is.na(life)) %>%
  # limit ourselves to years with at least five observations (otherwsie 1983 has just 1):
  group_by(year) %>%
  filter(sum(!is.na(inequality)) > 5) %>%
  ungroup() %>%
  mutate(year_factor = as.factor(year),
         year = as.integer(year))

# All three variables (and log of income) in one recent year
svg("../img/0136-pairs.svg", 9, 7)
country_data %>%
  filter(year == max(year) - 1)  %>%
  dplyr::select(income, log_income, inequality, life) %>%
  ggpairs() +
  ggtitle("Relationship between three key development indicators",
          paste("Income, life expectancy, and inequality (Gini coefficient) in", max(country_data$year) - 1)) +
  labs(caption = "Source: World Bank, World Development Indicators")
dev.off()


#-----------------------draw animations--------------
# life expectancy and income, all years (animation)

a1 <- ggplot(country_data, aes(x = income, y = life)) +
  scale_x_log10("Gross National Income per capita, 2000 US dollars", label = dollar) +
  geom_point(size = 2.5, aes(colour = country)) +
  geom_smooth(method = "gam") +
  ggtitle("A positive relationship between country-level income and life expectancy",
          subtitle = 'Relationship holds in all years for which data are available\nYear: {frame_time}') +
  labs(y = 'Life expectancy',
       caption = "Source: World Bank, World Development Indicators") +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() + 
  exit_shrink() +
  coord_cartesian(ylim = c(25, 90)) +
  theme(legend.position = "none")

# reality check - who has that big up-and-down in life expectancy?
country_data %>%
  group_by(country) %>%
  summarise(volatility =  sd(life, na.rm = TRUE)) %>%
  arrange(desc(volatility))

a2 <- ggplot(country_data, aes(x = inequality, y = life)) +
  scale_x_log10("Inequality (Gini coefficient)") +
  geom_point(size = 2.5, aes(colour = country)) +
  geom_smooth(method = "gam") +
  ggtitle("A weaker negative relationship between inequality and life expectancy",
          subtitle = 'Relationship holds in years for which substantial data are available\nYear: {frame_time}') +
  labs(y = 'Life expectancy',
       caption = "Source: World Bank, World Development Indicators") +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() + 
  exit_shrink() +
  coord_cartesian(ylim = c(25, 90)) +
  theme(legend.position = "none")

a3 <- ggplot(country_data, aes(x = income, y = inequality)) +
  scale_x_log10("Gross National Income per capita, 2000 US dollars", label = dollar) +
  geom_point(size = 2.5, aes(colour = country)) +
  geom_smooth(method = "gam") +
  ggtitle("A variable relationship between income and inequality",
          subtitle = 'Relationship is negative in years for which substantial data are available\nYear: {frame_time}') +
  labs(y = 'Inequality (Gini coefficient)',
       caption = "Source: World Bank, World Development Indicators") +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() + 
  exit_shrink() +
  coord_cartesian(ylim = c(15, 70)) +
  theme(legend.position = "none")

nf <- 500
animate(a1, nframes = nf, fps = nf / 10, 
        renderer = gifski_renderer(file = "../img/0136-income-life.gif"), width = 500, height = 450)

animate(a2, nframes = nf, fps = nf / 10, 
        renderer = gifski_renderer(file = "../img/0136-inequality-life.gif"), width = 500, height = 450)

animate(a3, nframes = nf, fps = nf / 10, 
        renderer = gifski_renderer(file = "../img/0136-income-inequality.gif"), width = 500, height = 450)



#-------------------------"unexplained" life expectancy-----------------

model1 <- lm(life ~ year_factor * log_income, data = country_data)
par(mfrow = c(2, 2), bty = "l")
plot(model1)



model2 <- gam(life ~ year_factor + s(log_income, year_factor, bs = "fs"), 
              data = country_data)
summary(model2) # R squared of 0.76
summary(model1) # R squared of 0.72

country_data$unexplained_life <- residuals(model2)

a4 <- ggplot(country_data, aes(x = inequality, y = unexplained_life)) +
  scale_x_log10("Inequality (Gini coefficient)") +
  geom_point(size = 2.5, aes(colour = country)) +
  geom_smooth(method = "gam") +
  ggtitle("Life expectancy after controlling for income, compared to inequality",
          subtitle = 'No clear relationship between inequality and residual life expectancy\nYear: {frame_time}') +
  labs(y = 'Unexplained life expectancy\n(residuals after modelling life expectancy on GNI per capita with a GAM)',
       caption = "Source: World Bank, World Development Indicators") +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() + 
  exit_shrink() +
  coord_cartesian(ylim = c(-25, 25)) +
  theme(legend.position = "none")

animate(a4, nframes = nf, fps = nf / 10, 
        renderer = gifski_renderer(file = "../img/0136-inequality-unexplained-life.gif"), width = 500, height = 450)




#======================Simulated data====================================
#' Generate a log normal distribution
#' 
#' @details Generates a log normal distribution of mean \code{mu} and inequality that will depend on \code{sd}.
#' @param n sample size
#' @param mu mean of the log-normal distribution
#' @param sd standard deviation of the underlying normal distribution
rinc <- function(n = 1000, mu = 20000, sd = 1){
  x <- exp(rnorm(n, mean = log(mu), sd = sd))
  x <- x - mean(x) + mu
  return(x)
}

#--------------------Simple two-country situation--------------------------
n <- 1000
x1 <- rinc(n = n, sd = 0.8)
x2 <- rinc(n = n, sd = 0.4)
data1 <- data_frame(
  income = c(x1, x2),
  country = rep(c("Unevenland", "Equaltopia"), each = n)
) %>%
  mutate(lifespan = ifelse(income < 10000, rnorm(n(), 50, 7), rnorm(n(), 70, 10)))

ginis <- data1 %>%
  group_by(country) %>%
  summarise(ineq = Gini(income))

data1a <- data1 %>%
  left_join(ginis, by = "country") %>%
  mutate(label = paste0(country, ": Gini = ", round(ineq, 2)))

svg("../img/0136-sim-density.svg", 8, 5)
data1a %>%
  ggplot(aes(x = income, fill = label)) +
  geom_density(alpha = 0.5, colour = NA) +
  scale_x_continuous(label = dollar) +
  labs(fill = "") +
  ggtitle("Two simulated income distributions",
          "Statistical density plots")
dev.off()

svg("../img/0136-sim-lorenz.svg", 8, 5)
data1a %>%
  group_by(country) %>%
  arrange(income) %>%
  mutate(cum_income = cumsum(income),
         cum_inc_prop = cum_income / max(cum_income),
         inc_level = 1:n() / n()) %>%
  ggplot(aes(x = inc_level, y = cum_inc_prop, fill = label)) +
  facet_wrap(~country) +
  geom_line() +
  geom_abline(slope = 1) +
  geom_ribbon(aes(x = inc_level, ymin = cum_inc_prop, ymax = inc_level),
              alpha = 0.5) +
  coord_equal() +
  ggtitle("Two simulated income distributions",
          "Lorenz curve plots; shaded area shows the gap to pure equality.") +
  labs(x = "Cumulated proportion of people, poorest to richest",
       y = "Cumulative income received",
       fill = "")
dev.off()

svg("../img/0136-sim-income-life.svg")
ggplot(data1, aes(x = income, y = lifespan, colour = country)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous("Individual income", label = dollar) +
  labs(y = "Individual life span",
       colour = "Country") +
  ggtitle("Relationship between life expectancy and income",
          "Simulated data, simplified relationship where $10k per year is a threshold for health benefits")
dev.off()

data1 %>%
  group_by(country) %>%
  summarise(Inequality = round(Gini(income), 2),
            `Mean income` = mean(income),
            `Mean life expectancy` = round(mean(lifespan), 1)) %>%
  kable()


#-------------------------Multi country situation---------------------------

# a more sophisticated relationship of income to life expectancy:
x <- 0 :100000
y <- 50 + x ^ 0.5 / 7

# visual demo of relationship:
svg("../0136-sim-curve.svg", 8, 5)
par(family = main_font, font.main = 1, bty = "l")
plot(x, y, type = "l", xlab = "Income", ylab = "Average life span",
     main = "Simulated dimininishing marginal impact of income on mean life expectancy",
     xaxt = "n")
marks <- seq(from = 0, to = 100000, length.out = 6)
axis(1,at = marks,labels = paste0("$", format(marks, scientific = FALSE, big.mark = ",")))
grid()
dev.off()

data2 <- data_frame(country = rep(paste("Country", 1:100), each = n),
                    country_mean = rep(rnorm(100, 20000, 5000), each = n),
                    country_sd =   rep(runif(100, 0.1, 0.9), each = n)) %>%
  group_by(country) %>%
  mutate(income = rinc(n(), mu = unique(country_mean), sd = unique(country_sd))) %>%
  ungroup() %>%
  mutate(lifespan = rnorm(n(), 50 + pmax(0, income) ^ 0.5 / 7, sd = 5))


data3 <- data2 %>%
  group_by(country) %>%
  summarise(mean_income = mean(income),
            inequality = Gini(income),
            life_exp = mean(lifespan)) %>%
  ungroup() 

svg("../0136-sim-pairs.svg", 8, 7)
data3 %>%
  dplyr::select(-country) %>%
  ggpairs() +
  ggtitle("Relationship between three key development indicators",
          "Inequality appears to have a country-level impact on life expectancy, even though the relationship is purely via individual income.") +
  labs(caption = "Source: Simulated data where individual income impacts on life expectancy, but country level inequality has no direct effect.")
dev.off()
  

# model with scaled data to make it easier to compare the two coefficients
mod <- lm(life_exp ~ mean_income + inequality, data = as.data.frame(scale(dplyr::select(data3, -country))))
anova(mod)
summary(mod)
confint(mod)

convert_pngs("0136")
