library(tidyverse)
library(ineq)
library(GGally)

rinc <- function(n = 1000, mu = 20000, sd = 1){
  x <- exp(rnorm(n, mean = log(mu), sd = sd))
  x <- x - mean(x) + mu
  return(x)
}

plot(density(rinc()))
mean(rinc())     
Gini(rinc(sd = 0.1))

n <- 1000
x1 <- rinc(n = n, sd = 0.8)
x2 <- rinc(n = n, sd = 0.4)
data1 <- data_frame(
  income = c(x1, x2),
  country = rep(c("Unevenland", "Equaltopia"), each = n)
) %>%
  mutate(lifespan = ifelse(income < 10000, rnorm(n(), 50, 7), rnorm(n(), 70, 10)))

data1 %>%
  ggplot(aes(x = income, fill = country)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(label = dollar)

Gini(x1)
Gini(x2)

ggplot(data1, aes(x = income, y = lifespan, colour = country)) +
  geom_point()

data1 %>%
  group_by(country) %>%
  summarise(ineq = Gini(income),
            income = mean(income),
            lifexp = mean(lifespan))




country_means <- 

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

data3 %>%
  select(-country) %>%
  ggpairs()
  

# model with scaled data to make it easier to compare the two coefficients
mod <- lm(life_exp ~ mean_income + inequality, data = as.data.frame(scale(select(data3, -country))))
anova(mod)
summary(mod)
confint(mod)

x <- 0 :100000
y <- 50 + x ^ 0.5 / 7
plot(x, y, type = "l")
