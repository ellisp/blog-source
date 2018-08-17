library(tidyverse)
library(scales)
library(viridis)

set.seed(765)
n <- 100000
data <- data_frame(
  animal_ = sample(c("lion", "tiger", "bear"), size = n, replace = TRUE, prob = c(0.2, 0.3, 0.5))
) %>%
  mutate(diseased = case_when(
    animal_ == "lion"  ~ sample(0:1, size = n, replace = TRUE, prob = c(0.5, 0.5)),
    animal_ == "tiger" ~ sample(0:1, size = n, replace = TRUE, prob = c(0.75, 0.25)),
    animal_ == "bear" ~ sample(0:1, size = n, replace = TRUE, prob = c(0.9, 0.1))
  )) 

true_props <- data %>%
  group_by(animal_) %>%
  summarise(diseased = round(mean(diseased), 2)) %>%
  mutate(odds = c("1:9", "1:1", "1:3"),
         lab = paste0("Probability: ", diseased, "\nOdds: ", odds))

svg("../img/0131-data.svg", 8, 4)
ggplot(data, aes(fill = as.logical(diseased), x = animal_)) +
  geom_bar(position = "fill") +
  geom_text(data = true_props, aes(label = lab, y = diseased), colour = "white") +
  labs(x = "", y = "Proportion", fill = "Diseased or not:") +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  ggtitle("Simulated data for demonstrating odds and risk ratios",
          "Risk ratio:    tiger / bear = 2.5,   lion / bear = 5.0\nOdds ratio:   tiger / bear = 3.0,   lion / bear = 9.0")
dev.off()

# Correct risk ratio is lion:bear = 5, tiger:bear = 2.5
# Correct odds ratio is lion:bear = 9, tiger:bear 3

# Risk ratio:
model_1 <- glm(diseased ~ animal_, family = quasipoisson, data = data)
exp(coef(model_1))[-1]

# Odds ratio:
model_2 <- glm(diseased ~ animal_, family = quasibinomial, data = data)
exp(coef(model_2))[-1]

# Risk ratio:
model_3 <- glm(diseased ~ animal_, family = quasi(link = "log", variance = "mu"), data = data)
exp(coef(model_3))[-1]

# Odds ratio:
model_4 <- glm(diseased ~ animal_, family = quasi(link = "logit", variance = "mu(1-mu)"), start = c(0,0,0),data = data)
exp(coef(model_4))[-1]

# Risk ratio:
model_5 <- glm(diseased ~ animal_, family = poisson, data = data)
exp(coef(model_5))[-1]

# Odds ratio:
model_6 <- glm(diseased ~ animal_, family = binomial, data = data)
exp(coef(model_6))[-1]

exp(confint(model_1))[-1, ]
exp(confint(model_3))[-1, ]
exp(confint(model_5))[-1, ]

c(summary(model_1)$dispersion, summary(model_3)$dispersion)


# Models 1 and 3 give identical results.  Model 5 has a *larger* confidence interval, because models 1 and 3
# estimate the dispersion factor at 0.7 (ie that the response is under-dispersed compared to a "real" Poisson)
# whereas Model 5 forces the dispersion parameter to be 1.0


exp(confint(model_2))[-1, ]
exp(confint(model_4))[-1, ]
exp(confint(model_6))[-1, ]

c(summary(model_2)$dispersion, summary(model_4)$dispersion)

# For the binmial models, even when the quasi families are use dthe dispersion parameter is estimated
# to be 1.000332 so model_6 gives practically identical confidence intervals

# Note that although the point estimates are biased, the confidence intervals all contain the correct value

convert_pngs("0131")
