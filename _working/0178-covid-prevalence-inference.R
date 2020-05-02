library(tidyverse)
library(janitor)
library(scales)
library(Cairo)
library(mgcv)

CairoWin()

#==================Data prep================================

#------------------Import data------------------

states_orig <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") 
states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv")

states <- states_orig %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  clean_names() %>%
  mutate(total_test_results_increase = pmax(positive_increase, total_test_results_increase)) %>%
  mutate(pos_rate = positive_increase / total_test_results_increase) %>%
  arrange(date) %>%
  mutate(date_n = as.numeric(date))  %>%
  left_join(select(states_info, state, state_name = name), by = "state")

# Just the 12 biggest states
states12 <- states %>%
  group_by(state) %>%
  summarise(max_pos = max(positive)) %>%
  arrange(desc(max_pos)) %>%
  slice(1:12) %>%
  inner_join(states, by = "state") %>%
  # state has to be a factor for use in mgcv::gam:
  mutate(state_name = fct_reorder(state_name, positive, .fun = sum)) 

#-----------------Smooth the positive test rates-----------
mod <- gam(pos_rate ~ state_name + s(date_n, by = state_name), 
           data = states12, 
           family = quasibinomial,
           weights = total_test_results_increase)

states12$pos_rate_smoothed <- predict(mod, newdata = states12, type = "response")

states12 %>%
  ggplot(aes(x = date, y = pos_rate)) +
  facet_wrap(~state_name) +
  #geom_line() +
  geom_line(aes(y = pos_rate_smoothed)) +
  geom_point(aes(size = total_test_results_increase), alpha = 0.1) +
  ylim(0,1) +
  scale_size_area(label = comma, max_size = 12) +
  labs(size = "Number of daily tests")

#==========================Exploring my model================

increase_cases <- function(observed_cases, pos_rate, m, k){
  y <- observed_cases * (pos_rate / 0.01) ^ k * m
  return(y)
}

the_data <- states12 %>%
  filter(state_name == "New York") %>%
  mutate(`Simple multiplier\n(confirmed x 10)` = increase_cases(positive_increase, pos_rate_smoothed, m = 10, k = 0),
         `Ratio multiplier` = increase_cases(positive_increase, pos_rate_smoothed, m = 0.273, k = 1),
         `Generalized adjustment` = increase_cases(positive_increase, pos_rate_smoothed, m = 1.667, k = 0.5))  %>%
  select(date, `Confirmed cases` = positive_increase, 
         `Simple multiplier\n(confirmed x 10)`:`Generalized adjustment`) %>%
  gather(variable, value, -date) %>%
  mutate(variable = fct_reorder(variable, -value, .fun = last))

the_data %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line() +
  theme(legend.position = "right") +
  scale_y_continuous(label = comma ) +
  scale_colour_brewer(palette = "Set1") +
  labs(colour = "Adjustment method",
       title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
       subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The 'simple multiplier' method probably overestimates cases when testing is good, and underestimates it when testing is inadequate.",
       y = "Daily new cases",
       x = "")

the_data %>%
  group_by(variable) %>%
  summarise(total_cases = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(y = variable, x = total_cases, colour = variable)) +
  scale_colour_brewer(palette = "Set1") +
  geom_point() +
  geom_segment(xend = 0, aes(yend = variable)) +
  scale_x_continuous(label = comma) +
  labs(title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
       subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The three adjustment methods have been calibrated to deliver similar total results for illustrative purposes.",
       y = "Adjustment method",
       x = "Total cases to 1 May 2020") +
  theme(legend.position = "none")

