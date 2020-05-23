library(tidyverse)
library(scales)
library(janitor)
library(ggseas)
library(mgcv)

#------------------Data import and tidying------------

states_orig <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") 
states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv")

states <- states_orig %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  clean_names() %>%
  # force total number of tests to be at least as many as the number of positives:
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
  mutate(state_name = fct_reorder(state_name, positive, .fun = sum)) %>%
  arrange(date) %>%
  ungroup()

#-----------------Smooth the positive test rates-----------
mod <- gam(pos_rate ~ state_name + s(date_n, by = state_name), 
           data = states12, 
           family = quasibinomial,
           weights = total_test_results_increase)

states12$pos_rate_smoothed <- predict(mod, newdata = states12, type = "response")

d <- states12 %>%
  mutate(adj_pos = positive_increase * sqrt(pos_rate_smoothed)) %>%
  select(date, state_name, positive_increase, adj_pos) %>%
  gather(variable, value, -date, -state_name) %>%
  mutate(variable = if_else(variable == "adj_pos",
                            true = "Adjusted for test positivity rate",
                            false = "Original")) %>%
  group_by(state_name, variable) %>%
  arrange(date) %>%
  mutate(value = value / value[n()] * 100)

#-----------------Common themes and labels-------

the_theme <- theme(axis.text.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank())

the_labs <- labs(x = "", 
                 colour = "", 
                 y = "New daily confirmed cases",
                 caption = "Source: data from covidtracking.com, positivity adjustment by http://freerangestats.info")
                 
#------------------------Plots---------------------

# Top 12 states
p1 <- d %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  facet_wrap(~state_name, scale = "free_y")  +
  stat_rollapplyr(width = 7) +
  the_theme +
  the_labs +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Trends in daily COVID-19 cases (rolling seven-day average, scale-free index)",
          "With and without adjustment for proportion of tests that return positives, suggesting relatively more unknown cases in March and April.")
  
# Texas:
p2 <- d %>%
  filter(state_name == "Texas") %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  stat_rollapplyr(width = 7) +
  the_theme +
  the_labs +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Trends in daily COVID-19 cases in Texas (rolling seven-day average, scale-free index)",
          "After adjustment for test-positivity, new cases are still accelerating.")



svg_png(p1, "../img/0183-12-states", w = 11, h = 6)
svg_png(p2, "../img/0183-texas", w = 8, h = 5)
