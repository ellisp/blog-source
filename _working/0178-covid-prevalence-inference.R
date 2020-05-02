library(tidyverse)
library(janitor)
library(scales)
library(Cairo)
library(mgcv)
library(EpiEstim)

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
  mutate(state_name = fct_reorder(state_name, positive, .fun = sum)) %>%
  # we want deaths in 7-14 days time as a crude indicator of cases now, for use later
  # Tried various methods and 7 was best. Obviously, if doing this 'for real', 7 should
  # be a parameter we estimate from the data
  group_by(state) %>%
  arrange(date) %>%
  mutate(deaths_x_days_later = lead(death_increase, 7)) %>%
  ungroup()

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
  mutate(`Simple multiplier\n(confirmed x 6)` = increase_cases(positive_increase, pos_rate_smoothed, m = 6, k = 0),
         `Deaths 7 days later x 100` = deaths_x_days_later * 100,
         `Ratio multiplier` = increase_cases(positive_increase, pos_rate_smoothed, m = 0.164, k = 1),
         `Generalized adjustment` = increase_cases(positive_increase, pos_rate_smoothed, m = 1, k = 0.5))  %>%
  select(date, `Confirmed cases` = positive_increase, 
         `Simple multiplier\n(confirmed x 6)`:`Generalized adjustment`) %>%
  gather(variable, value, -date) %>%
  mutate(variable = fct_reorder(variable, -value, .fun = last),
         variable = fct_relevel(variable, "Confirmed cases", after = Inf))

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
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +
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
The four adjustment methods have been calibrated to deliver similar total results for illustrative purposes.",
       y = "Adjustment method",
       x = "Total cases to 1 May 2020") +
  theme(legend.position = "none")


#================Estimating Reff================

# Get the data on
source("https://raw.githubusercontent.com/CBDRH/ozcoviz/master/get_nishiura_si_sample.R")

get_nishiura_si_sample()











#====================Modelling========================
# 
# # an ok prior for the infection fatality rate
# x <- 1:299 /9000
# plot(x, dbeta(x, 2, 200), type = "l")
# 
# #---------------brute force--------------
# 
# mod_data <- states12 %>%
#   filter(state_name == "New York") %>%
#   mutate(cfr = deaths_x_days_later / positive_increase) %>%
#   mutate(cfr = ifelse(is.nan(cfr), NA, cfr)) %>%
#   select(date, deaths_x_days_later, positive_increase, pos_rate_smoothed) %>%
#   drop_na()
#   
# 
# 
# compare_methods <- function(par, other_method, observed_cases, pos_rate){
#   par[1] <- logistic(par[1])
#   y1 <- other_method
#   y2 <- increase_cases(observed_cases, pos_rate, k = par[1], m = par[2])
#   return(sum((y1 - y2) ^ 2))
# }
# 
# res_l <- lapply(1:100, function(i){
#        tmp <- optim(c(k = 0.5, m = 1), 
#         fn = compare_methods, 
#         other_method = mod_data$deaths_x_days_later / rbeta(1,2,200), 
#         observed_cases = mod_data$positive_increase,
#         pos_rate = mod_data$pos_rate_smoothed)
#        
#        estimates <- tmp$par
#        estimates[1] <- logistic(estimates[1])
#        
#        return(estimates)}) 
# 
# res_df <- do.call("rbind", res_l) %>%
#   as_tibble()
# 
# pairs(res_df)
# 
# #-----------Stan-------------------
# 
# stan_data <- states12 %>%
#   filter(state_name == "New York") %>%
#   mutate(cfr = deaths_x_days_later / positive_increase) %>%
#   mutate(cfr = ifelse(is.nan(cfr), NA, cfr)) %>%
#   fill(cfr, deaths_x_days_later, .direction = "downup") %>%
#   filter(cfr >0)
# 
# 
# stan_list <- list(
#   n = nrow(stan_data),
#   cfr = stan_data$cfr,
#   leading_deaths = stan_data$deaths_x_days_later
#   
# )
# 
# fit <- stan("0178-covid-prevalence.stan", data = stan_list, cores = 4)
# 
# fit
