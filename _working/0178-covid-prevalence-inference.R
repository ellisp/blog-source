library(tidyverse)
library(janitor)
library(scales)
library(Cairo)
library(mgcv)
library(EpiEstim)
library(patchwork)

CairoWin()

#==================Data prep================================

#------------------Import data------------------

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

p1 <- states12 %>%
  ggplot(aes(x = date, y = pos_rate)) +
  facet_wrap(~state_name) +
  #geom_line() +
  geom_line(aes(y = pos_rate_smoothed)) +
  geom_point(aes(size = total_test_results_increase), alpha = 0.1) +
  scale_size_area(label = comma, max_size = 12) +
  labs(size = "Number of daily tests", 
       x = "",
       y = "",
       title = "Test-positivity rates for COVID-19 in 12 US states",
       caption = "Source: covidtracking.com, smoothing by freerangestats.info") +
  scale_y_continuous(label = percent, limits = c(0, 1))

svg_png(p1, "../img/0178-smoothed-test-rates", 11, 7)


#==========================Exploring my model================

increase_cases <- function(observed_cases, pos_rate, m, k){
  y <- observed_cases * pos_rate ^ k * m
  return(y)
}

the_data <- states12 %>%
  filter(state_name == "New York") %>%
  mutate(`Simple multiplier\n(confirmed x 6)` = increase_cases(positive_increase, pos_rate_smoothed, m = 6, k = 0),
         `Deaths 7 days later x 100` = deaths_x_days_later * 100,
         `Ratio multiplier` = increase_cases(positive_increase, pos_rate_smoothed, m = 1.64, k = 1),
         `Generalized adjustment` = increase_cases(positive_increase, pos_rate_smoothed, m = 10, k = 0.5))  %>%
  select(date, `Confirmed cases` = positive_increase, 
         `Simple multiplier\n(confirmed x 6)`:`Generalized adjustment`) %>%
  gather(variable, value, -date) %>%
  mutate(variable = fct_reorder(variable, -value, .fun = last),
         variable = fct_relevel(variable, "Confirmed cases", after = Inf))

p2 <- the_data %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  theme(legend.position = "right") +
  scale_y_continuous(label = comma ) +
  scale_colour_brewer(palette = "Set1") +
  labs(colour = "Adjustment method",
       title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
       subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The 'simple multiplier' method probably overestimates cases when testing is good, and underestimates it when testing is inadequate.",
       y = "Daily new cases",
       x = "",
       caption = "Source: Confirmed cases and testing data from covidtracking.com, analysis by freerangestats.info")

p2a <- p2 + geom_line()
p2b <- p2 + geom_smooth(method = "loess", se = FALSE, span = 0.5)
  

svg_png(p2a, "../img/0178-diff-methods", 10, 6)
svg_png(p2b, "../img/0178-diff-methods-smoothed", 10, 6)

p3 <- the_data %>%
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

svg_png(p3, "../img/0178-total-cases", 10, 6)

#================Estimating Reff================
# This section copied from https://github.com/CBDRH/ozcoviz/blob/master/nsw_eff_R_data_and_plot_prep.R
# by Tim Churches and Nick Tierney
#
# Get the data on serial interval distribution from Nishiura et al:
download.file("https://raw.githubusercontent.com/CBDRH/ozcoviz/master/get_nishiura_si_sample.R",
              destfile = "get_nishiura_si_sample.R")
source("get_nishiura_si_sample.R")
nishi_si_sample  <- get_nishiura_si_sample()

# configs for eff R estimation
# SI distribution from Nishiura et al.
parametric_si_nishiura_config <- make_config(list(mean_si = 4.7,
                                                  std_si = 2.9))

# values from https://www.nejm.org/doi/full/10.1056/NEJMoa2001316
parametric_si_li_config <- make_config(list(mean_si = 7.5,
                                            std_si = 3.4))

# posterior sample based on Nishiura et al SI data
si_from_sample_nishiura_config <-  make_config(list(n1=500, n2 = 50, seed=2))




# estimate eff R

all_variables <- unique(the_data$variable)
plots <- list()

for(i in 1:length(all_variables)){
  incid <- filter(the_data, variable == all_variables[i]) %>% 
    rename(I = value,
           dates = date) %>%
    select(-variable) %>%
    drop_na() %>%
    arrange(dates)
  
  effr <- estimate_R(incid,
                        method="si_from_sample",
                        si_sample=nishi_si_sample,
                        config = si_from_sample_nishiura_config)
  
  
  p <-  plot(effr, what = "R", legend = FALSE, options_R = list(col = "steelblue")) 
  p$labels$title <- paste("Estimated R for COVID-19 in New York:", all_variables[i])
  p$labels$x <- ""
  plots[[i]] <- p
  
}

annotation <- "All methods based on case numbers shown here have an unrealistic spike in effective reproduction number
in mid March as testing started to reveal very high numbers of unrecorded cases prevalent in the population. The first day with significant number of tests was 13 March
(2,900, compared to a previous high of 44). Estimates of R, based on a sliding 7 day window, cannot
be taken regarded as useful until 20 March onwards. Estimates based on 'deaths 7 days later' are problematic 
for other reasons." %>%
  str_wrap(., 60)

annotation_plot <- ggplot() +
  annotate("text", x = -1, y = 0, label = annotation, hjust = 0, size = 5) +
  xlim(-1, 1) +
  theme_void() +
  labs(title = str_wrap("These charts show attempts to adjust the estimation of reproduction 
                        number by scaling up case numbers with a high test positivity.", 70))

draw_plots <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + annotation_plot

frs::svg_png(draw_plots, "../img/0178-r-methods", w= 20, h = 11)











