library(tidyverse)
library(googlesheets4)
library(janitor)
library(scales)
library(mgcv)
library(EpiEstim)
library(EpiNow2) # remotes::install_github("epiforecasts/EpiNow2")
library(frs)     # removes::install_github("ellisp/frs-r-package/pkg")
library(patchwork)

url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

gd_orig <- read_sheet(url) 

d <- gd_orig %>%
  clean_names() %>% 
  filter(state == "VIC") %>%
  select(date, tests_conducted_total, cumulative_case_count) %>%
  # filter(!is.na(tests_conducted_total)) %>% 
  # correct one typo, missing a zero
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10"), 1068000, tests_conducted_total)) %>%
  mutate(date = as.Date(date)) %>%
  # remove two bad dates 
  filter(!date %in% as.Date(c("2020-06-06", "2020-06-07"))) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         case_increase = c(cumulative_case_count[1], diff(cumulative_case_count)),
         positivity = pmin(1, case_increase / test_increase),
         numeric_date = as.numeric(date)) %>%
  fill(positivity, .direction = "down") %>%
  filter(date > as.Date("2020-02-01")) %>%
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasipoisson")),
         ps2 = fitted(loess(positivity ~ numeric_date, data = ., span = 0.1)),
         cases_corrected = case_increase * ps1 ^ 0.1 / min(ps1 ^ 0.1)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(case_increase = 0, cases_corrected = 0))


  

d %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = positivity)) +
  geom_line(aes(y = ps2)) +
  scale_y_continuous(label = percent_format(accuracy = 1))

# I don't believe the sqrt "corrected" cases helped here so have a much more modest 0.1
d %>%
  select(date, cases_corrected, case_increase) %>%
  gather(variable, value, -date) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line()
  
d %>% ggplot(aes(x = date, y = tests_conducted_total)) +
  geom_line() +
  scale_y_continuous(label = comma)

# This section copied from https://github.com/CBDRH/ozcoviz/blob/master/nsw_eff_R_data_and_plot_prep.R
# by Tim Churches and Nick Tierney
#
# Get the data on serial interval distribution from Nishiura et al:
download.file("https://raw.githubusercontent.com/CBDRH/ozcoviz/master/get_nishiura_si_sample.R",
              destfile = "get_nishiura_si_sample.R")
source("get_nishiura_si_sample.R")
nishi_si_sample  <- get_nishiura_si_sample()

# posterior sample based on Nishiura et al SI data
si_from_sample_nishiura_config <-  make_config(list(n1=500, n2 = 50, seed=2))

incid1 <- d %>%
  select(dates = date, I = case_increase) %>%
  complete(dates = seq.Date(min(dates), max(dates), by="day"), fill = list(I = 0)) %>%
  filter(dates > as.Date("2020-04-01"))

effr1 <- estimate_R(incid1,
                   method = "si_from_sample",
                   si_sample = nishi_si_sample,
                   config = si_from_sample_nishiura_config)

p1 <- plot(effr1, what = "R", options_R = list(col = "brown")) + 
  ylim(c(0, 3.5)) +
  labs(title = "R in Victoria, estimated from raw incidence")


#-------uncorrected---------
incid2 <- d %>%
  select(dates = date, I = cases_corrected) %>%
  complete(dates = seq.Date(min(dates), max(dates), by="day"), fill = list(I = 0)) %>%
  filter(dates > as.Date("2020-04-01"))

effr2 <- estimate_R(incid2,
                   method = "si_from_sample",
                   si_sample = nishi_si_sample,
                   config = si_from_sample_nishiura_config)

p2 <- plot(effr2, what = "R", options_R = list(col = "steelblue")) + 
  ylim(c(0, 3.5)) +
  labs(title = "...and with incidence corrected for test positivity rate")

p1 + p2


