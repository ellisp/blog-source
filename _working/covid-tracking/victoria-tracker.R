library(data.table)
library(tidyverse)
library(googlesheets4)
library(janitor)
library(scales)
library(mgcv)
library(EpiNow2) # remotes::install_github("epiforecasts/EpiNow2")
library(frs)     # removes::install_github("ellisp/frs-r-package/pkg")
library(patchwork)
library(glue)

source("covid-tracking/plot_estimates.R")

# https://twitter.com/DiseaseEcology/status/1274956876640169988
#-----------------the Victoria data--------------

url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

k <- 0.1

gs4_deauth()
gd_orig <- read_sheet(url) 

# optional: remove today's data so we can pout it in by hand including positivity
#gd_orig <- filter(gd_orig, Date != Sys.Date())


# check by hand to see if we need to add today's news in
tmp <- filter(gd_orig, State == "VIC") %>% arrange(Date) %>% filter(!is.na(`Cumulative case count`))
tail(tmp)

if(max(tmp$Date) < Sys.Date()){
  warning("No data yet for today (perhaps you deleted it yourself?)")
}



latest_by_hand <- tribble(~date,                  ~confirm,
                          as.Date("2020-07-28"),   380
                          ) %>%
  mutate(tests_conducted_total = NA,
         cumulative_case_count = NA,
         test_increase = NA,
         pos_raw = NA)
  
if(max(tmp$Date) >= min(latest_by_hand$date)){
  stop("A manually entered data point doubles up with some actual Guardian data, check this is ok")
}

d <- gd_orig %>%
  clean_names() %>% 
  filter(state == "VIC") %>%
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE),
            cumulative_case_count = max(cumulative_case_count, na.rm = TRUE)) %>%
  mutate(tests_conducted_total  = ifelse(tests_conducted_total < 0, NA, tests_conducted_total),
         cumulative_case_count = ifelse(cumulative_case_count < 0, NA, cumulative_case_count)) %>%
  ungroup() %>%
  # filter(!is.na(tests_conducted_total)) %>% 
  # correct one typo, missing a zero
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10"), 1068000, tests_conducted_total)) %>%
  # remove two bad dates 
  filter(!date %in% as.Date(c("2020-06-06", "2020-06-07"))) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         confirm = c(cumulative_case_count[1], diff(cumulative_case_count)),
         pos_raw = pmin(1, confirm / test_increase)) %>%
  rbind(latest_by_hand) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(confirm = 0)) %>%
  mutate(numeric_date = as.numeric(date),
         positivity = pos_raw) %>%
  filter(date > as.Date("2020-05-01")) %>%
  fill(positivity, .direction = "downup") %>%
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasipoisson")),
         ps2 = fitted(loess(positivity ~ numeric_date, data = ., span = 0.1)),
         cases_corrected = confirm * ps1 ^ k / min(ps1 ^ k)) %>%
  ungroup() %>%
  mutate(smoothed_confirm = fitted(loess(confirm ~ numeric_date, data = ., span = 0.1))) 


if(max(d$date) < Sys.Date()){
  stop("No data yet for today")
}


the_caption <- glue("Data gathered by The Guardian; analysis by http://freerangestats.info. Last updated {Sys.Date()}."  )

#-----------------Positivity plot------------------
pos_line <- d %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ps1), colour = "steelblue") +
  geom_point(aes(y = pos_raw)) +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  labs(x = "",
       y = "",
       caption = the_caption,
       title = "Covid-19 test positivity in Victoria, Australia, 2020",
       subtitle = str_wrap(glue("Smoothed line is from a generalized additive model with a Poisson family response, 
       and is used to adjust incidence numbers before analysis to estimate effective reproduction number.
       The lowest rate is {percent(min(d$ps1), accuracy = 0.01)}, and a positivity rate of 1.5% would result in
       adjusted case numbers being {comma((0.015 / min(d$ps1)) ^ k, accuracy = 0.01)} times their raw value."), 110))

svg_png(pos_line, "../img/covid-tracking/victoria-positivity", h = 5, w = 9)

svg_png(pos_line, "../_site/img/covid-tracking/victoria-positivity", h = 5, w = 9)


#------------Estimating R with EpiNow2---------------------
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)





#---------Based on positivity-adjusted-------------

d2 <- d %>%
  mutate(confirm = round(cases_corrected) ) %>%
  select(date, confirm)

estimates2 <- EpiNow2::epinow(reported_cases = d2, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 7, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95)


pc2 <- my_plot_estimates(estimates2, 
                         extra_title = " and positivity",
                         caption = the_caption,
                         y_max = 2000)
svg_png(pc2, "../img/covid-tracking/victoria-latest", h = 10, w = 10)

svg_png(pc2, "../_site/img/covid-tracking/victoria-latest", h = 10, w = 10)

wd <- setwd("../_site")

system('git add img/covid-tracking/victoria-latest.*')
system('git add img/covid-tracking/victoria-positivity.*')


system('git config --global user.email "peter.ellis2013nz@gmail.com"')
system('git config --global user.name "Peter Ellis"')

system('git commit -m "latest covid plot"')
system('git push origin master')
setwd(wd)
