
source("covid-tracking/covid-setup.R")

#-----------------the Victoria data--------------

k <- 0.1

if(max(vic_dhhs$date) < (Sys.Date() - 1)){
  warning("No data yet for yesterday")
}

d <- gd_orig %>%
  clean_names() %>% 
  filter(state == "VIC") %>%
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE)) %>%
  mutate(tests_conducted_total  = ifelse(tests_conducted_total < 0, NA, tests_conducted_total)) %>%
  ungroup() %>%
  # correct one typo, missing a zero
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10"), 1068000, tests_conducted_total)) %>%
  # correct another typo or otherwise bad data point
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-08-08"), NA, tests_conducted_total)) %>%
  # remove two bad dates 
  filter(!date %in% as.Date(c("2020-06-06", "2020-06-07"))) %>%
  mutate(date = date - 1) %>%
  full_join(vic_dhhs, by = "date") %>%
  rename(confirm = cases) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         pos_raw = pmin(1, confirm / test_increase)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(confirm = 0)) %>%
  mutate(numeric_date = as.numeric(date),
         positivity = pos_raw) %>%
  filter(date > as.Date("2020-05-01")) %>%
  fill(positivity, .direction = "downup") %>%
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasibinomial")),
         ps2 = fitted(loess(positivity ~ numeric_date, data = ., span = 0.1)),
         cases_corrected = confirm * ps1 ^ k / min(ps1 ^ k)) %>%
  ungroup() %>%
  mutate(smoothed_confirm = fitted(loess(confirm ~ numeric_date, data = ., span = 0.1)),
         seven_day_avg_confirm = zoo::rollapplyr(confirm, width = 7, mean, na.pad = TRUE)) 

if(max(count(d, date)$n) > 1){
  tail(d)
  stop("Some duplicate days, usually coming from partial data")
}

if(max(d$date) < (Sys.Date() - 1)){
  stop("No data yet for yesterday")
}

if(! (Sys.Date() - 2) %in% d$date){
  stop("No data for day before yesterday")
}


vic_caption <- glue("Data from the DHHS; analysis by http://freerangestats.info. Last updated {Sys.Date()}."  )

#-----------------Positivity plot------------------
hp <- d %>%
  filter(!is.na(ps1)) %>%
  filter(date == max(date))

pos_line <- d %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ps1), colour = "steelblue") +
  geom_point(aes(y = pos_raw)) +
  geom_text(data = hp, aes(label = percent(ps1, accuracy = 0.1), y = ps1), 
            hjust = 0, nudge_x = 1, colour = "steelblue") +
  scale_y_log10(label = percent_format(accuracy = 0.1)) +
  xlim(min(d$date), hp$date + 3) +
  labs(x = "",
       y = "Test positivity (log scale)",
       caption = vic_caption,
       title = "Covid-19 test positivity in Victoria, Australia, 2020",
       subtitle = str_wrap(glue("Smoothed line is from a generalized additive model, 
       and is used to adjust incidence numbers before analysis to estimate effective reproduction number.
       The lowest rate is {percent(min(d$ps1), accuracy = 0.01)}, and a positivity rate of 1.5% would result in
       adjusted case numbers being {comma((0.015 / min(d$ps1)) ^ k, accuracy = 0.01)} times their raw value."), 110))

svg_png(pos_line, "../img/covid-tracking/victoria-positivity", h = 5, w = 9)

svg_png(pos_line, "../_site/img/covid-tracking/victoria-positivity", h = 5, w = 9)

#--------------------Positivity correction--------------
pos_correction <- d %>%
  select(date, `Confirmed` = confirm, `Positivity-corrected` = cases_corrected) %>%
  gather(variable, value, -date) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", span = 1/10) +
  scale_y_continuous(label = comma) +
  labs(x = "", 
       y = "Number of cases",
       colour = "",
       title = "Confirmed cases compared to positivity correction",
       subtitle = glue("With a positivity-correction parameter of k={k}"),
       caption = vic_caption) +
  scale_colour_brewer(palette = "Set1")

svg_png(pos_correction, "../img/covid-tracking/victoria-positivity-correction", h = 5, w = 9)

svg_png(pos_correction, "../_site/img/covid-tracking/victoria-positivity-correction", h = 5, w = 9)



#---------Estimate Reff-------------
# see https://www.mja.com.au/journal/2020/victorias-response-resurgence-covid-19-has-averted-9000-37000-cases-july-2020
# dates of the major changes - Stage 3 and Stage 4 restrictions on all of Melbourne:


d2 <- d %>%
  mutate(confirm = round(cases_corrected) ) %>%
  select(date, confirm) %>%
  mutate(breakpoint = as.numeric(date %in% npi_dates)) %>%
  as.data.table() 

stopifnot(sum(d2$breakpoint) == length(npi_dates))

estimates_vic <- EpiNow2::epinow(reported_cases = d2, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 14, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95,
                              estimate_breakpoints = TRUE)


if(max(filter(estimates_vic$estimates$summarised, variable == "R")$top, na.rm = TRUE) > 10){
  stop("Probable convergence problem; some estimates of R are implausibly high")
}

pc_vic <- my_plot_estimates(estimates_vic, 
                         extra_title = " and positivity",
                         caption = vic_caption,
                         y_max = 1000)

svg_png(pc_vic, "../img/covid-tracking/victoria-latest", h = 10, w = 10)

svg_png(pc_vic, "../_site/img/covid-tracking/victoria-latest", h = 10, w = 10)

vic_results <- rbind(
  mutate(estimates_vic$plots$reports$data, type = "positivity-adjusted reported cases"),
  mutate(estimates_vic$plots$infections$data, type = "Infections"),
  mutate(estimates_vic$plots$reff$data, type = "Effective reproduction number (R)")
) %>% 
  as_tibble() 
write_csv(vic_results, glue("../covid-tracking/vic-results-{Sys.Date()}.csv"))
write_csv(vic_results, glue("../_site/covid-tracking/vic-results-{Sys.Date()}.csv"))
write_csv(vic_results, glue("../_site/covid-tracking/vic-results-latest.csv"))


#-----------------------cases-------------------
latest_positivity_correction <- d %>%
  filter(date == max(date)) %>%
  mutate(ratio = cases_corrected / confirm) %>%
  pull(ratio)

forecast_cases <- estimates_vic$plots$reports$data %>%
  mutate(across(where(is.numeric), function(x){x / latest_positivity_correction})) %>%
  mutate(status = ifelse(date <= max(d$date), "Observed", "Forecast")) %>%
  mutate(status = fct_relevel(status, "Observed"))

latest_fc <- tail(forecast_cases, 1)

fc_plot_vic <- forecast_cases %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = status), alpha = 0.5) +
  geom_ribbon(aes(ymin = bottom, ymax = top, fill = status), alpha = 0.1) +
  geom_line(aes(y = median)) +
  geom_point(data = d, aes(y = confirm)) +
  annotate("text", x = latest_fc$date + 1, y = c(latest_fc$bottom, latest_fc$top), 
           label = comma(c(latest_fc$bottom, latest_fc$top))) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")[c(1, 3)]) +
  labs(fill = "",
       x = "",
       y = "Number of confirmed cases",
       title = "Forecast confirmed cases",
       subtitle = "This is a forecast of the number of actual observed, confirmed cases, which is a subset of total infections.",
       caption = vic_caption)
  

svg_png(fc_plot_vic, "../img/covid-tracking/victoria-latest-fc", h = 5, w = 10)

svg_png(fc_plot_vic, "../_site/img/covid-tracking/victoria-latest-fc", h = 5, w = 10)

#------------------Publish----------------------------

wd <- setwd("../_site")

system('git add img/covid-tracking/victoria-latest.*')
system('git add img/covid-tracking/victoria-positivity.*')
system('git add img/covid-tracking/victoria-positivity-correction.*')
system('git add img/covid-tracking/victoria-latest-fc.*')
system('git add covid-tracking/vic-results-*.csv')


system('git config --global user.email "peter.ellis2013nz@gmail.com"')
system('git config --global user.name "Peter Ellis"')

system('git commit -m "latest Victoria covid plot and data"')
system('git push origin master')
setwd(wd)



#---------------------estimate Reff without the positivity correction-----------
# d3 <- d %>%
#   select(date, confirm) %>%
#   as.data.table()
# 
# estimates_vic_np <- EpiNow2::epinow(reported_cases = d3, 
#                                  generation_time = generation_time,
#                                  delays = list(incubation_period, reporting_delay),
#                                  horizon = 7, samples = 3000, warmup = 600, 
#                                  cores = 4, chains = 4, verbose = TRUE, 
#                                  adapt_delta = 0.95)
# 
# 
# pc_vic_np <- my_plot_estimates(estimates_vic_np, 
#                             extra_title = "",
#                             caption = vic_caption,
#                             y_max = 2000)
# 
# svg_png(pc_vic_np, "../img/covid-tracking/victoria-latest-np", h = 10, w = 10)
# 
# svg_png(pc_vic_np, "../_site/img/covid-tracking/victoria-latest-np", h = 10, w = 10)
