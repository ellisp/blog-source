
source("covid-tracking/covid-setup.R")

#-----------------the Victoria data--------------

k <- 0.1

# optional: remove today's data so we can pout it in by hand including positivity
gd_orig <- filter(gd_orig, Date != Sys.Date())


# check by hand to see if we need to add today's news in
tmp <- filter(gd_orig, State == "VIC") %>% arrange(Date) %>% filter(!is.na(`Cumulative case count`))
tail(tmp)

if(max(tmp$Date) < Sys.Date()){
  warning("No data yet for today (perhaps you deleted it yourself?)")
}



latest_by_hand <- tribble(~date,                  ~confirm,
                           as.Date("2020-08-26"),   149
) %>%
  mutate(tests_conducted_total = NA,
         cumulative_case_count = NA,
         test_increase = NA,
         pos_raw = NA / NA)

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
  # correct another typo or otherwise bad data point
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-08-08"), NA, tests_conducted_total)) %>%
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

if(max(d$date) < Sys.Date()){
  stop("No data yet for today")
}

if(! (Sys.Date() - 1) %in% d$date){
  stop("No data for yesterday")
}


the_caption <- glue("Data gathered by The Guardian; analysis by http://freerangestats.info. Last updated {Sys.Date()}."  )

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
       caption = the_caption,
       title = "Covid-19 test positivity in Victoria, Australia, 2020",
       subtitle = str_wrap(glue("Smoothed line is from a generalized additive model, 
       and is used to adjust incidence numbers before analysis to estimate effective reproduction number.
       The lowest rate is {percent(min(d$ps1), accuracy = 0.01)}, and a positivity rate of 1.5% would result in
       adjusted case numbers being {comma((0.015 / min(d$ps1)) ^ k, accuracy = 0.01)} times their raw value."), 110))

svg_png(pos_line, "../img/covid-tracking/victoria-positivity", h = 5, w = 9)

svg_png(pos_line, "../_site/img/covid-tracking/victoria-positivity", h = 5, w = 9)





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


pc_vic <- my_plot_estimates(estimates_vic, 
                         extra_title = " and positivity",
                         caption = the_caption,
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

wd <- setwd("../_site")

system('git add img/covid-tracking/victoria-latest.*')
system('git add img/covid-tracking/victoria-positivity.*')
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
#                             caption = the_caption,
#                             y_max = 2000)
# 
# svg_png(pc_vic_np, "../img/covid-tracking/victoria-latest-np", h = 10, w = 10)
# 
# svg_png(pc_vic_np, "../_site/img/covid-tracking/victoria-latest-np", h = 10, w = 10)
