
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
                                 horizon = max(30, as.numeric(as.Date("2020-11-15") - Sys.Date())), 
                                 samples = 4000, warmup = 600, 
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


last_pos_ratio <- d %>%
  filter(!is.na(ps1)) %>%
  filter(date == max(date)) %>%
  mutate(ratio = cases_corrected / confirm) %>%
  pull(ratio)

# Probability average new cases < 50 in 14 days to 

winsorize_df <- function(data, variable, tr = 0.01){
  trn <- round(nrow(data) * tr)
  
  data %>%
    arrange(desc({{variable}})) %>%
    slice(-(1:trn)) %>%
    arrange({{variable}}) %>%
    slice(-(1:trn))
 }

vic_results <- rbind(
  mutate(estimates_vic$plots$reports$data, type = "positivity-adjusted reported cases"),
  mutate(estimates_vic$plots$infections$data, type = "Infections"),
  mutate(estimates_vic$plots$reff$data, type = "Effective reproduction number (R)")
) %>% 
  as_tibble() 
write_csv(vic_results, glue("../covid-tracking/vic-results-{Sys.Date()}.csv"))
write_csv(vic_results, glue("../_site/covid-tracking/vic-results-{Sys.Date()}.csv"))
write_csv(vic_results, glue("../_site/covid-tracking/vic-results-latest.csv"))



#--------------28 September--------------
pd1 <- estimates_vic$estimated_reported_cases$samples %>% 
  # replace simulations with the actual values, if we have them:
  left_join(select(d, date, actual = confirm), by = "date") %>%
  mutate(best_cases = ifelse(is.na(actual), cases, actual)) %>%
  filter(date >= "2020-09-14" & date <= "2020-09-27") %>%
  group_by(sample) %>%
  summarise(avg_14_day = mean(best_cases / last_pos_ratio))

pr1 <-  pd1 %>%
  summarise(pr = round(mean(avg_14_day < 50), 2)) %>%
  pull(pr)

plot1 <- pd1 %>%
  #winsorize_df(avg_14_day, tr = 0.02) %>%
  ggplot(aes(x = avg_14_day)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  coord_cartesian(xlim = c(0, 50)) +
  labs(title = glue("{percent(pr1, accuracy = 1)} chance of meeting target for 28 September 2020"),
       subtitle = "Target is 14 day average of less than 50 new confirmed cases per day.",
       x = "14 day average of cases, period finishing on 27 September 2020",
       caption = glue("Simplified version of Melbourne targets for Third Step. Forecast as at {Sys.Date()}."))

#------------------Rolling 14 day average-----------------
s1 <- estimates_vic$estimated_reported_cases$samples %>%
  # replace simulations with the actual values, if we have them:
  left_join(select(d, date, actual = confirm), by = "date") %>%
  mutate(best_cases = ifelse(is.na(actual), cases, actual)) %>%
  filter(date >= "2020-09-01") 

all_dates <- sort(unique(s1$date))[-(1:13)]
s2 <- list()
for(i in 1:length(all_dates)){
  the_date <- as.Date(all_dates[i])

  s2[[i]] <- s1 %>%
    filter(date <= the_date & date > (the_date - 14)) %>%
    group_by(sample) %>%
    summarise(avg_14_day = mean(best_cases / last_pos_ratio)) %>%
    mutate(date_to = the_date)
}

s3 <- bind_rows(s2) %>%
  group_by(date_to) %>%
  filter(date_to >= "2020-09-25") %>%
  summarise(lower = quantile(avg_14_day, 0.10),
            mid = median(avg_14_day),
            upper = quantile(avg_14_day, 0.80)) 

earliest5 <- s3 %>%
  filter(mid <=5) %>%
  arrange(date_to) %>%
  slice(1) %>%
  pull(date_to)

plot3 <- s3 %>%
  ggplot(aes(x = date_to)) +
  geom_vline(xintercept = (Sys.Date() - 1), size = 2, colour = "grey") +
  geom_hline(yintercept = 5, size = 2, colour = "grey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.5) +
  geom_line(aes(y = mid)) +
  labs(x = "14 day period ending (green area shows 80% credibility interval)",
       y = "Average new cases",
       title = "Expected 14 day rolling average of new cases", 
       subtitle = glue("Likely to go below 5 per day when data released on {format(earliest5 + 1, '%d %B %Y')}."),
       caption = glue("Simplified version of Melbourne targets for Second and Third Steps. Forecast as at {Sys.Date()}."))

#--------------18 October--------------
# noting 18 October is the day for which 19 october releases data
pd2 <- estimates_vic$estimated_reported_cases$samples %>%
  # replace simulations with the actual values, if we have them:
  left_join(select(d, date, actual = confirm), by = "date") %>%
  mutate(best_cases = ifelse(is.na(actual), cases, actual)) %>%
  filter(date >= "2020-10-05" & date <= "2020-10-18") %>%
  group_by(sample) %>%
  summarise(avg_14_day = mean(best_cases / last_pos_ratio))

pr2 <-  pd2 %>%
  summarise(pr = round(mean(avg_14_day < 5), 2)) %>%
  pull(pr)

plot2 <- pd2 %>%
  winsorize_df(avg_14_day, tr = 0.01) %>%
  ggplot(aes(x = avg_14_day)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  coord_cartesian(xlim = c(0, 20)) +
  labs(title = glue("{percent(pr2, accuracy = 1)} chance of meeting target for 19 October 2020"),
       subtitle = "Target is 14 day average of less than 5 new confirmed cases per day.",
       x = "14 day average of cases, period finishing on 18 October 2020",
       caption = "Forecasts are provisional and are subject to daily change. Analysis by http://freerangestats.info.")

#--------------25 October--------------
# noting 25 October is the day for which 16 october releases data
pd2a <- estimates_vic$estimated_reported_cases$samples %>%
  # replace simulations with the actual values, if we have them:
  left_join(select(d, date, actual = confirm), by = "date") %>%
  mutate(best_cases = ifelse(is.na(actual), cases, actual)) %>%
  filter(date >= "2020-10-12" & date <= "2020-10-25") %>%
  group_by(sample) %>%
  summarise(avg_14_day = mean(best_cases / last_pos_ratio))

pr2a <-  pd2a %>%
  summarise(pr = round(mean(avg_14_day < 5), 2)) %>%
  pull(pr)

plot2a <- pd2a %>%
  winsorize_df(avg_14_day, tr = 0.01) %>%
  ggplot(aes(x = avg_14_day)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  coord_cartesian(xlim = c(0, 20)) +
  labs(title = glue("{percent(pr2a, accuracy = 1)} chance of meeting target for 26 October 2020"),
       subtitle = "Target is 14 day average of less than 5 new confirmed cases per day.",
       x = "14 day average of cases, period finishing on 25 October 2020",
       caption = "Forecasts are provisional and are subject to daily change. Analysis by http://freerangestats.info.")


fcp <- function(){
  print(plot2a + plot3 + plot_layout(ncol = 2))
}
svg_png(fcp, "../img/covid-tracking/victoria-14day", w = 11, h = 5)
svg_png(fcp, "../_site/img/covid-tracking/victoria-14day",w = 11, h = 5)

# Some charts just for Tweeting:
svg_png(plot3, "../img/covid-tracking/victoria-14day-fc-only",w = 7, h = 3.7)
svg_png(plot2, "../img/covid-tracking/victoria-14day-density-only-1910",w = 7, h = 3.7)
svg_png(plot2a, "../img/covid-tracking/victoria-14day-density-only-2610",w = 7, h = 3.7)


#--------------23 November---------
# Too far away to forecast meaningfully
# pr3 <- estimates_vic$estimated_reported_cases$samples %>%
#   filter(date >= "2020-11-09" & date <= "2020-11-22") %>%
#   group_by(sample) %>%
#   summarise(avg_14_day = mean(cases / last_pos_ratio)) %>%
#   summarise(pr = mean(avg_14_day == 0)) %>%
#   pull(pr)
# 
# round(c(pr1, pr2, pr3), 2)


#------------------Publish----------------------------

stopifnot("patchwork" %in% class(fcp()))
#stopifnot(pr1 > 0 && pr2 > 0 && pr1 <= 1 && pr2 <= 1)

wd <- setwd("../_site")

system('git add img/covid-tracking/victoria-14day.*')
system('git add img/covid-tracking/victoria-latest.*')
system('git add img/covid-tracking/victoria-positivity.*')
system('git add img/covid-tracking/victoria-positivity-correction.*')
system('git add covid-tracking/vic-results-*.csv')

system('git config --global user.email "peter.ellis2013nz@gmail.com"')
system('git config --global user.name "Peter Ellis"')

system('git commit -m "latest Victoria covid 14 day forecasts"')
system('git push origin master')
setwd(wd)
