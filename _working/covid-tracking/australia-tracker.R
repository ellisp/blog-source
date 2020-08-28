
source("covid-tracking/covid-setup.R")

#-----------------the Australia data--------------

gd_orig_not_today <- filter(gd_orig, Date < Sys.Date())

# power transform parameter
k <- 0.1

d <- gd_orig_not_today %>%
  clean_names() %>% 
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date, state) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE),
            cumulative_case_count = max(cumulative_case_count, na.rm = TRUE)) %>%
  mutate(tests_conducted_total  = ifelse(tests_conducted_total < 0, NA, tests_conducted_total),
         cumulative_case_count = ifelse(cumulative_case_count < 0, NA, cumulative_case_count)) %>%
  ungroup() %>%
  # filter(!is.na(tests_conducted_total)) %>% 
  # correct one typo, missing a zero
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10") & state == "VIC", 1068000, tests_conducted_total)) %>%
  # remove two bad dates 
  filter(!(date %in% as.Date(c("2020-06-06", "2020-06-07")) & state == "VIC")) %>%
  group_by(state) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         confirm = c(cumulative_case_count[1], diff(cumulative_case_count))) %>%
  group_by(date) %>%
  summarise(confirm = sum(confirm),
            test_increase = sum(test_increase)) %>%
  mutate(pos_raw = pmin(1, confirm / test_increase)) %>%
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
  mutate(smoothed_confirm = fitted(loess(confirm ~ numeric_date, data = ., span = 0.1))) 


the_caption <- glue("Data gathered by The Guardian; analysis by http://freerangestats.info. Last data used from {max(d$date)}."  )

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
       title = "Covid-19 test positivity in Australia, 2020",
       subtitle = str_wrap(glue("Smoothed line is from a generalized additive model, 
       and is used to adjust incidence numbers before analysis to estimate effective reproduction number.
       The lowest rate is {percent(min(d$ps1), accuracy = 0.01)}, and a positivity rate of 1.5% would result in
       adjusted case numbers being {comma((0.015 / min(d$ps1)) ^ k, accuracy = 0.01)} times their raw value."), 110))

svg_png(pos_line, "../img/covid-tracking/australia-positivity", h = 5, w = 9)

svg_png(pos_line, "../_site/img/covid-tracking/australia-positivity", h = 5, w = 9)





#---------Estimate Reff-------------

d2 <- d %>%
  mutate(confirm = round(cases_corrected) ) %>%
  select(date, confirm) %>%
  mutate(breakpoint = as.numeric(date %in% npi_dates)) %>%
  as.data.table()

estimates_oz <- EpiNow2::epinow(reported_cases = d2, 
                                 generation_time = generation_time,
                                 delays = list(incubation_period, reporting_delay),
                                 horizon = 7, samples = 3000, warmup = 600, 
                                 cores = 4, chains = 4, verbose = TRUE, 
                                 adapt_delta = 0.95, estimate_breakpoints = TRUE)

if(max(filter(estimates_oz$estimates$summarised, variable == "R")$top, na.rm = TRUE) > 10){
  stop("Probable convergence problem; some estimates of R are implausibly high")
}




pc_oz <- my_plot_estimates(estimates_oz, 
                           location = " in Australia",
                            extra_title = " and positivity",
                            caption = the_caption,
                            y_max = 1200)

svg_png(pc_oz, "../img/covid-tracking/australia-latest", h = 10, w = 10)

svg_png(pc_oz, "../_site/img/covid-tracking/australia-latest", h = 10, w = 10)

wd <- setwd("../_site")

system('git add img/covid-tracking/australia-latest.*')
system('git add img/covid-tracking/australia-positivity.*')


system('git config --global user.email "peter.ellis2013nz@gmail.com"')
system('git config --global user.name "Peter Ellis"')

system('git commit -m "latest covid plot"')
system('git push origin master')
setwd(wd)
