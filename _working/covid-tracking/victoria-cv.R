
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
                           as.Date("2020-08-22"),   182
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



#---------Estimate Reff-------------
# see https://www.mja.com.au/journal/2020/victorias-response-resurgence-covid-19-has-averted-9000-37000-cases-july-2020
# dates of the major changes - Stage 3 and Stage 4 restrictions on all of Melbourne:

all_dates <- seq(from = as.Date("2020-06-01"), to = (Sys.Date() - 14), by = 1)
vic_cv_r_nowcasts <- tibble(date = date(),
                         bottom = numeric(),
                         top = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         median = numeric(),
                         mean = numeric(),
                         sd = numeric())

for(i in 1:length(all_dates)){
  the_date <- all_dates[i]
  print(the_date)

  d2 <- d %>%
    mutate(confirm = round(cases_corrected) ) %>%
    select(date, confirm) %>%
    mutate(breakpoint = as.numeric(date %in% npi_dates)) %>%
    filter(date <= the_date) %>%
    as.data.table() 
  
  estimates_vic <- EpiNow2::epinow(reported_cases = d2, 
                                generation_time = generation_time,
                                delays = list(incubation_period, reporting_delay),
                                horizon = 0, samples = 3000, warmup = 600, 
                                cores = 4, chains = 4, verbose = TRUE, 
                                adapt_delta = 0.95,
                                estimate_breakpoints = TRUE)
  
  latest_full <- estimates_vic$estimates$summarised
  latest <- latest_full %>%
    filter(variable == "R") %>%
    filter(date == the_date) %>%
    select(date, bottom, top, lower, upper, median, mean, sd)
  vic_cv_r_nowcasts <- rbind(vic_cv_r_nowcasts, latest)
  
  saveRDS(latest_full, file =glue("covid-tracking/estimates_vic_{the_date}.rds"))
}

save(vic_cv_r_nowcasts, file = "covid-tracking/vic_cv_r_nowcasts.rda")
#--------------------analysis----------
