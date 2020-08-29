# Time series cross-validation of effective reproduction number for R
# Expects to be run as part of the overall blog-source project (eg packages
# called in .Rprofile etc), but shouldn't be hard to pull out if necessary.
#
# Peter Ellis 22-29 August 2020

source("covid-tracking/covid-setup.R")
library(lubridate)

#-----------------the Victoria data--------------
# parameter for power positivity adjustment
k <- 0.1

# optional: remove today's data so we can put it in by hand including positivity
gd_orig <- filter(gd_orig, Date != Sys.Date())

# check by hand to see if we need to add today's news in
tmp <- filter(gd_orig, State == "VIC") %>% arrange(Date) %>% filter(!is.na(`Cumulative case count`))

if(max(tmp$Date) < Sys.Date()){
  warning("No data yet for today (perhaps you deleted it yourself?)")
}

if(max(tmp$Date) >= min(latest_vic_by_hand$date)){
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
  rbind(latest_vic_by_hand) %>%
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


#---------Estimate Reff-------------

all_dates <- seq(from = as.Date("2020-06-01"), to = (Sys.Date() - 14), by = 1)
vic_cv_r_nowcasts <- tibble(date = date(),
                         bottom = numeric(),
                         top = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         median = numeric(),
                         mean = numeric(),
                         sd = numeric())

# Trick of starting a loop from itself to make it easier to run this
# loop in part if it gets interrupted:
i <- 1
for(i in i:length(all_dates)){
  the_date <- all_dates[i]
  print(the_date)

  d2 <- d %>%
    mutate(confirm = round(cases_corrected) ) %>%
    dplyr::select(date, confirm) %>%
    mutate(breakpoint = as.numeric(date %in% npi_dates)) %>%
    filter(date <= the_date) %>%
    as.data.table() 
  
  estimates_vic <- EpiNow2::epinow(reported_cases = d2, 
                                generation_time = generation_time,
                                delays = list(incubation_period, reporting_delay),
                                horizon = 0, samples = 3000, warmup = 600, 
                                cores = 4, chains = 4, verbose = TRUE, 
                                adapt_delta = 0.95,
                                estimate_breakpoints = as.logical(the_date > "2020-07-15"))
  
  latest_full <- estimates_vic$estimates$summarised
  
  saveRDS(latest_full, file =glue("covid-tracking/estimates_vic_{the_date}.rds"))
}



#-----------latest for comparison purposes----------------
# Using all the data available up to today, closest we can
# get to definitive estimate of R in the past:
d3 <- d %>%
  mutate(confirm = round(cases_corrected) ) %>%
  dplyr::select(date, confirm) %>%
  mutate(breakpoint = as.numeric(date %in% npi_dates)) %>%
  as.data.table() 

estimates_vic_latest <- EpiNow2::epinow(reported_cases = d3, 
                                 generation_time = generation_time,
                                 delays = list(incubation_period, reporting_delay),
                                 horizon = 0, samples = 3000, warmup = 600, 
                                 cores = 4, chains = 4, verbose = TRUE, 
                                 adapt_delta = 0.95,
                                 estimate_breakpoints = TRUE)

save(estimates_vic_latest, file = "covid-tracking/estimates_vic_latest.rda")

#================analysis and visualisation of results==============

#----------------Data prep--------------------
# This can be run later if necessary as we are loading in data from above,
# which can take 20+ hours to generate.

# The latest "hindsight" version of estimates:
load("covid-tracking/estimates_vic_latest.rda")

# load in all our old estimates:
all_rds_files <- list.files("covid-tracking", pattern = ".rds", full.names = TRUE)

read <- function(f){
  readRDS(f)  %>%
    filter(variable == "R") %>%
    filter(date == max(date)) %>%
    select(date, bottom, top, lower, upper, median, mean, sd)
}

vic_cv_r_nowcasts <- bind_rows(lapply(all_rds_files, read))

# how much is our uncertainty on the day:
margins <- vic_cv_r_nowcasts %>%
  mutate(hwci1_prop = (upper - lower) / 2 / median,
         hwci2_prop = (top - bottom) / 2 / median) %>%
  summarise(hp1 = mean(hwci1_prop, tr = 0.2),
            hp2 = mean(hwci2_prop, tr = 0.2))

# Create confidence/credibility bands to use in the later scatter plot
# of a sort of half width conf/cred band from "getting it right"
howout <- tibble(x = seq(from =0.5, to = 2, length.out = 100)) %>%
  mutate(overestimate10 = x / (1 - margins$hp1),
         underestimate10 = x / (1 + margins$hp1),
         overestimate20 = x / (1 - margins$hp2),
         underestimate20 = x / (1 + margins$hp2))


# latest estimates with benefit of hindsight
latest_estimates <- estimates_vic_latest$estimates$summarised %>%
  filter(variable == "R") %>%
  as_tibble() %>%
  dplyr::select(names(vic_cv_r_nowcasts)) %>%
  mutate(var = "Latest estimate")

# what are the success rates of the credibility intervals? - used in text
vic_cv_r_nowcasts %>%
  select(date, bottom:upper) %>%
  left_join(select(latest_estimates, date, median), by = "date") %>%
  mutate(ok50= (median > lower & median < upper),
         ok90 = median > bottom & median < top) %>%
  summarise(ok50 = mean(ok50),
            ok90 = mean(ok90))

#        ok50      ok90
# 1 0.5789474 0.8947368


#------------Plot 1 - line/point/ribbon time series------------------

the_title <- str_wrap("A nowcast of 'R today' is going to be a lagging measure, compared to the best estimate in hindsight", 80)
the_caption <- "Source: analysis using EpiNow2 by http://freerangestats.info, of data from The Guardian"


p1 <- vic_cv_r_nowcasts %>%
  ggplot(aes(x = date, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "steelblue") +
  geom_point(colour ="darkblue") +
  geom_line(data = latest_estimates, aes(y = median), colour = "darkred", size = 1.5) +
  annotate("text", x = as.Date("2020-05-15"), y = 1, label = "Best estimate\n in hindsight", colour = "darkred") +
  annotate("text", x = as.Date("2020-08-09"), y = 1.5, label = "Point and 50% credibility\nestimates when nowcasting") +
  labs(x = "",
       y = "Estimated Effective Reproduction Number",
       title = the_title,
       subtitle = str_wrap("Tendency is to underestimate R when it is increasing, and overestimate it when decreasing",
                           100),
       caption = "")

#--------------Plot 2 -scatter plot comparing nowcast to 'actual'---------------------
combined <-  vic_cv_r_nowcasts %>%
  mutate(var = "On-the-day nowcasts")  %>%
  rbind(latest_estimates)

p2 <- combined %>%
  dplyr::select(date, median, var) %>%
  spread(var, median) %>%
  drop_na() %>%
  mutate(label = ifelse(date %in% range(date) | lubridate::mday(date) == 1, format(date, "%d %b"), "")) %>%
  ggplot(aes(y = `Latest estimate`, x = `On-the-day nowcasts`)) +
  geom_ribbon(data = howout, aes(ymin = underestimate20, ymax = overestimate20, x = x), inherit.aes = FALSE, alpha = 0.1) +
  geom_ribbon(data = howout, aes(ymin = underestimate10, ymax = overestimate10, x = x), inherit.aes = FALSE, alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_path(colour = "grey80") +
  geom_point() +
  geom_text_repel(aes(label = label), colour = "steelblue") +
  coord_equal(xlim = c(0.6, 1.6), ylim = c(0.6, 1.6)) +
  labs(title = "Comparing estimates of Covid-19 R for Victoria on the day with better estimates\nmade in hindsight",
       subtitle = str_wrap("In June the model under-estimated R; in July and early August it has over-estimated it.
                           The pale and dark grey areas approximate the width of the original 50% and 90% credibility intervals,
                           but centred on the eventual 'hindsight' estimate.", 100),
       caption = the_caption,
       x = "Nowcasts made with the best data available at the time",
       y = "Latest estimate using data available at 29 August") + 
  theme(plot.title.position = "plot")
  
svg_png(p1, "../img/0192-line", 8, 5)
svg_png(p2, "../img/0192-scatter", 7, 8)
