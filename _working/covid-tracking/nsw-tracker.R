
source("covid-tracking/covid-setup.R")

#-----------------the NSW data--------------

# Adapted from https://github.com/CBDRH/ozcoviz/blob/master/get_nsw_data.R,
# Thanks Nicholas Tierney and Tim Churches

nsw_incidence_by_source_url <- paste0("https://data.nsw.gov.au/data/dataset",
                                      "/c647a815-5eb7-4df6-8c88-f9c537a4f21e",
                                      "/resource/2f1ba0f3-8c21-4a86-acaf-444be4401a6d",
                                      "/download/covid-19-cases-by-notification-date",
                                      "-and-likely-source-of-infection.csv")

colspec <- cols(notification_date = col_date(format = ""),
                likely_source_of_infection = col_character())

nsw_incidence_by_source <- read_csv(nsw_incidence_by_source_url,
                                    col_types = colspec) %>%
  rename(source = likely_source_of_infection) %>%
  mutate(source = case_when(
    source == paste0("Locally acquired - contact of a ",
                     "confirmed case and/or in a known cluster") ~ "LC",
    source == "Locally acquired - contact not identified" ~ "LNC",
    source == "Overseas" ~ "OS",
    source == "Interstate" ~ "IS",
    source == "Under investigation" ~ "UIX",
    TRUE ~ "OTH")) %>%
  mutate(sum_source = case_when(source %in% c("IS", "OS") ~ "not_nsw",
                                TRUE ~ "nsw")
         ) %>%
  group_by(notification_date, sum_source) %>%
  summarise(cases = n()) %>%
  ungroup()
  
if(max(nsw_incidence_by_source$notification_date) < Sys.Date()){
  warning("No data yet for today")
}




#----------------test numbers----------
k <- 0.1

# For positivity, we don't care which source it is from
cases_total <- nsw_incidence_by_source %>%
  group_by(notification_date) %>%
  summarise(cases = sum(cases))
  

positivity <- gd_orig %>%
  clean_names() %>% 
  filter(state == "NSW") %>%
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE)) %>%
  filter(tests_conducted_total > 0) %>%
  ungroup() %>%
  # remove two bad dates 
  filter(date != as.Date("2020-07-03")) %>%
  filter(date >= as.Date(c("2020-03-09"))) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         test_increase = pmax(10, test_increase)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(confirm = 0)) %>%
  mutate(numeric_date = as.numeric(date)) %>%
  left_join(cases_total, by = c("date" = "notification_date")) %>%
  filter(date <= max(cases_total$notification_date)) %>%
  mutate(cases = replace_na(cases, 0),
         pos_raw = cases/ test_increase,
         positivity = pmin(0.1, pos_raw)) %>%
  fill(positivity, .direction = "downup") %>%
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasibinomial")),
         ps2 = fitted(loess(positivity ~ numeric_date), data = .)) %>%
  ungroup()  %>%
  select(date, ps1, pos_raw, positivity)



the_caption <- glue("Data from NSW Health and The Guardian; analysis by http://freerangestats.info. Latest data is {max(positivity$date)}."  )

#-----------------Positivity plot------------------
hp <- positivity %>%
  filter(!is.na(ps1)) %>%
  filter(date == max(date))

pos_line <- positivity %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ps1), colour = "steelblue") +
  geom_point(aes(y = pos_raw)) +
  geom_text(data = hp, aes(label = percent(ps1, accuracy = 0.01), y = ps1), 
            hjust = 0, nudge_x = 1, colour = "steelblue") +
  scale_y_log10(label = percent_format(accuracy = 0.1), limits = c(0.0001, 0.11)) +
  xlim(min(nsw_incidence_by_source$notification_date), hp$date + 3) +
  labs(x = "",
       y = "Test positivity (log scale)",
       caption = the_caption,
       title = "Covid-19 test positivity in New South Wales, Australia, 2020",
       subtitle = str_wrap(glue("Smoothed line is from a generalized additive model, 
       and is used to adjust incidence numbers before analysis to estimate effective reproduction number.
       The lowest rate is {percent(min(positivity$ps1), accuracy = 0.01)}, and a positivity rate of 1.5% would result in
       adjusted case numbers being {comma((0.015 / min(positivity$ps1)) ^ k, accuracy = 0.01)} times their raw value."), 110))

svg_png(pos_line, "../img/covid-tracking/nsw-positivity", h = 5, w = 9)

svg_png(pos_line, "../_site/img/covid-tracking/nsw-positivity", h = 5, w = 9)


#------------Positivity-adjusted-----



d <- nsw_incidence_by_source %>%
  rename(date = notification_date) %>%
  left_join(positivity, by = "date") %>%
  mutate(adjusted_cases = cases * ps1 ^ k / min(ps1 ^ k)) %>%
  select(date, adjusted_cases, sum_source) %>%
  spread(sum_source, adjusted_cases, fill = 0)



#---------Estimate Reff-------------
# See issue logged with EpiNow2 - not sure we can distinguish between local and imported cases.
# For now we'll fit to jsut the combined total cases..... Note that 
# EpiEstim lets you distinguish, but does not have the adjustment for delays etc necessary.
# EpiNow will do it, but then why did they dump EpiNow and start again with EpiNow2...
# Discussion on the issue on GitHub suggests they think the benefits of better dealing with
# delays outweigh the cost of import / local distinction (which will hopefully come back alter)
# Another alternative would be the epidemia package https://imperialcollegelondon.github.io/epidemia/
# - not sure if this can handle the imported v local thing but I think not.
d2 <- d %>%
  mutate(confirm = round(not_nsw + nsw)) %>%
  select(date, confirm) %>%
  as.data.table()

estimates_nsw <- EpiNow2::epinow(reported_cases = d2, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 7, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95)

if(max(filter(estimates_nsw$estimates$summarised, variable == "R")$top, na.rm = TRUE) > 10){
  stop("Probable convergence problem; some estimates of R are implausibly high")
}



pc_nsw <- my_plot_estimates(estimates_nsw, 
                         extra_title = " and positivity",
                         caption = the_caption,
                         location = " in New South Wales",
                         y_max = 400)

svg_png(pc_nsw, "../img/covid-tracking/nsw-latest", h = 10, w = 10)

svg_png(pc_nsw, "../_site/img/covid-tracking/nsw-latest", h = 10, w = 10)

wd <- setwd("../_site")

system('git add img/covid-tracking/nsw-latest.*')
system('git add img/covid-tracking/nsw-positivity.*')


system('git config --global user.email "peter.ellis2013nz@gmail.com"')
system('git config --global user.name "Peter Ellis"')

system('git commit -m "latest covid plot"')
system('git push origin master')
setwd(wd)

