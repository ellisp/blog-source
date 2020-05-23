library(tidyverse)
library(scales)
library(googlesheets4)
library(EpiEstim)
library(janitor)
library(patchwork)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 7)

#===============Australian state Reffs=======================

url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

strictly_accumulate <- function(x){
  for(i in (length(x) - 1):1){
    if(x[i + 1] < x[i]){
      x[i] <- x[i + 1]
    }
  }
  return(x)
}

sheets_deauth()
gd_orig <- read_sheet(url) 

all_dates <- data.frame(date = as.Date("2020-01-22") + 1:100)

states_d <- gd_orig %>%
  dplyr::select(State, Date, `Cumulative case count`, `Travel-related`) %>%
  clean_names() %>%
  rename(cumul = cumulative_case_count) %>%
  mutate(date = as.Date(date)) %>%
  full_join(all_dates, by = "date") %>%
  complete(state, date, fill = list(cumul = NA, travel_related = 0)) %>%
  group_by(state) %>%
  fill(cumul, .direction = "down") %>%
  mutate(cumul = replace_na(cumul, 0)) %>%
  group_by(state, date) %>%
  summarise(cumul = max(cumul),
            travel_related = sum(travel_related)) %>%
  mutate(cumul = strictly_accumulate(cumul),
         daily = c(0, diff(cumul)),
         non_travel = daily - travel_related) %>%
  ungroup() %>%
  filter(!is.na(state))


d <- states_d %>%
  filter(state == "VIC") 

stan_data <- list(
  N = nrow(d),
  y =  d$daily
)  

res <- stan("0181-reff-pois.stan", 
            data = stan_data, 
            chains = 4,
            iter = 2000,
            control = list(max_treedepth = 15))

res
x <- 1:99 / 100
plot(x, dbeta(x, 2, 5))

