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
library(RColorBrewer)


vic_dhhs <- read_csv("covid-tracking/victoria-daily-cases.csv", col_types = c("cd")) %>%
  mutate(date = as.Date(date, format = "%B %d, %Y"))

source("covid-tracking/plot_estimates.R")

guardian_url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

gs4_deauth()
gd_orig <- read_sheet(guardian_url) 



#----------dates for breaks---
# see https://www.mja.com.au/journal/2020/victorias-response-resurgence-covid-19-has-averted-9000-37000-cases-july-2020
# dates of the major changes - Stage 3 and Stage 4 restrictions on all of Melbourne:

#non-pharmaceutical interventions: stage 4, masks, stage 3, stage 3 (selected suburbs)
npi_dates <- as.Date(c("2020/08/03", "2020/07/22", "2020/07/10", "2020/07/01"))

#------------Reporting delays---------------------
# From the documentation re delays: " (assuming a lognormal distribution with all 
# parameters excepting the max allowed value on the log scale).


if(!exists("reporting_delay") || 
   !exists("generation_time") ||
   !exists("incubation_period")){
  
  set.seed(123)
  
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
}