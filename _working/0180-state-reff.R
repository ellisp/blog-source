
library(fitdistrplus)
library(tidyverse)
library(scales)
library(googlesheets4)
library(EpiEstim)
library(janitor)
library(patchwork)
library(forecast)

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

#================Estimating Reff================
# This section copied from https://github.com/CBDRH/ozcoviz/blob/master/nsw_eff_R_data_and_plot_prep.R
# by Tim Churches and Nick Tierney
#
# Get the data on serial interval distribution from Nishiura et al:
download.file("https://raw.githubusercontent.com/CBDRH/ozcoviz/master/get_nishiura_si_sample.R",
              destfile = "get_nishiura_si_sample.R")
source("get_nishiura_si_sample.R")

# Create a matrix where each column is a simulation of the distribution serial intervals. These numbers
# are parameters from a log-normal distribution
nishi_si_sample  <- get_nishiura_si_sample()

# posterior sample based on Nishiura et al SI data
si_from_sample_nishiura_config <-  make_config(list(n1=500, n2 = 50, seed=2))


all_states <- unique(states_d$state)
ok_states <- all_states[!all_states %in% c("ACT", "NT")]

effrs <- list()

for(i in 1:length(ok_states)){
  the_state <- ok_states[i]

  the_data <- states_d %>%
    filter(state == the_state) %>%
    select(dates = date,
           I = daily) %>%
    complete(dates) %>%
    arrange(dates) %>%
    filter(dates >= as.Date("2020-03-01"))

  effrs[[i]] <- estimate_R(the_data,
                   method="si_from_sample",
                   si_sample=nishi_si_sample,
                   config = si_from_sample_nishiura_config)

}

plots <- list()
for(j in 1:length(effrs)){
  plots[[j]] <- plot(effrs[[j]], what = "R",
                     options_R = list(col = "steelblue", xlab = ""),
                     legend = FALSE) +
    coord_cartesian(ylim=c(0,4)) +
  labs(title = ok_states[j]) 
}

plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +plots[[5]] + plots[[6]]

#=====================simulating based on Reff============================
x <- as.numeric(nishi_si_sample) * 365 +0.001

ggplot(data.frame(x), aes(x = x)) +
  geom_density() +
  geom_rug() +
  scale_x_log10(label = comma)

summary(x)
fitdistrplus::fitdist(x, "gamma")
y <- rgamma(1000, shape = 3, rate = 0.66)

ggplot(data.frame(y), aes(x = y)) +
  geom_density() +
  geom_rug() +
  scale_x_log10()



create_plague <- function(time_period = 365, r = rep(1.1, time_period), inc_one = 5, 
                          serial_shape = 3, serial_rate = 0.66, 
                          seed = NULL, verbose = FALSE, max_per_day = 1e5){
  if(length(r) != time_period){
    stop("r must be a vector of length time_period")
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  x <- numeric(time_period)
  x[1] <- inc_one
  
  # this loop looks non R-idiomatic, but is necessary as the process is fundamentally iterative.
  # However, the thing that slows it down is not the loop but the rpois() and rgamma() when n is large.
  for(i in 1:(time_period - 1)){
    if(verbose){print(i)}
    new_infections <- sum(rpois(x[i], r[i]))
    when_infected <- ceiling(rgamma(new_infections, shape = serial_shape, rate = serial_rate)) + i
    when_infected <- when_infected[when_infected <= time_period]
    when_inf_tab <- table(when_infected)
    indices <- as.numeric(names(when_inf_tab))
    x[indices] <- x[indices] + when_inf_tab
    if(max(x) > max_per_day){
      warning("Exceeding maximum incidence per day limit; truncating results")
      x <- pmin(x, max_per_day)
    }
  }
  return(x)
}

n <- 365
rr <- list()

set.seed(124)
rr[[1]] <- rep(1.05, n)
white <- arima.sim(model = list(ar = 0.95), n = n) / 15
rr[[2]] <- white - mean(white) + 1.05
rr[[3]] <- white - mean(white) + 1.15
rr[[4]] <- white - mean(white) + seq(from = 1.5, to = 0.6, length.out = n)
sapply(rr, mean)
sapply(rr, min)
stopifnot(min(sapply(rr, min)) > 0)

m <- lapply(rr, function(x){create_plague(time_period = n, r = x, seed = 123, verbose = FALSE)})


par(mfrow=c(2,2), bty = "l")
lapply(m, plot, type = "l")


lapply(rr, plot, type = "l")

lapply(m, function(x){plot(cumsum(x), type = "l")})



#========================forecasting Reff and using that for simulations========
# You could use time series methods to forecast Reff and then simulate future cases based on those
# forecasts but you effectively get infinite prediction intervals - becasuer forecasting Reff is
# not possible with simple methods. Better is to model a bunch of scenarios. But then we are back
# in traditional territory so might as well just use an ICM model from EpiModel for this.