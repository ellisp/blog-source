library(tidyverse)
library(scales)
library(googlesheets4)
library(propagate)
library(forecast)


url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

gd_orig <- read_sheet(url) 

the_data <- gd_orig %>%
  dplyr::select(state = State, date = Date, cum_cases = `Cumulative case count`) %>%
  mutate(date = as.Date(date)) %>%
  complete(state, date, fill = list(cum_cases = NA)) %>%
  group_by(state) %>%
  fill(cum_cases) %>%
  drop_na() %>%
  group_by(state, date) %>%
  summarise(cum_cases = max(cum_cases, na.rm = TRUE)) %>%
  group_by(date) %>%
  summarise(cum_cases = sum(cum_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(days_since = as.numeric(date - min(date[cum_cases >= 100]))) %>%
  filter(days_since >= 0)

mod_nls <- try(nls(log(cum_cases) ~ SSlogis(days_since, Asym, xmid, scal), 
                   data = the_data, start = list(Asym = log(max(the_data$cum_cases)), xmid = 6, scal = 5),
                   control = nls.control(
                     maxiter = 100, warnOnly = TRUE)))

nd <- tibble(days_since = 20:26)

pred_nls <- predictNLS(mod_nls, newdata = nd, interval = "prediction", alpha = 0.2)


cbind(nd, exp(pred_nls$summary)) %>%
  ggplot(aes(x = days_since)) +
  geom_line(aes(y = Prop.Mean.2)) +
  geom_line(data = the_data, aes(y = cum_cases)) +
  geom_ribbon(aes(ymin = `Prop.10%`, ymax = `Prop.90%`), alpha = 0.5) +
  scale_y_continuous(label = comma)

y_ts <- ts(the_data$cum_cases)
best_lambda <- BoxCox.lambda(y_ts)
mod_aa <- auto.arima(y_ts, lambda = best_lambda)
fc_aa <- forecast(mod_aa, h = 7, biasadj = TRUE)
autoplot(fc_aa) +
  scale_y_continuous(label = comma) +
  labs(x = "Days since 100 cases",
       y = "Cumulative number of cases")
