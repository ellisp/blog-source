library(tidyverse)
library(scales)
library(googlesheets4)
library(propagate)
library(forecast)
library(patchwork)

url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

gd_orig <- read_sheet(url) 

the_data <- gd_orig %>%
  dplyr::select(state = State, date = Date, c_cases = `Cumulative case count`) %>%
  mutate(date = as.Date(date)) %>%
  complete(state, date, fill = list(c_cases = NA)) %>%
  group_by(state) %>%
  fill(c_cases) %>%
  drop_na() %>%
  group_by(state, date) %>%
  summarise(c_cases = max(c_cases, na.rm = TRUE)) %>%
  group_by(date) %>%
  summarise(c_cases = sum(c_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(days_since = as.numeric(date - min(date[c_cases >= 100]))) %>%
  filter(days_since >= 0)

mod_nls <- try(nls(log(c_cases) ~ SSlogis(days_since, Asym, xmid, scal), 
                   data = the_data, start = list(Asym = log(max(the_data$c_cases)), xmid = 6, scal = 5),
                   control = nls.control(
                     maxiter = 100, warnOnly = TRUE)))

exp(confint(mod_nls))

nd <- tibble(days_since = 20:50)

pred_nls <- predictNLS(mod_nls, newdata = nd, interval = "prediction", alpha = 0.2)


y_ts <- ts(the_data$c_cases)
best_lambda <- BoxCox.lambda(y_ts)
mod_aa <- auto.arima(y_ts, lambda = best_lambda)
fc_aa <- forecast(mod_aa, h = 31, biasadj = TRUE, level = 80)

p1 <- cbind(nd, exp(pred_nls$summary)) %>%
  ggplot(aes(x = days_since + 1)) +
  geom_line(aes(y = Prop.Mean.2), colour = "steelblue") +
  geom_line(data = the_data, aes(y = c_cases)) +
  geom_ribbon(aes(ymin = `Prop.10%`, ymax = `Prop.90%`), alpha = 0.4, fill = "steelblue") +
  scale_y_continuous(label = comma, limits = c(0, 3e5)) +
  theme(panel.grid.minor.y =  element_blank()) +
  labs(x = "Days since 100th confirmed case",
       y = "Cumulative number of cases",
       title = "30 day scenario for country X",
       subtitle = "Logistic growth model - flattening curve to an asymptote",
       caption = "Difference between the forecasts is not just a modelling decision but a real world choice")



p2 <-autoplot(fc_aa, fcol = "steelblue") +
  scale_y_continuous(label = comma, limits = c(0, 3e5)) +
  theme(panel.grid.minor.y =  element_blank()) +
  labs(x = "Days since 100th confirmed case",
       y = "Shaded area shows 80% prediction interval",
       title = "Scenario choices drive modelling results",
       subtitle = "Time series model - things continue to grow as they have",
       caption = "freerangestats.info")

p3 <- p1 + p2


logy <- scale_y_log10(label = comma, limits = c(100, 3e5))
p4 <- p1 +  logy + labs(y = "Cumulative number of cases (log scale)") + p2 + logy

svg_png(p3, "../img/0174-linear",w = 10, h = 5)
svg_png(p4, "../img/0174-log",w = 10, h = 5)
