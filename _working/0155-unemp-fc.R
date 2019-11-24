library(quantmod)
library(tidyverse)
library(scales)
library(readabs)
library(lubridate)
library(forecast)
library(vars)
library(rvest)
library(seasonal)
library(ggdag)


the_caption <- "Analysis by http:://freerangestats.info"

#------------unemployment------------------
lfs1 <- read_abs("6202.0", tables = 1)

govt <- tibble(end = as.Date(c("1975-12-13", "1983-03-05", "1996-03-02", "2007-11-24", "2013-09-07", "2022-06-30")),
               party = c("ALP", "Lib/Nat", "ALP", "Lib/Nat", "ALP", "Lib/Nat")) %>%
  mutate(start = c(as.Date("1972-12-02"), end[-n()]))
          
p <- lfs1 %>%
  filter(series ==  "Unemployment rate ;  Persons ;" & series_type != "Trend") %>%
  ggplot() +
  geom_rect(data = govt, alpha = 0.8, 
            aes(xmin =start, xmax = end, ymin = -Inf, ymax = Inf, fill = party)) +
  geom_line(aes(x = date, y = value / 100, colour = series_type)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(x = "",
       y = "Unemployment rate",
       colour = "Unemployment series:",
       title = "Unemployment in Australia, 1978 to present",
       subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics",
       caption = the_caption) +
  scale_fill_manual("Federal government:", values = c("ALP" = "#E53440", "Lib/Nat" = "#1C4f9C")) +
  scale_colour_manual(values = c("Original" = "black", "Seasonally Adjusted" = "white")) +
  theme(legend.key = element_rect(fill = "grey"))

svg_png(p, "../img/0155-unemp", w =9, h = 4.5)

# prepare data frame for later with columns for each series
unemp <- lfs1 %>%
  filter(series ==  "Unemployment rate ;  Persons ;" &
           series_type %in% c("Seasonally Adjusted", "Original"))   %>%
  dplyr::select(date, value, series_type) %>%
  spread(series_type, value) %>%
  rename(unemployment = Original,
         unemployment_sa = `Seasonally Adjusted`) %>%
  mutate(unemployment_my_sa = ts(unemployment, start = c(1978, 2), frequency = 12) %>%
           seas() %>%
           final())
#--------------Comparison of seasonal adjustments------------------
col_labs <- seq(from = 1979, to = 2019, by = 10) %>%
  paste0("-01-01") %>%
  as.Date(format = "%Y-%m-%d")

my_col_scale <- scale_colour_viridis_c(breaks = as.numeric(col_labs),
                                       labels = year(col_labs),
                                       option = "D",
                                       direction = -1)

p <- ggplot(unemp, aes(x = unemployment_sa, y = unemployment_my_sa, colour = as.numeric(date))) +
  geom_point() +
  coord_equal() +
  labs(x = "Seasonally adjusted series published by the ABS",
       y = "Seasonally adjusted from seasonal",
       title = "Comparison of seasonally adjusted unemployment series",
       subtitle = "Vertical axis uses the defaults from the {seasonal} R package front-end to X13-SEATS-ARIMA",
       caption = the_caption,
       colour = "") +
  my_col_scale +
  theme(legend.position = "right")

svg_png("../img/0155-seas", p, 9, 6)

# check for the individual highest points
unemp %>%
  mutate(d = abs(unemployment_my_sa - unemployment_sa)) %>%
  arrange(desc(d)) %>%
  slice(1:20)

#-----------------------ASX 200 from Yahoo--------------------
axjo <- getSymbols("^AXJO", auto.assign = FALSE)

p <- autoplot(axjo$AXJO.Close) +
  labs(x = "",
       title= "ASX 200 stock price index",
       subtitle = "Sourced from Yahoo Finance via quantmod; data only available back to 2007",
       caption = the_caption)

svg_png("../img/0155-yahoo", p)

#---------------------ASX 200 from ASX------------------
url <- "https://www.asx.com.au/about/historical-market-statistics.htm"
asx <- read_html(url)

# Read the 18 separate HTML tables in as elements of a list
asx_l <- lapply(1:18, function(i){ html_table(asx)[[i]]})

# convert to a data frame. Note that `date` is going to use the first day of the
# month even though the values are actually month-end values. This is for joining
# to other data that use a different convention later on:
asx_df <-  do.call("rbind", asx_l) %>%
  as_tibble() %>%
  # convert the messy and inconsistent Month column into an actual date:
  mutate(mon = str_sub(Month, 1, 3),
         yr = str_extract(Month, "[0-9]*$"),
  date = as.Date(paste("01", mon, yr, sep = "-"), format = "%d-%b-%y")) %>%
  dplyr::select(-mon, -yr, -Month) %>%
  # convert numbers with comma big marks into numeric values:
  mutate(`Dom. Equity Mkt cap $m` = as.numeric(gsub(",", "", `Dom. Equity Mkt cap $m`)))

# exploratory plot
p <- asx_df %>%
  gather(variable, value, -date) %>%
  ggplot(aes(x = date, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line() +
  scale_y_continuous(label = comma) +
  labs(title = "Various stock market indicators direct from the ASX",
       subtitle = "Month-end data available back to 2002",
       x = "",
       y= "",
       caption = paste("Source: https://www.asx.com.au/about/historical-market-statistics.htm\n",
                       the_caption))

svg_png("../img/0155-asx", p)

# make a data frame of just the series we're interested in:
asx_end_month <- asx_df %>%
  dplyr::select(date,
         asx200 = `S&P/ASX 200 Price Index`)


#--------------------Interest rates---------------------

f11 <- read_csv("https://www.rba.gov.au/statistics/tables/csv/f1.1-data.csv?v=2019-06-08-13-25-32",
                skip = 10)

interest <- f11 %>%
  mutate(date = as.Date(paste0("01-", `Series ID`), format = "%d-%b-%Y")) %>%
  dplyr::select(date, interest = FIRMMCRI) %>%
  filter(!is.na(interest))

#===============Analysis===============


combined <- asx_end_month %>%
  full_join(unemp, by = "date") %>%
  full_join(interest, by = "date") %>%
  mutate(label = ifelse(month(date) == 1, year(date), "")) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(date_n = as.numeric(date)) %>%
  dplyr::select(-unemployment_my_sa) 



p <- combined %>%
  gather(variable, value, -date, -date_n, -label) %>%
  ggplot(aes(x = date, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line() +
  labs(x = "", y = "",
       title = "Stock prices, interest rates and unemployment considered together",
       subtitle = "Sourced from ASX, RBA and ABS",
       caption = the_caption)

svg_png(p, "../img/0155-combined-series", 9, 8)

d1 <- combined %>%
  dplyr::select(-unemployment) %>%
  filter(!is.na(asx200) & !is.na(unemployment_sa)) %>%
  gather(variable, value, -date, -date_n, -label, -unemployment_sa) 

col_labs <- seq(from = 2002, to = 2019, by = 3) %>%
  paste0("-01-01") %>%
  as.Date(format = "%Y-%m-%d")

my_col_scale <- scale_colour_viridis_c(breaks = as.numeric(col_labs),
                                       labels = year(col_labs),
                                       option = "D",
                                       direction = -1)

p <- d1 %>%
  ggplot(aes(x = value, y = unemployment_sa / 100, colour = date_n)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_smooth(method = "gam", colour = "brown") +
  geom_point(data = filter(d1, label == "")) +
  geom_text(aes(label = label)) +
  geom_path() +
  theme(legend.position = "right") +
  labs(x        = "Raw value",
       y        = "Seasonally adjusted unemployment rate",
       colour   = "",
       caption  = the_caption,
       title    = "Inverse relationship between unemployment and both stock prices and interest rates",
       subtitle = "Unemployment impacts on interest rates, rather than the other way around. And stock prices are undergoing long term structural change.") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = percent) +
  my_col_scale
svg_png(p, "../img/0155-csp1", 11, 6)


d2 <- combined %>%
  dplyr::select(-unemployment) %>%
  mutate(asx200 = c(NA, diff(log(asx200))),
         interest = c(NA, diff(interest)) / 100,
         unemployment_sa = c(NA, diff(log(unemployment_sa)))) %>%
  filter(!is.na(asx200) & !is.na(unemployment_sa)) %>%
  gather(variable, value, -date, -label, -unemployment_sa, -date_n)


p <- d2 %>%
  ggplot(aes(x = value, y = unemployment_sa / 100, colour = date_n)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_smooth(method = "gam", colour = "brown") +
  geom_point(data = filter(d2, label == "")) +
  geom_text(aes(label = label)) +
  geom_path() +
  theme(legend.position = "right") +
  labs(x        = "Growth in other variable",
       y        = "Growth in unemployment rate",
       title    = "Changes in interest rates and stock prices, compared to unemployment rate - monthly changes",
       subtitle = "Unemployment and ASX200 shown as diff(log(x)); change in interest rate shown in percentage points.
Increase in unemployment comes with decrease in interest rate; no real relationship to stock prices.",
       colour   = "") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)  +
  my_col_scale 
svg_png(p, "../img/0155-csp2", 11, 6)


#-----------------Subsets of data for modelling---------------
# a subset of the data that is complete for when we have the growth in ASX prices, and its lag, and unemployment
d4 <- combined %>%
  mutate(asx200 = c(NA, diff(log(asx200))),
         asx200_l = lag(asx200)) %>%
  filter(!is.na(unemployment) & !is.na(asx200_l)) 


asx200_ts <- ts(d4$asx200, start = c(2002, 6), frequency = 12)
asx200_ts1 <- ts(d4$asx200_l, start = c(2002, 6), frequency = 12)
unemp_ts <- ts(d4$unemployment, start = c(2002, 6), frequency = 12)
unemp_sa_ts <- ts(d4$unemployment_sa, start = c(2002, 6), frequency = 12)
interest_ts <- ts(d4$interest, start = c(2002, 6), frequency = 12)


#-----------------Vector auto-regression----------
# For the VAR, we'll use seasonally adjusted unemployment
comb_ts_d <- scale(cbind(asx200_ts, unemp_sa_ts, interest_ts)[-1, ])

mod_var <- VAR(comb_ts_d, p = 2)

p1 <- function(){plot(irf(mod_var, impulse = "interest_ts"), main = "Impact of a shock upwards to interest rates",
     sub = "We can see that this model can lead to a misinterpretation of causality", bty = "l")}
p2 <- function(){plot(irf(mod_var, impulse = "asx200_ts"), main = "Impact of a shock upwards to stock price")}
p3 <- function(){plot(irf(mod_var, impulse = "unemp_sa_ts"), main = "Impact of a shock upwards to unemployment")}

svg_png(p1, "../img/0155-irf1", 9, 5)
svg_png(p2, "../img/0155-irf2", 9, 5)
svg_png(p3, "../img/0155-irf3", 9, 5)


#--------------Digression to think about causality-----------------------
dagified <- dagify(unemployment ~ economy,
                   interest ~ RBA,
                   RBA ~ inflation + unemployment,
                   govt ~ voters,
                   voters ~ unemployment + stocks + inflation + interest,
                   economy ~ govt + inflation + interest,
                   inflation ~ economy + interest,
                   stocks ~ economy + interest,
                   outcome = "unemployment")

set.seed(123)
g <- tidy_dagitty(dagified) %>%
  ggplot(aes(x = x, y = y , xend = xend, yend = yend)) +
  geom_dag_edges_arc(edge_colour = "grey50") +
  geom_dag_node(colour ="grey80") +
  geom_dag_text(colour = "steelblue", family = main_font) +
  theme_void(base_family = main_font) +
  labs(title = "Simplified causal diagram of key economic actors and variables in Australia",
       caption = "Only primary impacts are shown, rather than impacts that depend on anticipating intermediaries.")

svg_png("../img/0155-dag", g, 8, 8)

#----------------ARIMA modelling---------------
mod_aa0 <- auto.arima(unemp_ts, d = 1)

mod_aa1 <- auto.arima(unemp_ts, d=1,
                      xreg = interest_ts)

mod_aa2 <- auto.arima(unemp_ts, d=1, 
                      xreg = cbind(asx200_ts, interest_ts))

mod_aa3 <- auto.arima(unemp_ts,  d=1,
                      xreg = cbind(asx200_ts, asx200_ts1, interest_ts))

knitr::kable(AIC(mod_aa0, mod_aa1, mod_aa2, mod_aa3))


knitr::kable(round(confint(mod_aa1), 2))

# The fact that higher interest rates are a good forecast of lower unemployment suggests the RBA (or banks?)
# are taking extra information into account in setting their interest rates - they know something about
# the direction the economy is going that lets them set the interest rate level even before it has 
# manifested itself in the unemployment


#-------------Time series cross-validation------------
# The model chosen by AIC (ie mod_aa1c, which includes interest rates and is better than the univariate model)
# should in theory be the same as what we'd get with leave-one-out cross validation. See:
# Stone M. (1977) An asymptotic equivalence of choice of model by cross-validation and Akaike’s criterion. Journal of the Royal Statistical Society Series B. 39, 44–7.
# But funny things can happne with time series, and actually our sample is not vast. So let's check that out:


#' auto.arima forecast function for time series cross validation
#' 
#' adapted from https://gist.github.com/robjhyndman/d9eb5568a78dbc79f7acc49e22553e96 
aafc <- function(y, h, xreg = NULL, ...){
  if(!is.null(xreg)){
    
    ncol <- NCOL(xreg)
    X <- matrix(xreg[1:length(y), ], ncol = ncol)
    if(NROW(xreg) < length(y) + h)
      stop("Not enough xreg data for forecasting")
    newX <- matrix(xreg[length(y) + (1:h), ], ncol = ncol)
    
    fit <- auto.arima(y, xreg=X, ...)
    return(forecast(fit, xreg = newX, h = h))
    
  } else {
    
    fit <- auto.arima(y, ...)
    return(forecast(fit, h = h))
  }
}

# this CV takes about 20 minutes
system.time({
  aa1_cv <- tsCV(unemp_ts, aafc, xreg = interest_ts, d = 1)
  aa0_cv <- tsCV(unemp_ts, aafc, d=1)
})

rbind(accuracy(unemp_ts - aa1_cv, unemp_ts),
      accuracy(unemp_ts - aa0_cv, unemp_ts)) %>%
  as.data.frame() %>%
  mutate(model = c("With interest rates", "Univariate")) %>%
  dplyr::select(model, everything()) %>%
  knitr::kable()

#---------longer series------

unemp_long <- ts(unemp$unemployment, frequency = 12, start = c(1978, 2))
interest_long <- ts(interest$interest, frequency = 12, start = c(1976, 5)) %>%
  window(start = c(1978, 2))

mod_aa0_long <- auto.arima(unemp_long, d = 1)
mod_aa1_long <- auto.arima(unemp_long, d=1, xreg = interest_long)
AIC(mod_aa0_long, mod_aa1_long) %>% knitr::kable()

system.time({
  aa1_cv_long <- tsCV(unemp_long, aafc, xreg = interest_long, d = 1)
  aa0_cv_long <- tsCV(unemp_long, aafc, d=1)
})

rbind(accuracy(unemp_long - aa1_cv_long, unemp_long),
      accuracy(unemp_long - aa0_cv_long, unemp_long)) %>%
  as.data.frame() %>%
  mutate(model = c("With interest rates", "Univariate")) %>%
  dplyr::select(model, everything()) %>%
  knitr::kable()

#----------Job Adverts---------
library(nousutils)
library(DBI)
sql <- "
SELECT 
COUNT(1)        AS job_adverts,
MONTH(job_date) AS month,
YEAR(job_date)  AS year
FROM job_adverts.d_jobs_bg
GROUP BY MONTH(job_date), YEAR(job_date)
"
dawn <- connect_dawn()
job_ads <- dbGetQuery(dawn, sql)

job_ads <- job_ads_raw %>%
  as_tibble() %>%
  mutate(date = as.Date(paste("01", month, year, sep = "-"), format = "%d-%m-%Y")) %>%
  arrange(date)

ggplot(job_ads, aes(x = date, y = job_adverts)) +
  geom_line()

ja_ts <- ts(job_ads$job_adverts, start = c(2012, 1), frequency = 12)
ja_tsd <- diff(log(ja_ts))

# "seasona"l lag ie each value is related to itself a year ago;
# and a one month negative correlation, suggesting regression to the mean when
# we have an unusually big or small month:
plot(pacf(ja_tsd))

ja_tsd_l1 <- stats::lag(ja_tsd)

ja_tsd_l0 <- window(ja_tsd, start = c(2012, 3), end = c(2019, 4))
ja_tsd_l1 <- window(ja_tsd_l1, start = c(2012, 3), end = c(2019, 4))
unemp_ts2 <- window(unemp_ts, start = c(2012, 3))

mod_aa3 <- auto.arima(unemp_ts2, stepwise = FALSE, stationary = TRUE)
mod_aa4 <- auto.arima(unemp_ts2, 
                      xreg = cbind(ja_tsd_l0, ja_tsd_l1), 
                      stepwise = FALSE, 
                      stationary = TRUE)
mod_aa5 <- auto.arima(unemp_ts2, 
                      xreg = cbind(ja_tsd_l0), 
                      stepwise = FALSE, 
                      stationary = TRUE)

AIC(mod_aa3, mod_aa4, mod_aa5)
                      
unemp_ts2a <- window(unemp_ts, start = c(2012, 2))
ja_tsd_l0a <- window(ja_tsd, start = c(2012, 2), end = c(2019, 4))
mod_aa6 <- auto.arima(unemp_ts2a,
                      xreg = ja_tsd_l0a,
                      stepwise = FALSE,
                      stationary = TRUE)

fc <- forecast(mod_aa6, xreg = window(ja_tsd, start = c(2019, 5)))
autoplot(fc)

plot(final(seas(ts(c(unemp_ts, fc$mean), start = c(2002, 6), frequency = 12))))

lfs1 %>%
  filter(series ==  "Unemployment rate ;  Persons ;" &
           series_type == "Seasonally Adjusted") %>%
  pull(value)

unemp_ts_full <- lfs1 %>%
  filter(series ==  "Unemployment rate ;  Persons ;" &
           series_type == "Original") %>%
  pull(value) %>%
  ts(start = c(1978, 2), frequency = 12) 

forecast(ets(unemp_ts_full), h = 1)
fc$mean
mean(c(4.98176,5.098767))
