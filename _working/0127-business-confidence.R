library(tidyverse)
library(stringr)
library(lubridate)
library(forecast)
library(seasonal)
library(nzelect) # for party colours
library(scales)

#--------------download data that was manually downloaded earlier--------------
download.file("https://github.com/ellisp/blog-source/blob/master/_working/DP_LIVE_01082018094138947.csv?raw=true",
              destfile = "DP_LIVE_01082018094138947.csv")

download.file("https://raw.githubusercontent.com/ellisp/blog-source/master/_working/SNE445001_20180801_075329_92.csv",
              destfile = "SNE445001_20180801_075329_92.csv")


#--------------prep - who was in power?-------------
# taken from https://en.wikipedia.org/wiki/List_of_Prime_Ministers_of_New_Zealand

power <- data_frame(
  date = as.Date(c(
    "12/12/1957", "12/12/1960", "8/12/1972", "12/12/1975", 
    "26/07/1984", "2/11/1990", "5/12/1999", "19/11/2008", "26/10/2017"), format = "%d/%m/%Y"),
  pm_party = c(
    "Labour", "National", "Labour", "National",
    "Labour", "National", "Labour", "National", "Labour"),
  # the pm_id identifier is used later in grouping data togheter to avoid annoying connecting lines
  # across the years:
  pm_id = 1:9
) %>%
  right_join(
    data_frame(date = as.Date("1957-12-11") + 1:(70 * 365)),
    by = "date"
  ) %>%
  fill(pm_party, pm_id) %>%
  mutate(qtr = quarter(date),
         yr = year(date),
         mon = month(date))

power_m <- power %>%
  group_by(yr, mon, pm_party, pm_id) %>%
  summarise(freq = n()) %>%
  group_by(yr, mon) %>%
  summarise(pm_party = pm_party[freq == max(freq)],
            pm_id = pm_id[freq == max(freq)])

power_q <- power %>%
  group_by(yr, qtr, pm_party, pm_id) %>%
  summarise(freq = n()) %>%
  group_by(yr, qtr) %>%
  summarise(pm_party = pm_party[freq == max(freq)],
            pm_id = pm_id[freq == max(freq)])




#--------------------Monthly business confidence data----------------------
# See https://data.oecd.org/leadind/business-confidence-index-bci.htm
# Data were downloaded as CSV non-programmatically.
# On a different scale and a bit different in shape (but related) to:
# https://tradingeconomics.com/new-zealand/business-confidence
bc <- read.csv("DP_LIVE_01082018094138947.csv", stringsAsFactors = FALSE)
names(bc)[1] <- "LOCATION" # read.csv corrupts the first name on the way in


bc_nz <- bc %>%
  filter(LOCATION == "NZL") %>%
  mutate(Time = as.Date(paste0(TIME, "-15"), format = "%Y-%m-%d"),
         yr = year(Time),
         mon = month(Time),
         qtr = quarter(Time)) %>%
  select(-INDICATOR, -SUBJECT, -MEASURE, -FREQUENCY) %>%
  as_tibble() %>%
  left_join(power_m, by = c("yr", "mon"))

svg("../img/0127-bc.svg", 8, 5)
bc_nz %>%
  ggplot(aes(x = Time, y = Value, colour = pm_party, group = pm_id)) +
  geom_step() +
  labs(x = "", y = "Amplitude-adjusted business confidence index\nlong term average = 100",
       caption = "Source: OECD, Business Confidence Index; Wikipedia, Prime Ministers of New Zealand") +
  ggtitle("Monthly business confidence in New Zealand") +
  scale_colour_manual("Prime Minister's party:", values = parties_v)
dev.off()



# There's a small monthly seasonality present in business confidence
bc_ts <- ts(bc_nz$Value, start = c(1961, 6), frequency = 12)

svg("../img/0127-bc-stl.svg", 8, 6)
par(family = main_font, font.main = 1)
plot(stl(bc_ts, s.window = 7), main = "Weak and variable seasonality in New Zealand monthly business confidence")
dev.off()

tsdisplay(bc_ts) # not shown

# seasonally adjusted version of business confidence
bc_nz$bc_sa <- final(seas(bc_ts))


#----------------quarterly GDP-----------------------------
# Total GDP, chain volume, production measure:
gdp <- read.csv("SNE445001_20180801_075329_92.csv", stringsAsFactors = FALSE, skip = 1)
names(gdp) <- c("yr_qtr", "gdp_p_cv")

gdp_q <- gdp %>%
  mutate(qtr = substring(yr_qtr, 6, 6),
         yr = substring(yr_qtr, 1, 4),
         yr_num = as.numeric(yr) + as.numeric(qtr) / 4 - 0.125) %>%
  filter(yr == as.numeric(yr)) %>%
  arrange(yr, qtr)

# Much stronger seasonality in the GDP growth rates than there was in the business confidence:
gdp_ts <- ts(gdp_q$gdp_p_cv, start = c(1987, 3), frequency = 4)

svg("../img/0127-gdp-stl.svg", 8, 6)
par(family = main_font, font.main = 1)
plot(stl(gdp_ts, s.window = 7), main = "Strong and consistent seasonality in New Zealand quarterly GDP")
dev.off()

tsdisplay(gdp_ts) # not shown in blog

# create a seasonally adjusted version of the volume series, which we'll use for growth rates
gdp_sa <- final(seas(gdp_ts))
gdp_q$gdp_sa <- gdp_sa

# note that this next method only works because it isn't a tibble.  Viva la base.
n <- nrow(gdp_q)

gdp_q$growth <- c(NA, gdp_q[2:n, "gdp_sa"] / gdp_q[1:(n - 1), "gdp_sa"] - 1)

# remove the first row, which is NA for growth (no comparison possible):
gdp_q <- gdp_q[-1, ]

# make a time series out of it for use later:
growth_ts <- ts(gdp_q$growth, start = c(1987, 3), frequency = 4)

# add a lagged growth series for some graphic comparisons:
gdp_q <- gdp_q %>%
  mutate(growth_lag1 = c(NA, growth[-n()])) %>%
  mutate(yr = as.integer(yr), qtr = as.integer(qtr)) %>%
  left_join(power_q, by = c("yr", "qtr"))

svg("../img/0127-growth-line.svg", 8, 5)
ggplot(gdp_q, aes(x = yr_num, y = growth, colour = pm_party, group = pm_id)) +
  geom_line() +
  labs(x = "", y = "Quarterly growth in seasonally adjusted GDP\n(Compares each quarter to the previous; not annualised)",
       caption = "Source: Stats NZ, chain volume GDP, production measure") +
  scale_y_continuous(label = percent) +
  scale_colour_manual("Prime Minister's party:", values = parties_v) +
  ggtitle("Growth rates in New Zealand GDP over time") 
dev.off()

#--------------combining the two series-------------
# First we need to make a quarterly version of the monthly business confidence data
bc_q <- bc_nz %>%
  group_by(yr, qtr) %>%
  summarise(bc = mean(bc_sa)) %>%
  ungroup() %>%
  mutate(bc_lag1 = c(NA, bc[-n()]),
         bc_lag2 = c(NA, bc_lag1[-n()]),
         bc_lag3 = c(NA, bc_lag2[-n()]),
         bc_lag4 = c(NA, bc_lag3[-n()]))

# then we merge this with the quarterly GDP:
comb <- gdp_q %>%
  inner_join(bc_q, by = c("yr", "qtr")) %>%
  as_tibble()

svg("../img/0127-csp.svg", 10, 9)
comb %>%
  select(yr_num:bc_lag4) %>%
  gather(variable, value, -yr_num, -growth, -gdp_sa, -pm_party, -pm_id) %>%
  ggplot(aes(y = growth, x = value, colour = yr_num)) +
  scale_colour_viridis("Year", option = "B") +
  facet_wrap(~variable, scale = "free_x") +
  geom_path() +
  geom_smooth(method = "lm") +
  scale_y_continuous(label = percent)+
  labs(y = "Quarterly growth in seasonally adjusted real GDP",
       x = "Value of business confidence, or lagged business confidence, or lagged quarterly growth",
       caption = "Source: OECD (business confidence) and Stats NZ (chain volume GDP, production measure)") +
  ggtitle("Relationship between business confidence and economic growth",
          "Business confidence ('bc') is positively correlated with future economic growth,\nbut not as strongly as past values of growth itself.") 
dev.off()


# Let's quantify those correlations:
cors <- comb %>%
  select(growth:bc_lag4, -pm_party, -pm_id) %>%
  slice(2:n()) %>%
  cor %>%
  as.data.frame()
cors[ , "correlate"] <- row.names(cors)

svg("../img/0127-cors.svg", 8, 5)
cors %>%
  filter(correlate != "growth") %>%
  mutate(correlate = fct_reorder(correlate, growth)) %>%
  ggplot(aes(x = growth, y = correlate)) +
  geom_point() +
  labs(x = "Correlation coefficient with real quarterly growth in seasonally adjusted GDP",
       caption = "Source: OECD (business confidence) and Stats NZ (chain volume GDP, production measure)") +
  ggtitle("Relationship between business confidence and economic growth",
          "Business confidence is positively correlated with future economic growth,\nbut not as strongly as is past values of growth itself.")
dev.off()

svg("../img/0127-csp-pm.svg", 10, 9)
comb %>%
  select(yr_num:bc_lag4) %>%
  gather(variable, value, -yr_num, -growth, -gdp_sa, -pm_party, -pm_id) %>%
  ggplot(aes(y = growth, x = value)) +
  geom_path(aes(colour = pm_party, group = pm_id)) +
  geom_smooth(method = "lm") +
  facet_grid(pm_party ~ variable, scales = "free_x") +
  labs(y = "Quarterly growth in seasonally adjusted real GDP",
       x = "Value of business confidence, or lagged business confidence, or lagged quarterly growth",
       caption = "Source: OECD (business confidence) and Stats NZ (chain volume GDP, production measure)") +
  ggtitle("Relationship between business confidence and economic growth",
          "The relationship between self-reported business confidence and GDP *looks* stronger under National than Labour.
However, the evidence isn't significant of Party of the Prime Minister impacting on either GDP or business confidence") +
  scale_colour_manual("Prime Minister's party:", values = parties_v) +
  scale_y_continuous(label = percent)
dev.off()

#----------------does business confidence help predict growth?------------
# Modelling to see if the lagged business confidence adds anything to a simple univariate time series model of GDP:
xreg <- comb %>%
  mutate(labour_ind = as.integer(pm_party == "Labour")) %>%
  select(bc_lag1:bc_lag4, labour_ind) %>%
  mutate(bc_lag1_lab = labour_ind * bc_lag1)

# Full model predicts growth including with Labour PM as a dummy, and interacting with the first level lag:
model_full <- auto.arima(growth_ts, xreg = xreg)
# Apolitical model ignores who is PM:
model_apol <- auto.arima(growth_ts, xreg = xreg[ , c(1:4)])
# Simple model just treats growth as a univariate time series, says other information has nothing to offer:
model_simp <- auto.arima(growth_ts)

# Business confidence does help.  The best model (lowest AIC) for predicting GDP ignores who is PM, 
# but does included lagged values of business confidence:
AIC(model_full, model_apol, model_simp)

model_apol
model_full
# the estimate of the bc_lag1 coefficient in the model_full is 0.0018.  So an increase in business confidence by 1 on the OECD
# scale would increase forecast quarterly GDP by 0.18 percentage points (eg from 1.00% to 1.18%).  1 is a
# reasonable increase - about 1.1 standard deviations (using last 20 years' variability as a baseline)
# sd(tail(bc_ts, 80))

# blog reports results for model_apol, which has the lowest AIC.

#---------------------can party explain business confidence?-------------------
# On the other hand, it's worth noting that lagged and current values of GDP growth
# do *not* at all appear to be much related to business confidence
xreg <- comb %>%
  mutate(labour_ind = as.integer(pm_party == "Labour")) %>%
  select(growth, growth_lag1, labour_ind)

bc_sub_ts <- ts(comb$bc, frequency = 4, start = c(1987, 3))

model_all <- auto.arima(bc_sub_ts, xreg = xreg)
model_apol <- auto.arima(bc_sub_ts, xreg = xreg[ ,1:2])
model_lab_only <- auto.arima(bc_sub_ts, xreg = xreg[ , 3])
model_nothing <- auto.arima(bc_sub_ts)

AIC(model_all, model_apol, model_lab_only, model_nothing)  


convert_pngs("0127")
