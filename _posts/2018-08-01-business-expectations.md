---
layout: post
title: Business confidence and economic growth
date: 2018-08-01
tag: 
   - NewZealand
   - Economics
   - R
description: I have a brief look at the relationship between reported business confidence in New Zealand and what actually happens down the track with economic growth.  Confidence can help (a bit) explain future growth; but current and past growth isn't helpful in explaining confidence.
image: /img/0127-csp-pm.svg
socialimage: http://freerangestats.info/img/0127-csp-pm.png
category: R
---

The ANZ bank have a nice bit of publicity for themselves each month in New Zealand with the release of the results of their monthly Business Outlook Survey.  This week it caused a bit of a stir, with the ANZ's own commentary reporting New Zealand corporate sector is "in a funk" (ANZ's words) with a net 45 percentage points of businesses pessimistic about the economy (while a net positive 4% were positive about their own activity). This led to some animated discussion on what this measure actually means on Twitter and elsewhere.

I set out to explore the value of business confidence as a measure of what the economy is actually going to do.  The ANZ's data back to the 1970s are available from [Trading Economics](https://tradingeconomics.com/new-zealand/business-confidence), but for non-subscribers this is only as a graphic, slightly inconvenient for my purposes.  David Hood [pointed out](https://twitter.com/Thoughtfulnz/status/1024575876552245248) that the OECD have a monthly business confidence measure for many countries.  The ultimate provenance of the data with regard to New Zealand isn't clear to me, but it follows a similar pattern to the ANZ (not identical, and on a different scale) and it is publicly available so I thought I would work with that.

All the code for today's post is at the bottom of the post.  

## Introducing "Business Confidence"

Here's the business confidence series reported by the OECD as part of its [leading indicators collection](https://data.oecd.org/leadind/business-confidence-index-bci.htm).  Because a crucial part of the controversy is whether business report (and possibly feel) their confidence differently according to their personal political views of the government of the day, I've coloured this chart by the party of the Prime Minister for most of the month that the survey relates to.

<img src='/img/0127-bc.svg' width='100%'>

Business confidence is usually reported as "percent of optimists minus percent of pessimists", which in principle is symmetric around zero.  It's not obvious how the OECD have converted such a measure (if they have) into something centred around 100.

The data go back to the 1960s. The dramatic drop reported by the ANZ in the last week hasn't yet percolated to the OECD database, if indeed the ANZ are one of the sources for the OECD data.  The OECD data go to June 2018, which is extremely up to date for data of this sort.

The most interesting thing with this graph is probably the historical matter - how confidence used to have a semi-regular and large cycle, but since the 1990s the regularity has gone (as has some of the volatility).  Those big patterns in the first few decades that look like seasonality actually reflect some other cycle, longer than a year; a cycle that no longer seems to apply with such regularity.  There were indeed dramatic changes in the economy, and in government-business relations, in the 1980s and 1990s that could explain this.  But I'd also want to check out on how the survey is delivered, and particularly how things are converted to this index, before drawing much from that.

There's a small component of seasonal regularity, changing over time, which we can see in this LOESS-based seasonal decomposition:

<img src='/img/0127-bc-stl.svg' width='100%'>

In the analysis later on, I made a seasonally adjusted version of confidence, and took the mean value over a quarter as the quarterly value.

## Economic growth

I decided the quarterly chain-volume (ie adjusted for price differences) GDP production measure would be a good one to compare to confidence levels.  I manually downloaded this as a CSV from Stats NZ's [Infoshare tool](http://archive.stats.govt.nz/infoshare/?url=/infoshare/). The published data naturally shows very strong non-stationarity (ie growth, with increasing population and productivity) and much stronger seasonality:

<img src='/img/0127-gdp-stl.svg' width='100%'>

Seasonality in the national accounts is driven by many things, with tourism a particularly big contributor in New Zealand.  

I used the same methods (X13-SEATS-ARIMA, via Christoph Sax's `seasonal` R package - with defaults such as adjusting for Easter moving from quarter to quarter) to seasonally adjust these quarterly GDP totals as for business confidence.  Then I used the seasonally adjusted totals as the basis for growth quarter-to-quarter:

<img src='/img/0127-growth-line.svg' width='100%'>

Note that these percentage rates are just from one quarter to the next, not the annualised growth rates that get most attention in policy discussions.

Stats NZ publish this series only back to 1987.

## Combining the two

The hope/expectation of business confidence as a leading indicator is that it tells us something about future GDP.  If we look at the relationship between business confidence and GDP 1, 2, 3 and 4 quarters ahead, we see that there is indeed a positive statistical relationship between the two:

<img src='/img/0127-csp.svg' width='100%'>

Connected scatter plots like those above are probably the best exploratory tool for this sort of bivariate time series, but they're too ugly for lots of communication purposes.  But in this case, we can see that generally, when business confidence is higher, then economic growth in the current period and the next four periods forward (with gradually weakening relations) is likely to be higher too.  However, there's a *lot* of noise in the relationship; check out for example the top left quadrant of any of the facets comparing business confidence (at various lags) with GDP, which show when business confidence was low, but economic growth ended up being high.

It's worth noting that a fair chunk of the noise seems to come from the older data (darker in the image above).

As a reference point, I've included the lagged value of quarterly growth itself.  Later on, I'll be looking to see if business confidence helps us predict GDP more than just knowing the historical values of growth does, so this is the right thing to compare it to.  The relationship between growth and its own lagged value is stronger than between growth and business confidence, as seen visually in the connected scatter plots above but also in simple correlations:

<img src='/img/0127-cors.svg' width='100%'>

How does it look when we consider periods with a Labour Prime Minister separately from with one from the Nationals (the only two parties to provide Prime Ministers in the time considered)?  Visually, it looks like the relationship between business confidence and subsequent economic growth figures is stronger when the Nationals (centre-right on the political spectrum and associated with being pro-business) are in power or leading a coalition rather than Labour (centre-left):

<img src='/img/0127-csp-pm.svg' width='100%'>

This fits with a convenient narrative of business owners erring on the side of pessimism when Labour are in power but being more rational in their expectations with a National government.  However, the statistical modelling discussed in the next section didn't find significant evidence of this or other party effects (on growth, on confidence, or on the relationship between growth and confidence).  So we leave this one as a non-conclusion.

## Modelling growth on confidence (and politics) 

I fit three different models to the data exploring how best to explain future economic growth.  These were:

- `model_simp` - treat it as a univariate time series problem, and only use past values of economic growth to explain the future
- `model_apol` - an apolitical model which adds lagged business confidence (at 1, 2, 3 and 4 quarter lags) as explanatory variables but does not take into account who was in political power
- `model_full` - which adds a dummy indicator for periods with a Labour Prime Minister, and also an interaction of that indicator with the first quarter lag of business confidence

All the models were fit with `auto.arima` from Rob Hyndman's `forecast` R package, which uses a well-tested algorithm to choose the best time series approach (combination of moving average randomness and lagged values of the response variable) for the response variable itself, in this case quarterly growth in seasonally adjusted real GDP.

Time series data is not worth as much, data point for data point, as is data that is sampled independently of a correlating dimension such as time.  The modelling in this section takes this into account (unlike far too much applied statistical/econometric work), and this adds some much-needed conservatism to the inference process.  Graphics such as those shown above are likely to lead to hasty conclusions if they are interpreted as though the data are all independently selected and equally valuable, rather than containing a lot of redundant information from the correlation over time.

The `model_apol` was the strongest of the three (with the lowest [AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion)). This was counter to my expectation that the simple univariate model would win.  This is a vote of confidence for business confidence measures having *something* to add to understanding of future economic growth.  This exercise doesn't compare them to other leading indicators (such as commodity and currency prices, to pick two of the most available and pertinent measures, particularly for an export-oriented economy like New Zealand with very large dairy and tourism industries), and it's quite possible that business confidence is picking up signals from those other measures.  But it certainly is picking up something that is of value.

An increase in business confidence of 1 point on the OECD scale (which would be a bit over one standard deviation of changes in the past decade or so) is estimated to lead to an increase in quarterly economic growth of somewhere between 0.0 and 0.3 percentage points - for example, from real seasonally adjusted quarterly growth of 1.00% to 1.15% (or to anywhere between 1.00% and 1.30%), all else being equal.  So plunges in business confidence are not worth panicking over, but shouldn't be ignored either.

## Modelling confidence on growth (and politics)

I also tried flipped the problem around.  Can we predict 'business confidence", knowing just past values of business confidence, the political party in power, and current and past values of GDP?  Following the same process as above, I came to a slightly surprising conclusion - neither the party in power, nor current and lagged values of GDP growth, not a combination of the two, add materially to a model trying to explain business confidence.  My best model to explain business confidence was a simple univariate time series model.  In other words, the best predictor (of the very limited set of candidates) of business confidence is past business confidence.

## Code


{% highlight R lineanchors %}
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
# Dates taken from https://en.wikipedia.org/wiki/List_of_Prime_Ministers_of_New_Zealand

# a data frame of who was in power for every day over a 70 year period:
power <- data_frame(
  date = as.Date(c(
    "12/12/1957", "12/12/1960", "8/12/1972", "12/12/1975", 
    "26/07/1984", "2/11/1990", "5/12/1999", "19/11/2008", "26/10/2017"), format = "%d/%m/%Y"),
  pm_party = c(
    "Labour", "National", "Labour", "National",
    "Labour", "National", "Labour", "National", "Labour"),
  # the pm_id identifier is used later in grouping data together to avoid annoying connecting lines
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

# roll up by month for later use:		 
power_m <- power %>%
  group_by(yr, mon, pm_party, pm_id) %>%
  summarise(freq = n()) %>%
  group_by(yr, mon) %>%
  summarise(pm_party = pm_party[freq == max(freq)],
            pm_id = pm_id[freq == max(freq)])

# roll up by quarter for later use:
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

bc_nz %>%
  ggplot(aes(x = Time, y = Value, colour = pm_party, group = pm_id)) +
  geom_step() +
  labs(x = "", y = "Amplitude-adjusted business confidence index\nlong term average = 100",
       caption = "Source: OECD, Business Confidence Index; Wikipedia, Prime Ministers of New Zealand") +
  ggtitle("Monthly business confidence in New Zealand") +
  scale_colour_manual("Prime Minister's party:", values = parties_v)



# There's a small monthly seasonality present in business confidence
bc_ts <- ts(bc_nz$Value, start = c(1961, 6), frequency = 12)

par(family = main_font, font.main = 1)
plot(stl(bc_ts, s.window = 7), main = "Weak and variable seasonality in New Zealand monthly business confidence")

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

par(family = main_font, font.main = 1)
plot(stl(gdp_ts, s.window = 7), main = "Strong and consistent seasonality in New Zealand quarterly GDP")

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

ggplot(gdp_q, aes(x = yr_num, y = growth, colour = pm_party, group = pm_id)) +
  geom_line() +
  labs(x = "", y = "Quarterly growth in seasonally adjusted GDP\n(Compares each quarter to the previous; not annualised)",
       caption = "Source: Stats NZ, chain volume GDP, production measure") +
  scale_y_continuous(label = percent) +
  scale_colour_manual("Prime Minister's party:", values = parties_v) +
  ggtitle("Growth rates in New Zealand GDP over time") 


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

# Draw a connected scatter plot:  
comb %>%
  select(yr_num:bc_lag4) %>%
  gather(variable, value, -yr_num, -growth, -gdp_sa, -pm_party, -pm_id) %>%
  ggplot(aes(y = growth, x = value, colour = yr_num)) +
  scale_colour_viridis(option = "B") +
  facet_wrap(~variable, scale = "free_x") +
  geom_path() +
  geom_smooth(method = "lm") +
  scale_y_continuous(label = percent)+
  labs(y = "Quarterly growth in seasonally adjusted real GDP",
       x = "Value of business confidence, or lagged business confidence, or lagged quarterly growth",
       caption = "Source: OECD (business confidence) and Stats NZ (chain volume GDP, production measure)") +
  ggtitle("Relationship between business confidence and economic growth",
          "Business confidence ('bc') is positively correlated with future economic growth,\nbut not as strongly as past values of growth itself.") 


# Let's quantify those correlations:
cors <- comb %>%
  select(growth:bc_lag4, -pm_party, -pm_id) %>%
  slice(2:n()) %>%
  cor %>%
  as.data.frame()
cors[ , "correlate"] <- row.names(cors)

cors %>%
  filter(correlate != "growth") %>%
  mutate(correlate = fct_reorder(correlate, growth)) %>%
  ggplot(aes(x = growth, y = correlate)) +
  geom_point() +
  labs(x = "Correlation coefficient with real quarterly growth in seasonally adjusted GDP",
       caption = "Source: OECD (business confidence) and Stats NZ (chain volume GDP, production measure)") +
  ggtitle("Relationship between business confidence and economic growth",
          "Business confidence is positively correlated with future economic growth, but not as strongly as is past values of growth itself.")

# alternative version of the connected scatter plot, this time with political party in power:
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
# but does included lagged values of business confidence (ie model_apol):
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
# model_nothing is lowest
{% endhighlight %}

