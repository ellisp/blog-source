---
layout: post
title: Seasonal adjustment
date: 2026-06-14
tag: 
   - TimeSeries
   - Tourism
   - WorkRelated
description: Seasonal adjustment is easy but getting hold of data can be hard. In this case I wrangle Samoa's monthly visitor arrivals data from 2017 to present and then use the seas and fable R packages to seasonally adjust it and ask if there is an impact from Iran war fuel crisis (there isn't).
image: /img/0322-final-presentation.svg
socialimage: https:/freerangestats.info/img/0322-final-presentation.png
category: R
---

A reasonably straightforward post today. I wanted to look at monthly tourism numbers in Samoa. In fact I started to do this for Pacific islands in general, but the data wrangling challenges were sufficient that I only got as far as Samoa for now. There's interest in these at the moment because they would be a relatively timely indicator of possible economic damage from the fuel crisis related to the Iran war. Visitor numbers, inflation, and merchandise trade are part of the very select number of monthly published economic statistics in this part of the world (for a select subset of countries).

There's two things happening in this post:

* getting hold of the Samoa's visitor arrivals numbers; and
* seasonally adjusting them.

The latter is more interesting to me than the former, but unfortunately the former took most of the time.

## Data wrangling

Here's what the visitor numbers look like, once we've got them all into one data frame:

<object type="image/svg+xml" data='/img/0322-original.svg' width='100%'><img src='/img/0322-original.png' width='100%'></object>

The Samoa Bureau of Statistics has a nice Excel workbook up to May 2023, but from that date onwards the data is only available as far as I can see in PDF reports. Luckily these are all available as links from [a single page](https://www.sbs.gov.ws/migration/), but there seems to be either a skill issue on my part or some kind of block on the website that stops any systematic download of them all, so I had to download all the PDFs by hand one at a time. 

Then Claude helped me write a parser to find the visitor arrivals number in each PDF. Actually, Claude wasn't any good at actually finding the right number, but it did give me a pattern I could adapt, much as in the old days I would have used Stack Overflow. There are a lot of numbers in each PDF and we need the right one&mdash;visitor arrivals, not total arrivals (yes, that's an em dash, but I write them all by hand). In this case it turns out that the trick is that the PDFs all include the sentence "Overall visitor numbers for the month under review stood at [number]", and luckily there is no other use of the word "stood" in the document.

The other fiddly thing was extracting the actual date each PDF referred to. Then everything needed to be tested. In the end, it would have been quicker to just manually type the 35 numbers I needed. But here's the code that does all the data wrangling.

{% highlight R lineanchors %}
library(tidyverse)
library(rvest)
library(httr2)
library(lubridate)
library(pdftools)   
library(readxl)
library(seasonal)
library(tsibble)
library(fable)
library(feasts)
library(ggtext)
library(scales)

#' Extract date from messy filenames
extract_date <- function(messy_date){
  tibble(path = messy_date) |>
  mutate(
    stem      = tools::file_path_sans_ext(basename(path)),
    # Remove any leading word followed by _ or - (e.g. "Migration_")
    stem      = str_remove(stem, "^([A-Za-z]+[_-])+(?=[A-Z])"),
    month_str = str_extract(stem, "[A-Za-z]{3,}"),
    # Normalise non-standard abbreviations
    month_str = str_replace(month_str, "^Sept$", "Sep"),
    year_str  = str_extract(stem, "\\d{4}|\\d{2}"),
    year      = if_else(nchar(year_str) == 2,
                        as.integer(paste0("20", year_str)),
                        as.integer(year_str)),
    date      = as.Date(paste(year, month_str, "01"), format = "%Y %B %d") |>
                  coalesce(as.Date(paste(year, month_str, "01"), format = "%Y %b %d"))
  ) |>
  pull(date)
}

test <- c("samoa_pdfs/April_25.pdf", "samoa_pdfs/Feb_25.pdf", "samoa_pdfs/Feb_26.pdf", 
"samoa_pdfs/Jan_26.pdf", "samoa_pdfs/January_25.pdf", "samoa_pdfs/July_25.pdf", 
"samoa_pdfs/June-25.pdf", "samoa_pdfs/March_2025.pdf", "samoa_pdfs/March_2026.pdf", 
"samoa_pdfs/May_25.pdf", "samoa_pdfs/Migration_April-2026.pdf",
"samoa_pdfs/Sept-24.pdf", "samoa_pdfs/Migration_Rep_June_2023.pdf")

extract_date(test)

#-------------------PDFs for recent data------------------
# For the more recent years no Excel tables are published, so need
# to use the PDFs and extract total from there
# These had to be downloaded by hand - nothing I tried was able to automate
# that. Download from https://www.sbs.gov.ws/migration/ and save
# in a subfolder /samoa_pdfs/.

pdf_dir <- "samoa_pdfs"
tbl <- tibble(local_path = list.files(pdf_dir, pattern = ".pdf$", full.names = TRUE))

parse_pdf_visitors <- function(path) {
  txt <- tryCatch(pdf_text(path), error = function(e) {
    message("  [WARN] pdftools could not read: ", basename(path))
    NULL
  })
  if (is.null(txt)) return(NA_integer_)
  full_text <- paste(txt, collapse = "\n")
  patterns <- c(
    "stood at [^0-9(]{0,10}\\(?([0-9,]+)\\)?"
  )
  for (pat in patterns) {
    m <- str_match(full_text, pat)[, 2]
    if (!is.na(m)) return(as.integer(str_remove_all(m, ",")))
  }
  message("  [WARN] Could not parse visitor count from: ", basename(path))
  NA_integer_
}

found <- tbl |> 
  filter(file.exists(local_path))

message("  Parsing ", nrow(found), " local PDFs...")
pdf_tbl <- found |>
  mutate(visitors = map_int(local_path, \(p) {
    message("  ", basename(p))
    parse_pdf_visitors(p)
  })) |>
  filter(!is.na(visitors)) |>
  mutate(date = extract_date(local_path))

#-----------------Excel versions for older data------------
# For May 2023 and earlier we can get the data for multiple months
# at a time from Table 1 of the Excel tables. The May 2023 Excel
# file goes back to 2017 January (although the rows are hidden)

fn <- "May_23.xlsx"
if(!file.exists(fn)){
  download.file("https://sbs.gov.ws/images/sbs-documents/social/Arrival/2023/May_23.xlsx",
                destfile = fn, mode = "wb")
}
x <- read_excel(fn, sheet = "Table 1", 
                 range = "D48:D130",
                 col_names = "visitors") |> 
  drop_na() |> 
  pull(visitors)

historical <- tibble(visitors = x, 
               date = seq(as.Date("2017-01-01"), as.Date("2023-05-01"), by = "month"))

#-----------combine and test-----------------------
samoa_visitors <- pdf_tbl |> 
  select(date, visitors) |>
  bind_rows(historical) |> 
  arrange(date) |> 
  mutate(date_month = yearmonth(date)) |> 
  as_tsibble(index = date_month)


# Test - some hand picked test cases, 4 from PDFs and 3 from the Excel
samoa_test <- tribble(~date, ~correct_visitors,
                      "2023-08-01", 16471,
                      "2024-04-01", 12644,
                      "2024-08-01", 17248,
                      "2026-04-01", 14188,
                      "2018-02-01", 7413,
                      "2020-12-01", 195,
                      "2022-06-01", 866) |> 
  mutate(date = as.Date(date))

stopifnot(
  samoa_test |> 
    anti_join(samoa_visitors, by = c("date", "correct_visitors" = "visitors")) |> 
    nrow() == 0
)
{% endhighlight %}

And here's the code that draws the basic time series chart I used earlier:

{% highlight R lineanchors %}
the_caption = "Source: Samoa Bureau of Statistics"

ggplot(samoa_visitors, aes(x = date, y = visitors)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  labs(x = "",
       y = "Visitor arrivals",
       title = "Visitor arrivals per month to Samoa",
       subtitle = "Unadjusted originals",
       caption = the_caption)
{% endhighlight %}

## Modelling seasonal decomposition

Phew. Ok, on to the more fun part of actually modelling. I intend to use the X-13ARIMA-SEATS tool. X‑13ARIMA‑SEATS is a program developed and maintained by the [US Census Bureau](https://www.census.gov/data/software/x13as.html) for seasonal adjustment and time‑series decomposition. It will automatically fit a SARIMA (seasonal autoregressive integrated moving average) time series model, has built in methods for identifying and dealing with outliers, by default adjusts for number of trading days and the moving Easter holiday, and allows the user to specify additional regression explanatory variables if you want. It's the go-to across the world for seasonal adjustment of official statistics.

X-13ARIMA-SEATS is available in R through the `seasonal` package (by Christoph Sax and Dirk Eddelbuettel). Downstream of that, the `feasts` and `fable` packages by (Mitchell O'Hara-Wild, Rob Hyndman and Earo Wangmake) make it easier to work with in a tabular, tidyverse approach.

The Covid period is an obvious dominating factor in tourism anywhere in recent decades, and shows up in the first chart I showed above. I'm also interested in the period since the USA-Israel-Iran war began to see if there is an impact from that. There's only two months of data (March and April 2026) since the war started, so an impact would have to be dramatic to show up, but it's worth checking.

X-13ARIMA-SEATS by default will fit forecasts when you ask it to model so you need to provide values of x regressor variables to cover not only the period of the data but a few periods out ahead. I'm doing this as simple time series vectors&mdash;I haven't yet worked out the easy way to do this in the tabular world of `fable`. Luckily, it seems I can use these vectors later even in `fable`. In a moment I'll use both `seasonal` directly and via `fable` to make sure I get the same results. In fact, updating my dated knowledge of time series modelling to use `fable` was one of my main motivations for this whole exercise.

Here's the code that makes the x regressors I'll be using for the presence of the Covid pandemic and for the Iran war:

{% highlight R lineanchors %}
#----------------x regressor variables-----------------

# Covid time series indicator to use as a regressor
covid_reg <- ts(
  as.numeric(seq(as.Date("2017-01-01"), as.Date("2029-04-01"), by = "month") %in%
    seq(as.Date("2020-04-01"), as.Date("2022-07-01"), by = "month")),
  start     = c(2017, 1),
  frequency = 12
)

# Iran war time series indicator
war_reg <- ts(
  as.numeric(seq(as.Date("2017-01-01"), as.Date("2029-04-01"), by = "month") %in%
    seq(as.Date("2026-03-01"), as.Date("2026-07-01"), by = "month")),
  start     = c(2017, 1),
  frequency = 12
)
{% endhighlight %}

### Directly with `seasonal`

Ok, it's modelling time. First, using just an old fashioned time series vector of Samoa's visitor arrivals and the `seasonal` package directly, here's fitting the X-13ARIMA-SEATS model with defaults using both the Covid and Iran war regressors:

{% highlight R lineanchors %}
sa_ts <- ts(samoa_visitors$visitors, frequency = 12, start = c(2017, 1))

fit_ts_war <- seas(sa_ts, xreg = cbind(covid_reg, war_reg))
summary(fit_ts_war)
{% endhighlight %}

Super simple. That gets us this output:

```
Call:
seas(x = sa_ts, xreg = cbind(covid_reg, war_reg))

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
xreg1             -3.10069    0.14567 -21.286  < 2e-16 ***
xreg2             -0.15937    0.13098  -1.217    0.224    
LS2020.Mar        -0.85036    0.14567  -5.838 5.29e-09 ***
AO2020.Dec        -0.75529    0.12350  -6.115 9.63e-10 ***
LS2021.May        -0.79012    0.13233  -5.971 2.36e-09 ***
AO2021.Jul        -1.36973    0.12378 -11.066  < 2e-16 ***
LS2022.May         0.89068    0.13233   6.731 1.68e-11 ***
LS2022.Aug        -1.07689    0.19608  -5.492 3.97e-08 ***
AR-Nonseasonal-01 -0.59006    0.07041  -8.380  < 2e-16 ***
MA-Seasonal-12     0.99961    0.08044  12.427  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

SEATS adj.  ARIMA: (1 1 0)(0 1 1)  Obs.: 112  Transform: log
AICc:  1608, BIC:  1634  QS (no seasonality in final):3.295  
Box-Ljung (no autocorr.): 26.42   Shapiro (normality): 0.9721 *
```

Some key things to note here. 

The data was log-transformed, which is good&mdash;I would certainly have chosen to do this, or at least a square root transform, given the way visitor arrivals variance increases as its mean does, all around the world.

The model eventually adopted is described as ARIMA (1 1 0)(0 1 1). This means the main series as a autoregression term on one lag, after one round of differencing; and the seasonal part has a moving average term on on lag, after one round of differencing. This is a very normal model for tourism numbers. it indicates a general trend/drive (the differencing in the main series), a tendency for the values in one month to be related one way or another to those in the month before (in this case with a negative correlation of -0.59) and a strong annual seasonality effect that changes slowly over time.

The Covid dummy variable (`xreg1`) is strongly negatively significant. With a coefficient of -3.1 and the log transform of the response variable, this means that during the Covid period the actual arrivals were on average `exp(-3.1) = 0.045` (ie 4.5% or down 95.5%) of during the non-Covid periods.

In contrast, we don't have a statistically significant effect for the Iran war (`xreg2`). For subsequent analysis I will take out that x regressor as I don't want it complicating the recent trend and seasonally adjusted figures.

Six months' data have been singled out as outliers and controlled for appropriately. These are all in the difficult-to-model Covid period of 2020 to 2022.

One point to note is that we don't have an Easter effect. I am 100% sure there is really an Easter effect in Samoa's visitor arrivals, but 9 years of data isn't enough to show it. Easter is sometimes in April and sometimes in March. But since 2017, it happens to have been in April every year apart from 2024. That's just not enough variation to distinguish it from regular monthly seasonal impacts.

To check my recollection that Easter is indeed checked for by default in X-13ARIMA-SEATS, I fit a model to the well known Box and Jenkins airline data:

```
> # Another examplefor comparison
+ m <- seas(AirPassengers)
+ summary(m)

Call:
seas(x = AirPassengers)

Coefficients:
                    Estimate Std. Error z value Pr(>|z|)    
Weekday           -0.0029497  0.0005232  -5.638 1.72e-08 ***
Easter[1]          0.0177674  0.0071580   2.482   0.0131 *  
AO1951.May         0.1001558  0.0204387   4.900 9.57e-07 ***
MA-Nonseasonal-01  0.1156204  0.0858588   1.347   0.1781    
MA-Seasonal-12     0.4973600  0.0774677   6.420 1.36e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

SEATS adj.  ARIMA: (0 1 1)(0 1 1)  Obs.: 144  Transform: log
AICc: 947.3, BIC: 963.9  QS (no seasonality in final):    0  
Box-Ljung (no autocorr.): 26.65   Shapiro (normality): 0.9908 
```

Here we see that the number of week days in a month and the moving Easter holiday are indeed in the final model, without me having to have asked for them to be checked. Easter has a positive impact on this air passengers series (1949 to 1960), and the number of week days in a month has a smaller negative impact.

So I refit the model without the war regressor and get an essentially identical result:

```
> fit_ts <- seas(sa_ts, xreg = covid_reg)
> summary(fit_ts)

Call:
seas(x = sa_ts, xreg = covid_reg)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
xreg              -3.10045    0.14690 -21.106  < 2e-16 ***
LS2020.Mar        -0.83292    0.14617  -5.698 1.21e-08 ***
AO2020.Dec        -0.75463    0.12449  -6.062 1.35e-09 ***
LS2021.May        -0.78975    0.13356  -5.913 3.36e-09 ***
AO2021.Jul        -1.36737    0.12476 -10.960  < 2e-16 ***
LS2022.May         0.89113    0.13356   6.672 2.52e-11 ***
LS2022.Aug        -1.07733    0.19782  -5.446 5.15e-08 ***
AR-Nonseasonal-01 -0.58577    0.07071  -8.284  < 2e-16 ***
MA-Seasonal-12     0.99912    0.07827  12.765  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

SEATS adj.  ARIMA: (1 1 0)(0 1 1)  Obs.: 112  Transform: log
AICc:  1607, BIC:  1631  QS (no seasonality in final):2.501  
Box-Ljung (no autocorr.): 26.63   Shapiro (normality): 0.9706 *
```


### With `fable` and `feasts`

`tsibble`, `fable` and `feasts` are a brilliant set of packages that let you work with time series in R in a more tabular and `tidyverse`-friendly way than the various older time series data structures let you. Most of my work with time series was before they were available, though, so I'm lacking confidence in how they work. Luckily it seems to be pretty straightforward.

I'd already done the critical step earlier with `as_tsibble(index = date_month)`, specifying my main `samoa_visitors` tibble is actually a time series tibble. Now the modelling using that tsibble is pretty simple:

{% highlight R lineanchors %}
#----------fable version-------
fit_fb <- samoa_visitors |> 
  model(X_13ARIMA_SEATS(
    visitors ~ xreg(covid_reg)
  ))

report(fit_fb)
{% endhighlight %}

Note we use `report()` rather than `summary()` to get the end result. I'm not going to print it here because it is literally identical to what we got earier with `summary(fit_ts)`.

The main appeal for me in using the `fable`/`feasts` approach is that it fits better with both my data wrangling workflow and my approach to `ggplot2` graphics. so here is a nice decomposition of hte original timeseries, produced with `autoplot(`

{% highlight R lineanchors %}
fit_fb |> 
  components() |> 
  autoplot() 
{% endhighlight %}

<object type="image/svg+xml" data='/img/0322-autoplot.svg' width='100%'><img src='/img/0322-autoplot.png' width='100%'></object>

Note that in this decomposition, the original 'visitors' series and the trend are on the original scale, but the 'seasonal' and 'irregular' components are expressed as multipliers. So a seasonal value of 1.2 means in a given month the value is 20% higher as a result of the seasonality than otherwise.

And here is my final, presentation version of the data:

<object type="image/svg+xml" data='/img/0322-final-presentation.svg' width='100%'><img src='/img/0322-final-presentation.png' width='100%'></object>

Produced with this code:

{% highlight R lineanchors %}
comp_data <- fit_fb |> 
  components() |> 
  # the 'trend' that comes straight from the decomposition does
  # not adjust ofr the Covid coefficient and looks pretty weird
  # so it is more intuitive to present it after adjustment for
  # Covid, which we need to calculate by multiplying (because of
  # the log transform that SEATS used autoamtically):
  mutate(covid = as.numeric(covid_reg)[1:nrow(samoa_visitors)],
         trend_adj = trend * exp(covid * coef(fit_ts)[1])) 

comp_data|> 
  ggplot(aes(x = date_month, y = season_adjust)) +
  geom_line(linewidth = 1.3) +
  geom_line(aes(y = trend_adj), colour = "steelblue", alpha = 0.9, linewidth = 1.2) + 
  geom_point(aes(y = visitors), colour = "grey70") +
  scale_y_continuous(label = comma) +
  labs(x = "",
       y ="Visitor arrivals",
      title = "Visitor arrivals per month to Samoa",
      subtitle = "<span style='color:grey60'>Original</span>, seasonally adjusted <span style='color:steelblue'>and trend (adjusted for Covid period).</span>",
    caption = "Source: data from Samoa Bureau of Statistics. Seasonal adjustment by freerangestats.info.") +
  theme(plot.subtitle = element_markdown())
{% endhighlight %}

What insight do we have? Well, not a lot really, other than the blindingly obvious trends of the devastation to the industry of Covid and the slow and noisy growth trend since then. We are at least well placed to make commentary on the impact of the fuel crisis on tourism, and can say that there isn't any evident yet. If and when we do see the impact, we'll be able to talk about in terms of trends and random variation, after having removed the seasonal element. So that's useful.

Well, that's all. Perhaps I'll get around in a later post to adding the other Pacific island countries with monthly tourism data&mdashFiji, Vanuatu, Cook Islands, French Polynesia being the key ones I'm aware of.


