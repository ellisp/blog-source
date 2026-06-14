
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

# path <- found[1, ]$local_path

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
#-----------------------analysis----------------
the_caption = "Source: Samoa Bureau of Statistics"

p1 <- ggplot(samoa_visitors, aes(x = date, y = visitors)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  labs(x = "",
       y = "Visitor arrivals",
      title = "Visitor arrivals per month to Samoa",
    subtitle = "Unadjusted originals",
  caption = the_caption)

svg_png(p1, "../img/0322-original", w = 10, h = 5)

sa_ts <- ts(samoa_visitors$visitors, frequency = 12, start = c(2017, 1))


fit_ts_war <- seas(sa_ts, xreg = cbind(covid_reg, war_reg))
summary(fit_ts_war)

fit_ts <- seas(sa_ts, xreg = covid_reg)
summary(fit_ts)
# log transform, SARIMA (1 1 0)(0 1 1)
# 6 outliers in the covid months even after taking the xreg in account.

# Another examplefor comparison
m <- seas(AirPassengers)
summary(m)
# note transform: log, Easter included as a regressor, one outlier


#----------fable version-------
fit_fb <- samoa_visitors |> 
  model(X_13ARIMA_SEATS(
    visitors ~ xreg(covid_reg)
  ))

report(fit_fb)
summary(fit_ts)
# two models are identical eg log transform, same SARIMA parameters, etc


plot_comp <- function(){
  par(mfrow = c(1, 2), bty = "l")
  plot(components(fit_fb)$trend, trend(fit_ts), main = "Comparison of trend",
        xlab = "Fit with fabletools::model(X_13ARIMA_SEATS...",
        ylab = "Fit with seasonal::seas")
  abline(0, 1)
  grid()
  # identical

  plot(components(fit_fb)$season_adjust, final(fit_ts), main = "Comparison of seasonally adjusted",
        xlab = "Fit with fabletools::model(X_13ARIMA_SEATS...",
        ylab = "Fit with seasonal::seas")
  abline(0, 1)
  grid()
  # identical
}

svg_png(plot_comp, "../img/0322-comparisons", w = 9, h = 5)

fit_fb |> 
  components() |> 
  autoplot() |> 
  svg_png("../img/0322-autoplot", w = 10, h = 6)

comp_data <- fit_fb |> 
  components() |> 
  # the 'trend' that comes straight from the decomposition does
  # not adjust ofr the Covid coefficient and looks pretty weird
  # so it is more intuitive to present it after adjustment for
  # Covid, which we need to calculate by multiplying (because of
  # the log transform that SEATS used autoamtically):
  mutate(covid = as.numeric(covid_reg)[1:nrow(samoa_visitors)],
         trend_adj = trend * exp(covid * coef(fit_ts)[1])) 

p_final <- comp_data|> 
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

svg_png(p_final, "../img/0322-final-presentation", w = 10, h = 5)
