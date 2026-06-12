
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

#-------------------PDFs------------------
# For the more recent years no Excel tables are published, so need
# to use the PDFs and extract total from there

tbl <- tibble(local_path = list.files(pdf_dir, pattern = ".pdf$", full.names = TRUE))

parse_pdf_visitors <- function(path) {
  txt <- tryCatch(pdf_text(path), error = function(e) {
    message("  [WARN] pdftools could not read: ", basename(path))
    NULL
  })
  if (is.null(txt)) return(NA_integer_)
  full_text <- paste(txt, collapse = "\n")
  patterns <- c(
    "totaling[^0-9(]{0,10}\\(?([0-9,]+)\\)?",
    "totalling[^0-9]{0,10}([0-9,]+)"
  )
  for (pat in patterns) {
    m <- str_match(full_text, pat)[, 2]
    if (!is.na(m)) return(as.integer(str_remove_all(m, ",")))
  }
  message("  [WARN] Could not parse visitor count from: ", basename(path))
  NA_integer_
}

found <- tbl |> filter(file.exists(local_path))
if (nrow(found) == 0) {
  message("  [INFO] No local PDFs found in '", pdf_dir, "/'. Returning empty.")
  return(tibble())
}

message("  Parsing ", nrow(found), " local PDFs...")
pdf_tbl <- found |>
  mutate(arrivals = map_int(local_path, \(p) {
    message("  ", basename(p))
    parse_pdf_visitors(p)
  })) |>
  filter(!is.na(arrivals)) |>
  mutate(date = extract_date(local_path))

View(pdf_tbl)

#-----------------Excel versions------------
# For May 2023 and earlier we can get the data for multiple months
# at a time from Table 1 of the Excel tables. The May 2023 Excel
# file goes back to 2017 January (although the rows are hidden)


x <- read_excel("samoa_pdfs/May_23.xlsx", sheet = "Table 1", 
                 range = "D48:D130",
                 col_names = "arrivals") |> 
  drop_na() |> 
  pull(arrivals)

historical <- tibble(arrivals = x, 
               date = seq(as.Date("2017-01-01"), as.Date("2023-05-01"), by = "month"))

samoa_arrivals <- pdf_tbl |> 
  select(date, arrivals) |>
  bind_rows(historical) |> 
  arrange(date) |> 
  mutate(date_month = yearmonth(date)) |> 
  as_tsibble(index = date_month)

# Create Easter regressor as a ts object 
easter_reg <- genhol(easter, start = -4, end = 4,
                     frequency = 12)

samoa_arrivals$easter <- as.numeric(window(easter_reg, 
                           start = c(2017, 1), 
                           end = c(2026, 4)))

covid_reg <- ts(
  as.numeric(seq(as.Date("2017-01-01"), as.Date("2029-04-01"), by = "month") %in%
    seq(as.Date("2020-04-01"), as.Date("2022-07-01"), by = "month")),
  start     = c(2017, 1),
  frequency = 12
)
#-----------------------analysis----------------

sa_ts <- ts(samoa_arrivals$arrivals, frequency = 12, start = c(2017, 1))

tail(samoa_arrivals)

ggplot(samoa_arrivals, aes(x = date, y = arrivals)) +
  geom_line()

fit_ts <- seas(sa_ts, xreg = covid_reg)
summary(fit_ts)
# log transfofrm, SARIMA (3 1 1)(0 1 1)
# 6 outliers in the covid months even after taking the xreg in account.

tscomponents <- series(fit_ts, c("seats.trend", "seats.seasonal", "seats.irregular"))
plot(tscomponents)
final(fit_ts)

plot(final(fit_ts))
coef(fit_ts)

# Another examplefor comparison
m <- seas(AirPassengers)
summary(m)
# note transform: log, Easter included as a regressor, one outlier


#----------fable version-------
fit_fb <- samoa_arrivals |> 
  model(X_13ARIMA_SEATS(
    arrivals ~ xreg(covid_reg)
  ))

report(fit_fb)
summary(fit_ts)
# two models are identical eg log transform, same SARIMA parameters, etc

plot(components(fit_fb)$trend, tscomponents[, 1])

plot(components(fit_fb)$season_adjust, final(fit_ts))

fit_fb |> 
  components() |> 
  autoplot()
