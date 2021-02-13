library(tidyverse)
library(glue)
library(lubridate)
library(frs) # for download_if_fresh and execute_sql
library(janitor)
library(ggrepel)
library(kableExtra)
library(quantmod)
library(RSQLite)
library(DBI)

#=============database setup============================

con <- dbConnect(RSQLite::SQLite(), "stocks.sqlite")

sql <- "DROP TABLE IF EXISTS f_prices
DROP TABLE IF EXISTS d_products
"
dbExecute(con, "drop table if exists d_products")
dbExecute(con, "drop table if exists f_prices")

execute_sql(con, "0200-stocks-db-setup.sql", log_table = NULL)


dbGetQuery(con, "select * from d_products")
dbGetQuery(con, "select * from f_prices")

#===============Get the data=============


#----------Download-------------
# From:
# https://asic.gov.au/regulatory-resources/markets/short-selling/short-position-reports-table/

all_dates <- seq(from = as.Date("2010-06-16"), to = as.Date("2021-01-22"), by = 1)
dir.create("asic-shorts", showWarnings = FALSE)

# Began 8:54am
for(i in 1:length(all_dates)){
  the_date <- all_dates[i]
  
  m <- str_pad(month(the_date), width = 2, side = "left", pad = "0")
  y <- year(the_date)
  ch <- format(the_date, "%Y%m%d")
  fn <- glue("RR{ch}-001-SSDailyAggShortPos.csv")
  
  url <- glue("https://asic.gov.au/Reports/Daily/{y}/{m}/{fn}")
  
  # Only exists for trading days. Rather than bother to work out exactly the trading days,
  # we will just skip over any 404 error
  try(download_if_fresh(url, glue("asic-shorts/{fn}")))
}


#----------------------Import---------------------------

all_csvs <- list.files("asic-shorts", pattern = "DailyAggShortPos", full.names = TRUE)
all_data_l <- list()
i =1
for(i in i:length(all_csvs)){
  
  the_csv <- all_csvs[i]
  the_date <- as.Date(str_extract(the_csv, "[0-9]+"), format= "%Y%m%d")
  
  # The first 1400 files are actually tab-delimited and UTF-16, the
  # rest are genuine comma separated files and more standard encoding
  if(i <= 1400){
    d <- read.csv(the_csv, fileEncoding = "UTF-16", sep = "\t") 
  } else {
    d <- read.csv(the_csv) 
  }
  
  all_data_l[[i]] <- d %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(date = the_date)
  
  # progress counter so we know it isn't just stuck
  if(i %% 100 == 0){cat(i)}
}

all_data <- bind_rows(all_data_l) %>%
  mutate(product_code = str_trim(product_code)) 

# Rename one very long variable name
names(all_data) <- gsub("x_of_total_product_in_issue_reported_as_short_positions", 
                        "short_position_prop",
                        names(all_data))

# not sure where this column came from but it's just empty so let's drop it:
all_data$yth_p <-  NULL

# convert from percentage to proportion
all_data$short_position_prop <- all_data$short_position_prop / 100

# Rationalise the product names which sometimes change over time, so we have the latest
# one to use consistently over time in some charts
pnl <- all_data %>%
  group_by(product_code) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  select(product_code, recent_product_name = product) %>%
  ungroup()

all_data <- all_data %>%
  inner_join(pnl, by = "product_code")

#=================Exploratory analysis of short positions=====================

# so some companies had moments when they were very heavily shorted - more than 16 times
most_shorted <- all_data %>%
  group_by(recent_product_name, product_code) %>%
  summarise(max_sp = max(short_position_prop),
            # earliest and latest date the firm had that position (noting not necessarily
            # continuous between these two dates)
            max_sp_date_early = min(date[short_position_prop == max_sp]),
            max_sp_date_late = min(date[short_position_prop == max_sp]),
            max_sp_days = sum(short_position_prop == max_sp)) %>%
  ungroup() %>%
  arrange(desc(max_sp)) 

# 35 firms had more than 100% of their float shorted
most_shorted %>%    filter(max_sp > 1)

# usually only for 1 day (but of course that's not surprising as I required it to exactly match)
most_shorted %>%
  filter(max_sp > 1) %>%
  arrange(desc(max_sp_days))


# 46 firms had at least sometimes 50% shorted
heavily_shorted <- most_shorted %>%   
  filter(max_sp > 0.5) %>%
  select(product_code, max_sp)

extremes <- most_shorted %>%
  filter(max_sp > 5) %>%
  rename(short_position_prop = max_sp,
         date = max_sp_date_early) %>%
  arrange(date) %>%
  select(recent_product_name, product_code, short_position_prop, date)


all_data %>%
  inner_join(heavily_shorted, by = "product_code") %>%
  ggplot(aes(x = date, y = short_position_prop, colour = product_code)) +
  geom_line() +
  geom_text_repel(data = extremes, aes(label = product_code)) +
  scale_y_sqrt(label = percent, breaks = c(0.3, 1, 5, 10, 15)) +
  theme(legend.position =  "none",
        panel.grid.minor.y = element_blank()) +
  labs(y = "Percentage of product issue\nreported as short position",
       x = "",
       title = glue("The {nrow(heavily_shorted)} most shorted firms on the ASX"),
       subtitle = "Four unusual shorting events since 2010, two of them involving multiple products.
Chart shows the short positions of all products that at least once had 50% of product issue reported as short position.",
       caption = "Source: ASIC short position reports")

extremes %>%
  rename("Most shorted as %" = short_position_prop) %>%
  kable(digits = 0) %>%
  kable_styling()


#================stock price and volume information===========

#------------------Download stock price etc--------

codes_wanted <- heavily_shorted %>%
  mutate(product_code = paste0(product_code, ".AX")) %>%
  pull(product_code)


# chunk of code adapted from https://www.michaelplazzer.com/import-asx-data-r/
all_stocks_l <- list()

for (i in 1:length(codes_wanted)) {
  ax_ticker <- codes_wanted[i]
  
  tryCatch({
    df_get <- data.frame(getSymbols(
      ax_ticker,
      src = 'yahoo',
      from = min(all_data$date),
      to = max(all_data$date),
      auto.assign = FALSE))
    
    df_get$date <- row.names(df_get)
    row.names(df_get) <- NULL
    df_stock <- tibble(ax_ticker=ax_ticker,df_get)
    names(df_stock) <- tolower(c("ax_ticker","OPEN","HIGH","LOW","CLOSE","VOLUME","ADJUSTED","Date"))
    
    all_stocks_l[[i]] <- df_stock
  },
  error=function(e){})
}

all_stocks_data <- bind_rows(all_stocks_l) %>%
  mutate(date = as.Date(date),
         product_code = gsub("\\.AX$", "", ax_ticker) ) %>%
  group_by(product_code) %>%
  filter(!is.na(close)) %>%
  arrange(date) %>%
  mutate(close_index = close / close[1]) %>%
  ungroup()

#---------------Explore stock price etc----------------------

all_stocks_data  %>%
  ggplot(aes(x = date, y = close_index, colour = ax_ticker)) +
  geom_line() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank()) +
  scale_y_log10(label = comma_format(accuracy = 0.001)) +
  labs(x = "",
       y = "Closing price\n(as index, first observed value = 1)",
       title = "Closing stock prices of the 46 most shorted products on the ASX")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  ggplot(aes(x = close_index, y = short_position_prop, colour = product_code)) +
  geom_path() +
  geom_smooth(aes(colour = NULL), method = "gam", colour = "black") +
  #geom_point() +
  scale_y_log10(label = percent) +
  scale_x_log10() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(x = "Closing price (as index, first observed value = 1)",
       y = "Percentage of product issue\nreported as short position")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  ggplot(aes(x = volume / 1000, y = short_position_prop, colour = product_code)) +
  geom_path() +
  geom_smooth(aes(colour = NULL), method = "gam", colour = "black") +
  scale_y_log10(label = percent) +
  scale_x_log10(label = comma_format(suffix = "k")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(x = "Volume of trades",
       y = "Percentage of product issue\nreported as short position")


#====================Combine the two=================

set.seed(126)
some_products <- sample(all_stocks_data$product_code, 13)

all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  filter(product_code == some_products) %>%
  ggplot(aes(x = volume / 1000, y = short_position_prop, colour = year(date))) +
  geom_path() +
  geom_smooth(method = "gam", colour = "black") +
  facet_wrap(~glue("{product_code}: {recent_product_name}"), scales = "free") +
  scale_y_log10(label = percent) +
  scale_x_log10(label = comma_format(suffix = "k")) +
  scale_colour_viridis_c() +
  theme(legend.position = "right") +
  labs(x = "Volume of trades",
       y = "Percentage of product issue\nreported as short position",
       colour = "Year")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  filter(product_code == some_products) %>%
  ggplot(aes(x = volume / 1000, y = close, colour = year(date))) +
  geom_path() +
  geom_smooth(method = "gam", colour = "black") +
  facet_wrap(~glue("{product_code}: {recent_product_name}"), scales = "free") +
  scale_y_log10(label = dollar) +
  scale_x_log10(label = comma_format(suffix = "k")) +
  scale_colour_viridis_c() +
  theme(legend.position = "right") +
  labs(x = "Volume of trades",
       y = "Closing price",
       colour = "Year")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  filter(product_code == some_products) %>%
  ggplot(aes(x = close, y = short_position_prop, colour = year(date))) +
  geom_path() +
  geom_smooth(method = "gam", colour = "black") +
  facet_wrap(~glue("{product_code}: {recent_product_name}"), scales = "free") +
  scale_y_log10(label = percent) +
  scale_x_log10(label = dollar) +
  scale_colour_viridis_c() +
  theme(legend.position = "right") +
  labs(x = "Closing price",
       y = "Percentage of product issue\nreported as short position",
       colour = "Year")
