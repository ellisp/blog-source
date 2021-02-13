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


#-----------Define database------------------
if(!"stocks.sqlite" %in% list.files()){
  # if this is the very first run we need to create the empty database and its tables
  con <- dbConnect(RSQLite::SQLite(), "stocks.sqlite")
  execute_sql(con, "0199-stocks-db-setup.sql", log_table = NULL)
  
} else {
  con <- dbConnect(RSQLite::SQLite(), "stocks.sqlite")
}

#------------set up product dimension-----------

asx_cos <- read.csv("http://www.asx.com.au/asx/research/ASXListedCompanies.csv",skip=1) %>%
  mutate(ticker_yahoo = paste0(ASX.code, ".AX"),
         exchange_origin = "ASX",
         latest_observed = as.Date(NA),
         latest_full_description_tc = tools::toTitleCase(tolower(Company.name))) %>%
  select(
    ticker_yahoo,
    ticker_origin = ASX.code,
    exchange_origin,
    latest_full_description = Company.name,
    latest_full_description_tc,
    industry_group = GICS.industry.group,
    latest_observed
  )

RSQLite::dbWriteTable(con, "d_products", asx_cos, row.names =FALSE, append = TRUE)

dbGetQuery(con, "select * from d_products") %>%
  as_tibble()



#================stock price and volume information===========

all_stocks <- dbGetQuery(con, "select product_id, ticker_yahoo from d_products") %>% as_tibble()

for(i in 1:nrow(all_stocks)){
  if(i %% 20 == 0){
    cat(i, " ")
  }
  ax_ticker <- all_stocks[i, ]$ticker_yahoo
  
  latest <- as.Date(dbGetQuery(con, glue("select max(observation_date) as x from f_prices 
                        where product_id = {all_stocks[i, ]$product_id}"))$x)
  if(is.na(latest)){
    start_date <- as.Date("1980-01-01")
  } else {
    start_date <- latest + 1
  }
  
  tryCatch({
    df_get <- data.frame(getSymbols(
      ax_ticker,
      src = 'yahoo',
      from = start_date,
      to = Sys.Date(),
      auto.assign = FALSE)) 
    
    if(nrow(df_get) > 0){
      df_get$observation_date <- row.names(df_get)
      
      row.names(df_get) <- NULL
      names(df_get) <- c("open", "high", "low", "close", "volume", "adjusted", "observation_date")
      upload_data <- df_get %>%
        mutate(product_id = all_stocks[i, ]$product_id) %>%
        mutate(short_positions = NA,
               total_product_in_issue = NA,
               short_positions_prop = NA) %>%
        select(product_id,
               open,
               high,
               low,
               close,
               volume, 
               adjusted,
               short_positions,
               total_product_in_issue,
               short_positions_prop,
               observation_date) %>%
        filter(observation_date >= start_date)
    
      dbWriteTable(con, "f_prices", upload_data, append = TRUE)
    }
    
  },
  error=function(e){})
}

#------------------Download stock price etc--------


#===============Get the data=============


#----------Download-------------
# From:
# https://asic.gov.au/regulatory-resources/markets/short-selling/short-position-reports-table/

all_dates <- seq(from = as.Date("2010-06-16"), to = Sys.Date(), by = 1)
dir.create("asic-shorts", showWarnings = FALSE)

# Began 8:54am
i = i
for(i in i:length(all_dates)){
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
  
  d2 <- d %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(date = the_date)
  
  # progress counter so we know it isn't just stuck
  if(i %% 100 == 0){cat(i)}
}

all_data <- bind_rows(all_data_l) %>%
  mutate() 

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
