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




#===============Get the short positions data=============


#----------Download-------------
# From:
# https://asic.gov.au/regulatory-resources/markets/short-selling/short-position-reports-table/

all_dates <- seq(from = as.Date("2010-06-16"), to = Sys.Date(), by = 1)
dir.create("asic-shorts", showWarnings = FALSE)

i = 1
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

all_products <- dbGetQuery(con, "SELECT product_id, ticker_yahoo 
                                 FROM d_products 
                                 WHERE exchange_origin = 'ASX'") %>%
  as_tibble()

already_done_dates <- dbGetQuery(con, "SELECT DISTINCT observation_date AS od
                                      FROM f_prices_and_volumes AS a
                                      INNER JOIN d_variables AS b
                                        On a.variable_id = b.variable_id
                                      WHERE b.variable = 'short_position'")$od

d_variables <- dbGetQuery(con, "select variable_id, variable from d_variables")

all_csvs <- list.files("asic-shorts", pattern = "DailyAggShortPos", full.names = TRUE)

# we are going to do this backwards so product names are the latest ones
i =length(all_csvs)
for(i in i:1){
  
  the_csv <- all_csvs[i]
  the_date <- as.Date(str_extract(the_csv, "[0-9]+"), format= "%Y%m%d")
  
  # if we've already got short positions observations in the data for this date,
  # then break out of the loop and go to the next iteration
  if(the_data %in% already_done_dates){break()}
  
  # The first 1400 files are actually tab-delimited and UTF-16, the
  # rest are genuine comma separated files and more standard encoding
  if(i <= 1400){
    d1 <- read.csv(the_csv, fileEncoding = "UTF-16", sep = "\t") 
  } else {
    d1 <- read.csv(the_csv) 
    # sometimes this still doesn't work and we go back to the other method:
    if(nrow(d1) < 10){
      d1 <- read.csv(the_csv, fileEncoding = "UTF-16", sep = "\t") 
    }
  }
  
  d2 <- d1 %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(observation_date = the_date,
           ticker_yahoo = paste0(str_trim(product_code), ".AX")) %>%
    left_join(all_products, by = "ticker_yahoo") %>%
    select(
      product_id,
      observation_date,
      short_positions = reported_short_positions,
      total_product_in_issue,
      short_positions_prop = x_of_total_product_in_issue_reported_as_short_positions,
      ticker_yahoo,
      product,
      product_code
    ) %>%
    # convert from percentage to proportion:
    mutate(short_positions_prop = short_positions_prop / 100)
  
  non_match <- sum(is.na(d2$product_id))
  if(non_match > 0){
    message(glue("Found {non_match} products in the short data not yet in the database"))
    print(select(filter(d2, is.na(product_id)), ticker_yahoo, product, observation_date))
    
    new_products <- d2 %>%
      filter(is.na(product_id)) %>%
      mutate(exchange_origin = 'ASX',
             latest_full_description_tc = tools::toTitleCase(tolower(product)),
             industry_group = NA,
             latest_observed = NA,
             ticker_origin = str_trim(product_code)) %>%
      select(ticker_yahoo,
             ticker_origin,
             exchange_origin,
             latest_full_description = product,
             latest_full_description_tc,
             industry_group,
             latest_observed)
    
    RSQLite::dbWriteTable(con, "d_products", new_products, row.names =FALSE, append = TRUE)
    
    all_products <- dbGetQuery(con, "SELECT product_id, ticker_yahoo 
                                 FROM d_products 
                                 WHERE exchange_origin = 'ASX'") %>%
      as_tibble()
  }
  
  upload_data <- d2 %>% 
    select(observation_date:ticker_yahoo) %>%
    left_join(all_products, by = "ticker_yahoo") %>%
    select(-ticker_yahoo) %>%
    gather(variable, value, -product_id, -observation_date) %>%
    left_join(d_variables, by = "variable") %>%
    select(product_id,
         observation_date,
         variable_id,
         value) %>%
    # a small number of occasions the short_positions_prop is NA or Inf because
    # the totla product in issue is 0 even though there are some short positions.
    # we will just filter these out
    filter(!is.na(value)) %>%
    mutate(observation_date = as.character(observation_date))
  
  dbWriteTable(con, "f_prices_and_volumes", upload_data, append = TRUE)
  
  # progress counter so we know it isn't just stuck
  if(i %% 100 == 0){cat(i)}
}


#================stock price and volume information===========

all_stocks <- dbGetQuery(con, "SELECT product_id, ticker_yahoo 
                               FROM d_products 
                               ORDER BY product_id") %>% as_tibble()

i=1
for(i in i:nrow(all_stocks)){
  # display a counter ever 20 iterations so we know we're making progress:
  if(i %% 20 == 0){
    cat(i, " ")
  }
  ax_ticker <- all_stocks[i, ]$ticker_yahoo
  
  latest <- as.Date(dbGetQuery(con, glue("select max(observation_date) as x from f_prices_and_volumes 
                        where product_id = {all_stocks[i, ]$product_id}"))$x)
  if(is.na(latest)){
    start_date <- as.Date("1980-01-01")
  } else {
    start_date <- latest + 1
  }
  
  if(start_date <= Sys.Date()){
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
          as_tibble() %>%
          mutate(product_id = all_stocks[i, ]$product_id) %>%
          gather(variable, value, -observation_date, -product_id) %>%
          inner_join(d_variables, by = "variable") %>%
          select(product_id,
                 observation_date,
                 variable_id,
                 value) %>%
          filter(observation_date >= start_date) %>%
          mutate(observation_date = as.character(observation_date))
        
        dbWriteTable(con, "f_prices_and_volumes", upload_data, append = TRUE)
      }
      
    },
    error=function(e){})
  }
}



#===============Update the observation dates in the dimension table============
# This is much more complicated with SQLite than in SQL Server because of the
# apparent inability of SQLite to elegantly update a table via a join with
# another table. There may be a better way than the below but I couldn't find it:

sql1 <- 
  "CREATE TABLE tmp AS
  SELECT product_id, max(observation_date) as the_date
  FROM f_prices_and_volumes 
  WHERE observation_date IS NOT NULL
  GROUP BY product_id;"

sql2 <- 
  "UPDATE d_products
  SET latest_observed = (SELECT the_date FROM tmp WHERE d_products.product_id = tmp.product_id)
  WHERE EXISTS (SELECT * FROM tmp WHERE d_products.product_id = tmp.product_id);"

sql3 <- "DROP TABLE tmp;"


dbSendQuery(con, sql1)
dbSendQuery(con, sql2)
dbSendQuery(con, sql3)

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
