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
  
  # Don't bother trying to download on weekends:
  if(wday(the_date, label = TRUE) %in% c("Sat", "Sun")){
    next()
    }
  
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
                                      WHERE b.variable = 'short_positions'
                                      ORDER BY observation_date")$od

d_variables <- dbGetQuery(con, "select variable_id, variable from d_variables")

all_csvs <- list.files("asic-shorts", pattern = "DailyAggShortPos", full.names = TRUE)

# we are going to do this backwards so product names are the latest ones
i =length(all_csvs)
for(i in i:1){
  
  the_csv <- all_csvs[i]
  the_date <- as.Date(str_extract(the_csv, "[0-9]+"), format= "%Y%m%d")
  
  # if we've already got short positions observations in the data for this date,
  # then break out of the loop and go to the next iteration
  if(as.character(the_date) %in% already_done_dates){next()}
  
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


#============Create a denormalised (wide) version of the main fact table=======
# In another database system this would be a materialized view or indexed view or similar,
# but we don't have that in SQLite so it is a straight out table. Note that this roughly
# doubles the size of the database on disk; and duplication means we need to remember
# to re-create this table whenevedr the original fact table updates.

sql1 <- "DROP TABLE IF EXISTS f_prices_and_volumes_w"

sql2 <- "
CREATE TABLE f_prices_and_volumes_w AS
SELECT 
	observation_date,
	c.ticker_yahoo,
	c.ticker_origin,
	c.latest_full_description,
	SUM(CASE WHEN variable = 'open' THEN value END) AS open,
	SUM(CASE WHEN variable = 'high' THEN value END) AS high,
	SUM(CASE WHEN variable = 'low' THEN value END) AS low,
	SUM(CASE WHEN variable = 'close' THEN value END) AS close,
	SUM(CASE WHEN variable = 'volume' THEN value END) AS volume,
	SUM(CASE WHEN variable = 'adjusted' THEN value END) AS adjusted,
	SUM(CASE WHEN variable = 'short_positions' THEN value END) AS short_positions
FROM f_prices_and_volumes AS a
INNER JOIN d_variables AS b
	ON a.variable_id = b.variable_id
INNER JOIN d_products AS c
	ON a.product_id = c.product_id
GROUP BY observation_date, ticker_yahoo, ticker_origin, latest_full_description"

dbSendStatement(con, sql1)
dbSendStatement(con, sql2) # takes a minute or so

#==============exploration===========

#------------Basic numbers---------

sql <- "
SELECT 
	variable,
	COUNT(1) AS n,
	COUNT(DISTINCT(product_id)) AS number_products,
	COUNT(DISTINCT(observation_date)) AS number_dates
FROM f_prices_and_volumes AS a
INNER JOIN d_variables AS b
	ON a.variable_id = b.variable_id
GROUP BY variable
ORDER BY b.variable_id"

dbGetQuery(con, sql) %>%
  kable() %>%
  kable_styling()

#---------Recent growth-----------

sql <- "
WITH annual_vals AS
	(SELECT
	    CAST(STRFTIME('%Y', observation_date) AS INT) AS year,
		AVG(adjusted) AS avg_adjusted,
		latest_full_description,
		ticker_origin
	FROM f_prices_and_volumes_w
	GROUP BY latest_full_description, ticker_yahoo, STRFTIME('%Y', observation_date))
SELECT 
	SUM(CASE WHEN year = 2020 THEN avg_adjusted END) AS adj_2020,
	SUM(CASE WHEN year = 2021 THEN avg_adjusted END) AS adj_2021,
	ticker_yahoo,
	latest_full_description
FROM annual_vals
WHERE year >= 2020
GROUP BY ticker_origin, latest_full_description
HAVING SUM(CASE WHEN year = 2020 THEN avg_adjusted END) > 0"

recent_growth <- dbGetQuery(con, sql) %>% as_tibble()

recent_growth
