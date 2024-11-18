



library(tidyverse)


status <- tibble(id = 1:orig_starters,
       invested = 100,
       value_tmp = 100,
       value = 100,
       withdrawn = 0,
       time_period = 1)

cash_on_hand <- with(status, sum(invested) - sum(withdrawn))

while(cash_on_hand > 0){
  update <-  status |>
    filter(time_period == max(time_period)) |>
    mutate(value_tmp = value_tmp * 2) |>
    mutate(action = sample(c("withdraw_all", "withdraw_small", "rollover", "invest"),
                           size = n(),
                           replace = TRUE,
                           prob = c(0.05, 0.2, 0.7, 0.05)))|>
    mutate(withdrawn = case_when(
      action == "withdraw_all" ~ withdrawn + value_tmp,
      action == "withdraw_small" ~ withdrawn + 100,
      TRUE ~ withdrawn
    )) |>
    mutate(value = case_when(
      action == "withdraw_all" ~ 0,
      action == "withdraw_small" ~ value_tmp - 100,
      action == "rollover" ~ value_tmp,
      action == "invest" ~ value_tmp + 100
    )) |>
    mutate(invested = case_when(
      action == "invest" ~ invested + 100,
      TRUE ~ invested
    ))  |>
    select(-action) |>
    mutate(time_period = max(status$time_period + 1))
  
  new_investors <- tibble(id = 1:5 + max(status$id),
                          invested = 100,
                          value_tmp = 100,
                          value = 100,
                          withdrawn = 0,
                          time_period = unique(update$time_period))
  
  
  status <- rbind(status, update, new_investors)
  
  cash_on_hand <- status |>
    filter(time_period == max(time_period)) |>
    summarise(x = sum(invested) - sum(withdrawn)) |>
    pull(x)
  
}  

status  |>
  filter(time_period == max(time_period)) |>
  summarise(total_invested = sum(invested),
            total_withdrawn = sum(withdrawn),
            paper_value = sum(value),
            months = unique(time_period))


ponzi <- function(orig_starters = 10, 
                  mu = 100, 
                  sd = 0, 
                  new_starters = 0,
                  invest_more_rate,
                  roll_over_rate,
                  withdraw_orig_rate,
                  withdraw_all_rate = (1 - invest_more_rate - 
                                         roll_over_rate - withdraw_orig_rate)){
  
}