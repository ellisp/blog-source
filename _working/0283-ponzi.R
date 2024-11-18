



library(tidyverse)




ponzi <- function(orig_investors = 10, 
                  mu = 100, 
                  sd = 0, 
                  new_investor_growth = 1,
                  invest_more_rate = 0.1,
                  withdraw_small_rate = 0.1,
                  withdraw_all_rate = 0.05,
                  roll_over_rate = (1 - invest_more_rate - 
                                         withdraw_small_rate - withdraw_all_rate),
                  ceiling = 1e6){
  
  number_new_investors <- orig_investors
  
  status <- tibble(id = 1:orig_investors,
                   invested = mu,
                   value_tmp = mu,
                   value = mu,
                   withdrawn = 0,
                   time_period = 1)
  
  cash_on_hand <- with(status, sum(invested) - sum(withdrawn))
  
  while(cash_on_hand > 0 & max(status$id) < ceiling){
    update <-  status |>
      filter(time_period == max(time_period)) |>
      mutate(value_tmp = value_tmp * 2) |>
      mutate(action = sample(c("withdraw_all", "withdraw_small", "rollover", "invest"),
                             size = n(),
                             replace = TRUE,
                             prob = c(withdraw_all_rate, withdraw_small_rate, roll_over_rate, invest_more_rate)))|>
      mutate(withdrawn = case_when(
        action == "withdraw_all" ~ withdrawn + value_tmp,
        action == "withdraw_small" ~ withdrawn + mu,
        TRUE ~ withdrawn
      )) |>
      mutate(value = case_when(
        action == "withdraw_all" ~ 0,
        action == "withdraw_small" ~ value_tmp - mu,
        action == "rollover" ~ value_tmp,
        action == "invest" ~ value_tmp + 100
      )) |>
      mutate(invested = case_when(
        action == "invest" ~ invested + 100,
        TRUE ~ invested
      ))  |>
      select(-action) |>
      mutate(time_period = max(status$time_period + 1))
    
    # this is doing it as a ratio of all 
    number_new_investors <- round(new_investor_growth * number_new_investors)
    if(number_new_investors > 0){
    
      new_investors <- tibble(id = 1:number_new_investors + max(status$id),
                              invested = mu,
                              value_tmp = mu,
                              value = mu,
                              withdrawn = 0,
                              time_period = unique(update$time_period))
    } else {
      new_investors <- tibble()
    }  
    
    status <- rbind(status, update, new_investors)
    
    cash_on_hand <- status |>
      filter(time_period == max(time_period)) |>
      summarise(x = sum(invested) - sum(withdrawn)) |>
      pull(x)
    
    total_invested <- status |>
      filter(time_period == max(time_period)) |>
      summarise(x = sum(invested)) |>
      pull(x)
    
    
  }  
  
  return(status)
}

status <- ponzi(new_investor_growth = 1.5, invest_more_rate = 0.2, 
                withdraw_all_rate = 0.01)

status  |>
  filter(time_period == max(time_period)) |>
  summarise(total_invested = sum(invested),
            total_withdrawn = sum(withdrawn),
            paper_value = sum(value),
            months = unique(time_period),
            total_investors = max(id),
            leverage = round(paper_value / total_invested)) |>
  t()

tail(status)
max(status$id)
