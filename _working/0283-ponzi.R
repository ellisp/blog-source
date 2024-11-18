

#https://bsky.app/intent/compose?text=I%27m%20reading%20through%20the%20Bluesky%20API%20docs%21%20%F0%9F%A6%8B%0Ahttps%3A//docs.bsky.app

library(tidyverse)


# TODO 
# - add a 'fee' that is extracted by the scammers each month
# - make the amount added a random log normal number
# - make the random amount pulled towards the average for that individual from the past

rlognormal <- function(n, mu, cv){
  if(cv == 0){
    output <- rep(mu, n)
  } else {
    sdlog <- sqrt(log(cv ^2 + 1))
    meanlog <- log(mu) - (1 / (2 * sdlog ^ 2))
    output <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  }
  
  return(output)
  
}

mean(log(rlnorm(2000, 5, log(2.5))))

stopifnot(mean(rlognormal(1000, 100, 0)) == 100)
stopifnot(sd(rlognormal(1000, 100, 0)) == 0)
stopifnot(round(mean(rlognormal(1000, 100, 2))) == 100)
stopifnot(round(sd(rlognormal(1000, 100, 2)) / 100) == 2)



ponzi <- function(number_investors = c(1:100, 100:1) * 10, 
                  mu = 100, 
                  cv = 0, 
                  invest_more_rate = 0.1,
                  withdraw_small_rate = 0.1,
                  withdraw_all_rate = 0.05,
                  roll_over_rate = (1 - invest_more_rate - 
                                         withdraw_small_rate - withdraw_all_rate),
                  ceiling = 1e7){
  
  #---------checks on number of investors------------
  if(min(number_investors) < 0){
    stop("number_investors should be a vector of numbers of 0 or greater")
  }
  
  if(number_investors[1] < 1){
    stop("First element of number_investors should be greater than 0")
  }
  
  number_investors <- round(number_investors)
  
  # make sure we have enough for 2000time periods
  nni <- length(number_investors) 
  if(nni < 2000){
    number_investors <- c(number_investors, 
                          rep(number_investors[length(number_investors)]), 2000 - nni)
  }
  
  #---------------------set up month 1------------------
  number_new_investors <- number_investors[1]
  
  status <- tibble(id = 1:number_new_investors,
                   invested = mu,
                   value_tmp = mu,
                   value = mu,
                   withdrawn = 0,
                   time_period = 1)
  
  cash_on_hand <- with(status, sum(invested) - sum(withdrawn))
  
  #---------------------months 2 and onwards---------------------
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
    
    number_new_investors <- number_investors[unique(update$time_period)]
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

status <- ponzi(invest_more_rate = 0.2, 
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

status  |>
  group_by(time_period) |>
  summarise(`Paper value` = sum(value),
            `Real value` = pmax(0, sum(invested) - sum(withdrawn))) |>
  gather(variable, value, -time_period) |>
  ggplot(aes(x = time_period, y = value, colour = variable)) +
  geom_line() +
  scale_y_log10(label = dollar)

tail(status)
max(status$id)
