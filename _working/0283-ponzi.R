

library(tidyverse)


# TODO 
# - add a 'fee' that is extracted by the scammers each month
# - make the amount added a random log normal number
# - make the random amount pulled towards the average for that individual from the past

#' Generate samples from a log normal distribvution given E(X) and coefficient
#' of variation
#' @details Only needed because rlnorm() has the parameters of hte Normal
#' distribution that log(X) follows and I wanted a version that used the
#' actual mean of X and its coefficient of variation.
#' 
#' Also (important) because if the coefficient of variation is 0, it
#' still returns values!
#' 
rlognormal <- function(n, ex, cv){
  if(cv == 0){
    output <- rep(ex, n)
  } else {
    
    sdlog <- sqrt(log(cv ^ 2 + 1)) 
    
    meanlog <- log(ex) - (sdlog ^ 2) / 2
    
    
    output <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  }
  
  return(output)
  
}

# check that this function works

# If cv is zero, should just return a whole bunch of ex (i.e. 100):
stopifnot(mean(rlognormal(1000, 100, 0)) == 100)
stopifnot(sd(rlognormal(1000, 100, 0)) == 0)

# If cv is not zero, check that the mean and coefficient of variation
# are as expectd:
stopifnot(round(mean(rlognormal(10000, 100, 0.5))) == 100)
stopifnot(round(sd(rlognormal(10000, 100, 0.5)) / 100, 1) == 0.5)


#' Simulate ponzi scheme
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
                   invested = rlognormal(number_new_investors, mu, cv),
                   value_tmp = invested,
                   value = invested,
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
      mutate(incr_tmp = rlognormal(n(), mu, cv)) |>
      mutate(withdrawn = case_when(
        action == "withdraw_all" ~ withdrawn + value_tmp,
        action == "withdraw_small" ~ withdrawn + incr_tmp,
        TRUE ~ withdrawn
      )) |>
      mutate(value = case_when(
        action == "withdraw_all" ~ 0,
        action == "withdraw_small" ~ value_tmp - incr_tmp,
        action == "rollover" ~ value_tmp,
        action == "invest" ~ value_tmp + incr_tmp
      )) |>
      mutate(invested = case_when(
        action == "invest" ~ invested + incr_tmp,
        TRUE ~ invested
      ))  |>
      select(-action, -incr_tmp) |>
      mutate(time_period = max(status$time_period + 1))
    
    number_new_investors <- number_investors[unique(update$time_period)]
    if(number_new_investors > 0){
    
      new_investors <- tibble(id = 1:number_new_investors + max(status$id),
                              invested = rlognormal(number_new_investors, mu, cv),
                              value_tmp = invested,
                              value = invested,
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
                withdraw_all_rate = 0.01,
                cv = 0.5)

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
