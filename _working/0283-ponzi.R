

library(tidyverse)
library(foreach)
library(doParallel)
library(glue)

#' Generate samples from a log normal distribvution given E(X) and coefficient
#' of variation
#'
#' @param n number of samples to generate
#' @param ex expected value of the distribution, on its observed (not log) scale
#' @param cv coefficient of variation (ie standard deviation as a proportion of
#'   the mean) of the distribution, on its observed (not log) scale
#' @details Only needed because rlnorm() has the parameters of the Normal
#'   distribution that log(X) follows and I wanted a version that used the
#'   actual mean of X and its coefficient of variation.
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
set.seed(321)
stopifnot(round(mean(rlognormal(100000, 100, 0.5))) == 100)
stopifnot(round(sd(rlognormal(100000, 100, 0.5)) / 100, 1) == 0.5)

p <- function(){
par(bty = "l", mfrow = c(1, 2))
plot(density(rlognormal(1000, 100, cv = 0.5)), 
     main = "Log-normal distribution,\nmean 100 and cv 0.5")
plot(density(rlognormal(1000, 100, cv = 2)), 
     main = "Log-normal distribution\nmean 100 and cv 2")
}

svg_png(p, "../img/0283-log-normal", h = 3)

#' Simulate ponzi scheme that doubles in paper value each time period
#'
#' @param number_investors vector of number of new investors joining the scheme
#'   each time period. If this is less than 2000 time periods long it will be
#'   filled out with repeats of the last element.
#' @param mu average investment of each investor when they first join or invest
#'   further
#' @param cv coefficient of variation of the amount that investors each invest
#' @param invest_more_rate the proportion of investors in each time period that
#'   invest additional funds
#' @param withdraw_small_rate the proportion of investors in each time period
#'   that withdraw a small amount equivalent to their original investment
#' @param withdraw_all_rate the proportion of investors in each time period that
#'   withdraw the entire paper value of their investment
#' @param roll_over_rate the proportion of investors who simply leave their
#'   investment as it is at the end of each investment, neither withdrawing or
#'   investing further
#' @param ceiling the maximum number of investors that can ever be involved in
#'   the scheme, including those that have already withdrawn all funds. If the
#'   cumulative sum of number_investors exceeds ceiling then no more investors
#'   are added ie \code{ceiling} overrides \code{number_investors}
#' @param extraction_rate The proportion of the total real value of the scheme
#'   that the owners extract for their own use, each time period
#' @param keep_history whether to return the state of the scheme at each
#'   time_period (if TRUE), or only the final state (if FALSe)
#' @param time_multiplier how much the investors are promised their investment
#'   increases by each time period; defaults to 2 ie doubling
ponzi <- function(number_investors = c(1:100, 100:1) * 10, 
                  mu = 100, 
                  cv = 0, 
                  invest_more_rate = 0.1,
                  withdraw_small_rate = 0.1,
                  withdraw_all_rate = 0.05,
                  roll_over_rate = (1 - invest_more_rate - 
                                         withdraw_small_rate - withdraw_all_rate),
                  ceiling = 1e7,
                  extraction_rate = 0.01,
                  keep_history = TRUE,
                  time_multiplier = 2){
  
  #---------checks on number of investors------------
  if(min(number_investors) < 0){
    stop("number_investors should be a vector of numbers of 0 or greater")
  }
  
  if(number_investors[1] < 1){
    stop("First element of number_investors should be greater than 0")
  }
  
  number_investors <- round(number_investors)
  
  # make sure we have enough for 2000 time periods
  nni <- length(number_investors) 
  if(nni < 2000){
    number_investors <- c(number_investors, 
                          rep(number_investors[length(number_investors)]), 2000 - nni)
  }
  
  #--------------Other checks--------------
  if(invest_more_rate + withdraw_small_rate + withdraw_all_rate + roll_over_rate != 1){
    stop("invest_more_rate, withdraw_small_rate, withdraw_all_rate and roll_over_rate should add to one")
  }
  
  if(extraction_rate < 0 | extraction_rate > 1){
    stop("extraction_rate should be between zero and one")
  }
  
  #---------------------set up month 1------------------
  number_new_investors <- number_investors[1]
  
  status <- tibble(id = 1:number_new_investors,
                   invested = rlognormal(number_new_investors, mu, cv),
                   value_tmp = invested,
                   value = invested,
                   withdrawn = 0,
                   time_period = 1,
                   extracted = 0)
  
  cash_on_hand <- with(status, sum(invested) - sum(withdrawn) - sum(extracted))
  
  #---------------------months 2 and onwards---------------------
  while(cash_on_hand > 0 & max(status$id) < ceiling){
    
    # we make a new state for our existing investors, which is going to later be
    # appended to the state so far:
    update <-  status |>
      # limit to the state from the last time_period:
      filter(time_period == max(time_period)) |>
      # Double the 'value' of existing investments
      mutate(value_tmp = value_tmp * time_multiplier) |>
      # Decide for each investor what they are going to do this time period:
      mutate(action = sample(c("withdraw_all", "withdraw_small", "rollover", "invest"),
                             size = n(),
                             replace = TRUE,
                             prob = c(withdraw_all_rate, withdraw_small_rate, 
                                      roll_over_rate, invest_more_rate))) |>
      # An incremental amount which might be used for furhter investments or for withdrawals:
      mutate(incr_tmp = rlognormal(n(), mu, cv)) |>
      # some people try to withdraw their paper value. This increases the amount
      # they have ever withdrawn:
      mutate(withdrawn = case_when(
        action == "withdraw_all" ~ withdrawn + value_tmp,
        action == "withdraw_small" ~ withdrawn + incr_tmp,
        TRUE ~ withdrawn
      )) |>
      # ... and decreases their paper value by the same amount
      mutate(value = case_when(
        action == "withdraw_all" ~ 0,
        action == "withdraw_small" ~ value_tmp - incr_tmp,
        action == "rollover" ~ value_tmp,
        action == "invest" ~ value_tmp + incr_tmp
      )) |>
      # Some other people choose to invest more:
      mutate(invested = case_when(
        action == "invest" ~ invested + incr_tmp,
        TRUE ~ invested
      ))  |>
      # the people running the Ponzi scheme extract a percentage of the
      # real money available, calculated per investor (so fully withdrawn
      # investors not included):
      mutate(extract_tmp = pmax(0, (invested - withdrawn - extracted) * extraction_rate),
             extracted = extracted + extract_tmp) |>
      select(-action, -incr_tmp, -extract_tmp) |>
      # time period goes up one:
      mutate(time_period = max(status$time_period + 1))
    
    # We are also going to get a number of new investors:
    number_new_investors <- number_investors[unique(update$time_period)]
    if(number_new_investors > 0){
    
      new_investors <- tibble(id = 1:number_new_investors + max(status$id),
                              invested = rlognormal(number_new_investors, mu, cv),
                              value_tmp = invested,
                              value = invested,
                              withdrawn = 0,
                              time_period = unique(update$time_period),
                              extracted = 0)
    } else {
      new_investors <- tibble()
    }  
    
    # depending on whether the user wants all the history, we either append our
    # new update for the previous investors and our new investors to the old state,
    # or we just keep the latest update on previous investors plus our new investors:
    if(keep_history){
      status <- rbind(status, update, new_investors)
    } else {
      status <- rbind(update, new_investors)
    }
    
    # We need to determine if the scheme has gone bust, by calculating its
    # actual cash available (the amount invested so far, minus the total
    # withdrawn and total extracted)
    cash_on_hand <- status |>
      filter(time_period == max(time_period)) |>
      summarise(x = sum(invested) - sum(withdrawn) - sum(extracted)) |>
      pull(x)
    
  }  
  
  return(status)
}

#-----------------some examples-----------------


ponzi_plot <- function(status, return_numbers = FALSE){

  numbers <- status  |>
    dplyr::filter(time_period == max(time_period)) |>
    dplyr::summarise(total_invested = sum(invested),
              total_withdrawn = sum(withdrawn),
              total_extracted = sum(extracted),
              paper_value = sum(value),
              months = unique(time_period),
              total_investors = max(id),
              leverage = round(paper_value / total_invested)) |>
    t()
  
  plot <- status  |>
    dplyr::group_by(time_period) |>
    dplyr::summarise(`Paper value` = sum(value),
              `Real value` = pmax(0, sum(invested) - sum(withdrawn)),
              `Extracted by scammers` = sum(extracted)) |>
    tidyr::gather(variable, value, -time_period) |>
    ggplot2::ggplot(aes(x = time_period, y = value, colour = variable)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10(label = dollar) +
    labs(colour = "", y = "Value", x = "Number of months")
  
  if(return_numbers){
    return(numbers)
  } else {
    return(plot)
  }
}

set.seed(123)
p1 <- ponzi(invest_more_rate = 0.2,
            withdraw_all_rate = 0.01,
            cv = 2, keep_history = TRUE) |>
  ponzi_plot(return_numbers = FALSE)

# what if the number of investors grew by 50% each month,
# 50% of current investor chose to invest more ach round,
# and only 1/1000 people decided to withdraw everying
p2 <- ponzi(number_investors = round(10 * 1.5 ^ (1:100)),
            invest_more_rate = 0.5, 
            withdraw_all_rate = 0.001,
            cv = 2,
            keep_history = TRUE) |>
  ponzi_plot()

svg_png(p1, "../img/0283-ponzi1")
svg_png(p2, "../img/0283-ponzi2")

#--------------systematic exploration-----------

set.seed(123)
params <- expand_grid(
  investor_growth = c(1, 1.2, 1.4, 1.6, 1.8),
  cv = c(0, 0.5, 1),
  invest_more_rate = c(0, 0.1, 0.2),
  withdraw_all_rate = c(0.001, 0.01, 0.05),
  extraction_rate = c(0.001, 0.01, 0.05)
) |>
  # give me three of each
  slice(1:n(), 1:n(), 1:n()) |>
  # bit of randomness in one of the most important parameters
  mutate(investor_growth = investor_growth + runif(n(), -0.1, 0.1)) |>
  mutate(i = 1:n()) 

# set up parallel processing cluster
cluster <- makeCluster(7) 
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(foreach)
  library(tidyverse)
})

clusterExport(cluster, c("params", "ponzi"))

results <- foreach(i = 1:nrow(params), .combine = rbind) %dopar% {
  set.seed(i)
  
  ni <- round(10 * params[i, ]$investor_growth ^ (1:2000))
  
  this_sim <- ponzi(number_investors = ni,
                  invest_more_rate = params[i, ]$invest_more_rate, 
                  withdraw_all_rate = params[i, ]$withdraw_all_rate,
                  extraction_rate = params[i, ]$extraction_rate,
                  cv = params[i, ]$cv,
                  ceiling = 1e7,
                  keep_history = FALSE) |>
    filter(time_period == max(time_period))|>
    summarise(total_invested = sum(invested),
              total_withdrawn = sum(withdrawn),
              total_extracted = sum(extracted),
              paper_value = sum(value),
              months = unique(time_period),
              total_investors = max(id),
              leverage = round(paper_value / total_invested),
              i = i)
  
  return(this_sim)
  
}

# note the peak at 20 months that's when you hit the ceiling of 10million
# people

# lm(log(total_extracted) ~ investor_growth + cv + withdraw_all_rate, data = results)


p3 <- results |>
  left_join(params, by = "i") |>
  mutate(extraction_rate = glue("Extrct: {extraction_rate}"),
         withdraw_all_rate = glue("Exit: {withdraw_all_rate}")) |>
  ggplot(aes(x = investor_growth, y = months, colour = as.ordered(cv))) +
  facet_grid(extraction_rate~withdraw_all_rate) +
  geom_jitter()  +
  labs(colour = "Variation in investor amounts",
       y = "Length of scam (in months)",
       x = "Monthly growth in number of 'investors'\n(2 = doubling per month; capped at 10 million)",
       subtitle = "A low 'exit rate' (by investors/victims) is key to prolonging a Ponzi scheme",
       title = "How long can a Ponzi scheme last?")



p4 <- results |>
  left_join(params, by = "i") |>
  mutate(extraction_rate = glue("'Tax' scammed:\n{extraction_rate}"),
         withdraw_all_rate = glue("Exit: {withdraw_all_rate}")) |>
  ggplot(aes(x = investor_growth, y = total_extracted, colour = as.ordered(cv))) +
  facet_grid(extraction_rate~withdraw_all_rate) +
  geom_jitter() +
  scale_y_log10(label = dollar) +
  labs(colour = "Variation in investor amounts",
       y = "Total extracted by scammers",
       x = "Monthly growth in number of 'investors'\n(2 = doubling per month; capped at 10 million)",
       subtitle = "Key success factor for getting rich from a Ponzi scheme is to quickly and consistently grow your 'investors' and extract as much 'tax' you can.",
       title = "How much can the scammers extract from a Ponzi scheme?")

svg_png(p3, "../img/0283-months",w = 10, h = 8)
svg_png(p4, "../img/0283-extract",w = 10, h = 8)
