library(tidyverse)
library(scales)

#' Net present value
#' 
#' @param discount Discount rate, as a percentage (eg "7" means 7%)
#' @param cost Vector of annual costs
#' @param benefit Vector of annual benefits
#' @param return_abs Whether or not to return the absolute value of the net present value (only likely to be
#' used if you are using this function as part of an internal rate of return calculation)
#' @details for a given stream of costs, benefits, number of years and discount rate, return the net
#' present value (discounted benefits minus discounted costs)
#' @examples
#' net_present_value(discount = 7,
#'                   cost = c(1000000, rep(100, 29)),
#'                   benefit = rep(50000, 30))
net_present_value <- function(discount, cost, benefit, return_abs = FALSE){
  if (length(discount) != 1 | class(discount) != "numeric"){
    stop("discount should be a single number")
  }
  
  if(length(benefit) != length(cost)){
    stop("cost and benefit should be equally-lengthed numeric vectors")
  }
  
  # convert discount percentage into a multiplier
  discount_m <- 1 - discount / 100
  
  # number of years, other than year zero, which get discounted
  m <- length(benefit) - 1
  
  npv <- sum(benefit  * discount_m ^ (0:m)) - sum(cost  * discount_m ^ (0:m))
  if(return_abs){
    npv <- abs(npv)
  }
  return(npv)
}


costs <- c(1000000, rep(100, 29))
benefits <- rep(70000, 30)
net_present_value(discount = 7, cost = costs, benefit  = benefits)
net_present_value(discount = 4, cost = costs, benefit  = benefits)

#----------with uncertain assumptions----------
nsim <- 10000
set.seed(123)
sims <- data.frame(
  benefit_rand = rgamma(nsim, 7, 0.0001),
  cost_rand = rgamma(nsim, 10, 0.1),
  npv = NA
)
for(i in 1:nsim){
  sims[i, "npv"] <- net_present_value(discount = 4, 
                                      cost = c(1000000, rep(sims[i, "cost_rand"], 29)),
                                      benefit = rep(sims[i, "benefit_rand"], 30))
}

p0 <- sims %>%
  ggplot(aes(x = npv)) +
  geom_density(fill = "steelblue", alpha = 0.1) +
  scale_x_continuous(label = dollar) +
  labs(x = "Net present value",
       title = "Impact of randomness on net present value",
       subtitle = "Investment with initial cost of $1m, mean annual costs of around $100, and mean annual benefits of around $70,000\nAnnual costs and benefits are Gamma-distributed random variables")


quantile(sims$npv, c(0.1, 0.9))

svg_png(p0, "../img/0161-varying-npvs")
#---------Relationship to IRR------------
discounts <- seq(from = 0, to = 10, length.out = 100)
npvs <- sapply(discounts, net_present_value, cost = costs, benefit = benefits)

p1 <- tibble(npv = npvs,
       discount = discounts) %>%
  ggplot(aes(x = discount, y = npvs)) +
  annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymax = 0, ymin = -Inf, fill = "red", alpha = 0.2) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  labs(x = "Discount rate",
       y = "Net Present Value",
       title = "Impact of discount rate on net present value",
       subtitle = "Investment with initial cost of $1m, annual costs of $100, and annual benefits of $70,000")

frs::svg_png(p1, "../img/0161-discount-rates")

internal_rate_return <- function(cost, benefit, interval = c(-25, 25)){
  opt <- optimise(net_present_value,
                 interval = interval, 
                 cost = cost,
                 benefit = benefit,
                 return_abs = TRUE)
  return(opt$minimum)
}

internal_rate_return(costs, benefits)

#-------------IRR with random costs and benefits--------

for(i in 1:nsim){
  sims[i, "irr"] <- internal_rate_return(cost = c(1000000, rep(sims[i, "cost_rand"], 29)),
                                         benefit = rep(sims[i, "benefit_rand"], 30))
}

p2 <- sims %>%
  ggplot(aes(x = irr / 100)) +
  geom_density(fill = "steelblue", alpha = 0.1) +
  scale_x_continuous(label = percent) +
  labs(x = "Internal rate of return",
       title = "Impact of randomness on internal rate of return",
       subtitle = "Investment with initial cost of $1m, mean annual costs of around $100, and mean annual benefits of around $70,000\nAnnual costs and benefits are Gamma-distributed random variables")

svg_png(p2, "../img/0161-varying-irr")
quantile(sims$irr, c(0.1, 0.9))


#'  Cost benefit analysis of a single state of the world
cba_single <- function(
  initial_cost,
  initial_cost_se = initial_cost * generic_uncertainty,
  ongoing_cost,
  ongoing_cost_se = ongoing_cost * generic_uncertainty,
  customer_level_shift,
  customer_level_shift_se = customer_level_shift * generic_uncertainty,
  customer_growth,
  customer_growth_se = customer_growth * generic_uncertainty,
  customer_spend,
  customer_spend_se = customer_spend * generic_uncertainty,
  end_year = 30,
  discount = 7,
  generic_uncertainty = 0.1
) {
  
  # randomness
  initial_cost_this <- rnorm(1, initial_cost, initial_cost_se)
  ongoing_cost_this <- rnorm(end_year, ongoing_cost, ongoing_cost_se)
  customer_level_shift_this = rnorm(1, customer_level_shift, customer_level_shift_se)
  customer_growth_this = rnorm(1, customer_growth, customer_growth_se)
  customer_spend_this = rnorm(1, customer_spend, customer_spend_se)
                         
  costs_current <- c(initial_cost_this, ongoing_cost_this)
  
  customers <- customer_level_shift_this * (1 + customer_growth_this) ^ (0:end_year)
  benefits_current <- customers * customer_spend_this
  
  npv <- net_present_value(discount, cost = costs_current, benefit = benefits_current)
  irr <- internal_rate_return(cost = costs_current, benefit = benefits_current)
  
  dm <- 1 - discount / 100
  costs_discounted <- costs_current * dm ^ (0:end_year)
  benefits_discounted <- benefits_current * dm ^ (0:end_year)
  
  time_series_output <- tibble(
    'Costs in current prices' = costs_current,
    'Benefits in current prices' = benefits_current,
    'Discounted costs' = costs_discounted,
    'Discounted benefits' = benefits_discounted,
    'Cumulative discounted costs' = cumsum(costs_discounted),
    'Cumulative discounted benefits' = cumsum(benefits_discounted),
    year = 0:end_year
  )
  
  return(list(
    'Net Present Value' = npv,
    'Internal Rate of Return' = irr,
    time_series_output = time_series_output
  ))
}

# Example
set.seed(321)
cba_single(
  initial_cost = 1000000,
  ongoing_cost = 10000,
  customer_level_shift = 1000,
  customer_growth = 0.02,
  customer_spend = 100
)

#' Cost benefit analysis of many simulations
cba_multi <- function(nsim = 1000, seed = 123, ...){
  output_simple <- tibble('Net Present Value' = numeric(nsim), 
                          'Internal Rate of Return' = numeric(nsim))
  output_complex_l <- list()
  set.seed(seed)
  
  for(i in 1:nsim){
    analysis <- cba_single(...)  
    output_simple[i, ] <- c(analysis$'Net Present Value', analysis$'Internal Rate of Return')
    tmp <- analysis$time_series_output
    tmp$id <- i
    output_complex_l[[i]] <- tmp
  }
  
  output_complex <- do.call(rbind, output_complex_l) %>%
    gather(variable, value, -id, -year) %>%
    group_by(variable, year)  %>%
    summarise(mid = mean(value),
              lower = quantile(value, 0.1),
              upper = quantile(value, 0.9))
  
  return(list(
    output_simple = output_simple,
    output_complex = output_complex)
  )
}

results <- cba_multi(
              initial_cost = 1000000,
              ongoing_cost = 10000,
              customer_level_shift = 1000,
              customer_growth = 0.01,
              customer_spend = 100
            )


results$output_simple %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~variable, scales = "free") +
  geom_density(fill = "steelblue", alpha = 0.1)

results$output_complex %>%
  ggplot(aes(x = year)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = mid)) +
  scale_y_continuous(label = dollar) +
  labs(x = "Year",
       y = "Value")
