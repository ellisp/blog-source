library(tidyverse)
library(scales)
library(foreign) # for importing Stata files
library(actuar)  # for access to the Burr distribution
library(acid)
library(forcats)

# Data described at https://www.gc.cuny.edu/CUNY_GC/media/LISCenter/brankoData/LaknerMilanovic2013WorldPanelIncomeDistributionLMWPIDDescription.pdf
# The database has been created specifically for the
# paper “Global Income Distribution: From the Fall of the Berlin Wall to the Great Recession”,
# World Bank Policy Research Working Paper No. 6719, December 2013, published also in World
# Bank Economic Review (electronically available from 12 August 2015). 
download.file("https://wfs.gc.cuny.edu/njohnson/www/BrankoData/LM_WPID_web.dta", 
              mode = "wb", destfile = "LM_WPID_web.dta")

wpid <- read.dta("LM_WPID_web.dta")

# inc_con whether survey is income or consumption
# group income decline group 1 to 10
# RRinc is average per capita income of the decile in 2005 PPP

# the first 10 rows are Angola in 1995, so let's experiment with them
angola <- wpid[1:10, c("RRinc", "group")]

svg("../img/0107-angola-quantiles.svg", 7, 5)
ggplot(angola, aes(x = group, y = RRinc)) +
  geom_line() +
  geom_point() +
  ggtitle("Mean consumption by decile in Angola in 1995") +
  scale_y_continuous("Annual consumption for each decile group", label = dollar) +
  scale_x_continuous("Decile group", breaks = 1:10) +
  labs(caption = "Source: Lakner/Milanovic World Panel Income Distribution data") +
  theme(panel.grid.minor = element_blank())
dev.off()

  
svg("../img/0107-angola-lorenz.svg", 7, 5)
angola %>%
  arrange(group) %>%
  mutate(cum_inc_prop = cumsum(RRinc) / sum(RRinc),
         pop_prop = group / max(group)) %>%
  ggplot(aes(x = pop_prop, y = cum_inc_prop)) +
  geom_line() +
  geom_ribbon(aes(ymax = pop_prop, ymin = cum_inc_prop), fill = "steelblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  labs(x = "Cumulative proportion of population",
       y = "Cumulative proportion of consumption",
       caption = "Source: Lakner/Milanovic World Panel Income Distribution data") +
  ggtitle("Inequality in Angola in 1995",
          "Lorenz curve based on binned decile mean consumption")
dev.off()

weighted.gini(angola$RRinc)

#========equating means of 10 sorted bins to quantiles===========
population <- rburr(10000, 1, 3, 3)

svg("../img/0107-burr.svg", 7, 4)
par(family = "myfont", bty = "l", font.main = 1)
plot(density(population), main = "Burr(1,3,3) distribution")
dev.off()



# purpose of this section is to explore what the relationship is between mean
# income of the 10 decile groups to the quantiles of an underlying distribution.
# Naively, might think the means are just the 0.05, 0.15, 0.25 ... 0.95 quantiles.
# And that must be not too far out.  But not quite right either...

#' Quantile averages
#' 
#' Mean value in binned groups
#' 
#' @param y numeric vector to provide summary statistics on
#' @param len number of bins to calculate means for
#' @details this is different from producing the actual quantiles; it is the mean value of y within each bin.
bin_avs <- function(y, len = 10){
  # argument checks:
  if(class(y) != "numeric"){stop("y should be numeric") }
  if(length(y) < len){stop("y should be longer than len")}
  
  # calculation:
  y <- sort(y)
  bins <- cut(y, breaks = quantile(y, probs = seq(0, 1, length.out = len + 1)))
  tmp <- data.frame(bin_number = 1:len,
                    bin_breaks = levels(bins),
                    mean = as.numeric(tapply(y, bins, mean)))
  return(tmp)
}

ginis <- numeric(99)
for(i in 1:99){
  ginis[i]   <- weighted.gini(bin_avs(population, len = i + 1)$mean)$Gini
}
ginis_df <- data.frame(
  number_bins = 2:100,
  gini = ginis
)

svg("../img/0107-ginis-from-deciles.svg", 9, 6)
ginis_df %>%
  mutate(label = ifelse(number_bins < 11 | round(number_bins / 10) == number_bins / 10, number_bins, "")) %>%
  ggplot(aes(x = number_bins, y = gini)) +
  geom_line(colour = "steelblue") +
  geom_text(aes(label = label)) +
  labs(x = "Number of bins",
       y = "Gini coefficient estimated from means within bins") +
  ggtitle("Estimating Gini coefficient from binned mean values of a Burr distribution population",
          paste0("Correct Gini is ", round(weighted.gini(population)$Gini, 3), ". Around 25 bins needed for a really good estimate."))
dev.off()

reps <- 1000
results <- matrix(0, reps, 10)
shape <- 0.5
rate <- 1
for(i in 1:reps){
  y <- rgamma(800, shape, rate)
  results[i, ] <- pgamma(bin_avs(y)$mean, shape, rate)
}
round(apply(results, 2, mean), 3)
# the more skewed the distribution (shape getting closer to zero and less than one) the more
# the averages in the t10 bins tend to be higher quantiles eg 0.08, 0.17, 0.26 rather than 0.05, 0.15, 0.25



# algorithm will be iterative
# 1. assume the 10 binned means represent the following quantiles: 0.05, 0.15, 0.25 ... 0.65, 0.75, 0.85, 0.95
# 2. pick the optimal distribution that fits those 10 quantile values.  Treat as a non-linear optimisation problem
# 3. generate data from that distribution and work out what the actual quantiles are
# 4. repeat 2
# 5. repeate 3.  If it's basically the same, stop here, otherwise iterate

n <- 10000
x <- angola$RRinc

fn1 <- function(params) {
  sum((x - qburr(p, params[1], params[2], params[3])) ^ 2 / x)
  }

# this can be very erratic:
p <- seq(0.05, 0.95, length.out = 10)
fit1 <- optim(c(1,1,1), fn1)
simulated1 <- rburr(n, fit1$par[1], fit1$par[2], fit1$par[3])
p <- pburr(bin_avs(simulated1)$mean, fit1$par[1], fit1$par[2], fit1$par[3])
fit1 <- optim(c(1,1,1), fn1)
simulated1 <- rburr(n, fit1$par[1], fit1$par[2], fit1$par[3])
cbind(bin_avs(simulated1), x)
weighted.gini(simulated1)$Gini
weighted.gini(x)$Gini

# this version with a log norm distribution is *much* more reliable
fn2 <- function(params) {
  sum((x - qlnorm(p, params[1], params[2])) ^ 2 / x)
}
p <- seq(0.05, 0.95, length.out = 10)
fit2 <- optim(c(1,1), fn2)
simulated2 <- rlnorm(n, fit2$par[1], fit2$par[2])
p <- plnorm(bin_avs(simulated2)$mean, fit2$par[1], fit2$par[2])
fit2 <- optim(c(1,1), fn2)
simulated2 <- rlnorm(n, fit2$par[1], fit2$par[2])

cbind(bin_avs(simulated2), x)
weighted.gini(simulated2)$Gini

# compare to the value estimated directly from the data:
weighted.gini(x)$Gini
# what if we do it all by monte carlo for binned means, no approximations?
# It's much slower and comes up with similar answers.
fn3 <- function(params){
  n <- 1000
  sim <- rlnorm(n, params[1], params[2])
  sum((x - bin_avs(sim)$mean) ^ 2 / x)
}
fit3 <- optim(c(1, 1), fn3, method = "SANN")
simulated3 <- rlnorm(n, fit3$par[1], fit3$par[2])
cbind(bin_avs(simulated3), x)
weighted.gini(simulated3)$Gini


#' Convert data that is means of deciles into a Gini coefficient
#' 
#' @param x vector of 10 numbers, representing mean income (or whatever) for 10 deciles
#' @param n number of simulated values of the underlying log-normal distribution to generate
#' @details returns an estimate of Gini coefficient that is less biased than calculating it
#' directly from the deciles, which would be slightly biased downwards.
deciles_to_gini <- function(x, n = 1000){
  fn <- function(params) {
    sum((x - qlnorm(p, params[1], params[2])) ^ 2 / x)
  }
  
  # starting estimate of p based on binned means and parameters
  p <- seq(0.05, 0.95, length.out = 10)
  fit <- optim(c(1,1), fn)
  
  # calculate a better value of p
  simulated <- rlnorm(n, fit$par[1], fit$par[2])
  p <- plnorm(bin_avs(simulated)$mean, fit$par[1], fit$par[2])
  
  # new fit with the better p
  fit <- optim(c(1,1), fn)
  simulated <- rlnorm(n, fit$par[1], fit$par[2])
  output <- list(par = fit$par)
  output$Gini <- as.numeric(weighted.gini(simulated)$Gini)
  return(output)
}

deciles_to_gini(x = wpid[61:70, ]$RRinc)
deciles_to_gini(x = wpid[171:180, ]$RRinc)

svg("../img/0107-all-countries-ginis.svg", 8, 6)
wpid %>%
  filter(country != "Switzerland") %>%
  mutate(inc_con = ifelse(inc_con == "C", "Consumption", "Income")) %>%
  group_by(region, country, contcod, year, inc_con) %>%
  summarise(Gini = deciles_to_gini(RRinc)$Gini) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = Gini, colour = contcod, linetype = inc_con)) +
  geom_point() +
  geom_line() +
  facet_wrap(~region) +
  guides(colour = FALSE) +
  ggtitle("Inequality over time",
          "Gini coefficients estimated from decile data") +
  labs(x = "", linetype = "",
       caption = "Source: Lakner/Milanovic World Panel Income Distribution data") 
dev.off()

svg("../img/0107-all-countries-snapshot.svg", 9, 9)
wpid %>%
  filter(country != "Switzerland") %>%
  mutate(inc_con = ifelse(inc_con == "C", "Consumption", "Income")) %>%
  group_by(region, country, contcod, year, inc_con) %>%
  summarise(Gini = deciles_to_gini(RRinc)$Gini) %>%
  ungroup() %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, Gini),
         region = fct_lump(region, 5)) %>%
  ggplot(aes(x = Gini, y = country, colour = inc_con, label = contcod)) +
  geom_text(size = 2) +
  facet_wrap(~region, scales = "free_y", nrow = 2) +
  labs(colour = "", y = "", x = "Gini coefficient",
       caption = "Source: Lakner-Milanovic World Panel Income Distribution") +
  ggtitle("Inequality by country",
          "Most recent year available up to 2008; Gini coefficients are estimated from decile mean income.")
dev.off()  

# cleanup
unlink("LM_WPID_web.dta")

convert_pngs("0107")
