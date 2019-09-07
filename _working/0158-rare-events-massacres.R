library(tidyverse)
library(NHPoisson)
library(scales)
library(knitr)
library(clipr)
library(kableExtra)

#----------------Data prep and first visualisation----------------

mass_shootings <- data.frame(
  mon = c(9, 1, 6, 8, 10, 12, 9, 8, 8, 10, 3, 1, 4),
  yr = c(1981, 1984, 1987, 1987, 1987, 1987, 1988, 1990, 1991, 1992, 1993, 1996, 1996)
) 

mass_shootings <- mass_shootings %>%
  mutate(months = 12 * (yr - 1979) + mon,
         approx_date = as.Date(paste(yr, mon, 15, sep = "-")),
         interval = c(NA, diff(approx_date)))

p1 <- ggplot(mass_shootings, aes(xend = approx_date, x = approx_date)) +
  geom_rect(xmin = as.Date("1996-07-15"), xmax = Inf, ymin = -Inf, ymax = Inf,
            fill = "steelblue", alpha = 0.01) +
  geom_rect(xmax = as.Date("1996-07-15"), xmin = -Inf, ymin = -Inf, ymax = Inf,
            fill = "red", alpha = 0.01) +
  geom_segment(y = -Inf, yend = Inf) +
  scale_x_date(limits = c(min(mass_shootings$approx_date - 50), as.Date("2018-02-15"))) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Approximate date of mass shooting",
       y = "") +
  annotate("text", x = as.Date("2008-01-01"), y = 1, label = "After the gun buy-back",
           colour = "steelblue") +
  ggtitle("Firearm-related homicides in Australia, to February 2018", 
          "Events in which at least 5 persons other than perpetrator died") 

frs::svg_png(p1, "../img/0158-events")


#-------------------------Modelling and likelihood ratio test-----------
# number of mass shootings in first period:
n_massacres <- nrow(mass_shootings)

# Expected mass shootings in each of the two periods, under constant rare events model:
lam0 <- (210 / 470) * n_massacres
lam1 <- (260 / 470) * n_massacres

# Put those two expected values into a table:
cbind(lam0, lam1) %>% kable() %>% kable_styling() %>% write_clip()

# Reduction in deviance comparing the two hypotheses
logLRobs <- 2 * (dpois(n_massacres, n_massacres, log = TRUE) -
              (dpois(n_massacres, lam0, log = TRUE) + dpois(0, lam1, log = TRUE)))

# Print teducation in deviance to screen:			  
logLRobs

# Calculate p-value if null hypothesis of a random drop in deviance:
1 - pchisq(logLRobs, df = 1)

#--------------------------------Robustness - would an extra, late massacre matter?---------------
# Robustness check - what if there was an extra massacre happening in the post-buyback period
mu0 <- ((n_massacres + 1) / (210 + 260)) * 210
mu1 <- ((n_massacres + 1) / (210 + 260)) * 260
cbind(mu0,mu1)

logLRperturb <- 2 * ((dpois(n_massacres, n_massacres, log = TRUE) + dpois(1, 1, log = TRUE))-
                       (dpois(n_massacres, mu0, log = TRUE) + dpois(1, mu1, log = TRUE)))
logLRperturb

1 - pchisq(logLRperturb, df = 1)


#------------Comparison with resampling----------------
# Force R to use non-uniform Rounding sampler, as per older versions of R, to get exact results
RNGkind(sample.kind="Rounding")
set.seed(20180226)

n_sim <- 20000000
logLRsim <- 0
for (i in 1:n_sim){
  x <- rpois(1, lam0)
  y <- rpois(1, lam1)
  lam0sim <- (x + y) / (210 + 260)
  logL0 <- dpois(x, lam0sim * 210, log = TRUE) + dpois(y, lam0sim * 260, log = TRUE)
  logL1 <- dpois(x, x, log = TRUE) + dpois(y, y, log = TRUE)
  logLRsim[i] <- 2 * (logL1 - logL0)
}

# Compare the drop of deviance actually observed with that simulated under the null hypothesis:
no_exceeding <- sum(logLRsim >= logLRobs)
no_exceeding
no_exceeding / n_sim

# same comparison, with the "one more recent massacre" perturbed data's drop in deviance:
no_exceeding_perturb <- sum(logLRsim >= logLRperturb)
no_exceeding_perturb / n_sim




#---------------Robustness - was there clumping to invalidate our assumptions---------------

p2 <- ggplot(mass_shootings, aes(x = jitter(interval, 2.5))) +
  geom_density(fill = "grey", alpha = 0.5, colour = NA) +
  geom_rug() +
  stat_function(fun = dexp, 
                args = list(rate = 1 / mean(mass_shootings$interval, na.rm = TRUE)), 
                colour = "blue",
                size = 2) +
  labs(x = "Interval between mass shootings in Australia 1981 to 1996",
       title = "Comparison of intervals between mass shooting with theoretical independence",
       subtitle = "There are fewer close-together shootings, and more far-apart, than expected from exponential distribution") +
  scale_x_continuous(label = comma)

frs::svg_png(p2, "../img/0158-exp-distribution")

#' Calculate the highest number of events in a given window
#' 
#' @param months a vector of times at which events took place
#' @param window length of window
#' @return The highest number of events occuring in the given window of time, in the given list of intervals of events
scan_stat <- function(months, window){
  sum_window <- 0
  for (j in 1:(210 - window + 1)){
    sum_window[j] = sum((j <= months) & (months < j + window))
  }
  max(sum_window)
}


#### Simulated data from a theoretical poisson process
n_sim <- 10000
max_window <- 18

# matrix to hold results for simulated data
max_stat_mat <- matrix(0, n_sim, max_window)

for (j in 1:n_sim){
  N <- rpois(1, n_massacres)
  months_sim <- sort(sample(1:210, size=N, repl=FALSE))
  for (i in 1:max_window){
    max_stat_mat[j,i] <- scan_stat(months_sim, i)
  }
}


#### Compare what actually happened to the simulated data
p_vals <- 0
stat_obs <- 0
window <- 1:max_window
for (k in window) {
  stat_obs[k] <- scan_stat(mass_shootings$months, k)
  p_vals[k] <- mean(max_stat_mat[, k] >= stat_obs[k])
}

# all unadjusted p values:
cbind(window, stat_obs, p_vals) %>% kable() %>% kable_styling() %>% write_clip()

# the lowest unadjusted p value:
unadj_pval <- min(p_vals)

# adjust that p value for all the data dredging we've done so far by comparing our result (lowest p value)
# to what we get by simulating everything from an actual Poisson process
M_sim <- 10000
pvals_sim <- 0
min_pval_sim <- 0
for (a in 1:M_sim){
  N <- rpois(1, n_massacres)
  months_sim <- sort(sample(1:210, size = N,repl = FALSE))
  stat_obs_sim <- 0
  for (b in window){
    stat_obs_sim[b] <- scan_stat(months_sim,b)
    pvals_sim[b] <- mean(max_stat_mat[,b] >= stat_obs_sim[b])
  }
  min_pval_sim[a] <- min(pvals_sim)
}

# On average, how often is the minimum p value we just simulated less than the unadjusted p value
# we got from the comparison of data to simulations?
adj_pval <- mean(min_pval_sim <= unadj_pval)
adj_pval

#-------------Addendum - including the Osmington solution---------
mass_shootings2 <- data.frame(
  mon = c(9, 1, 6, 8, 10, 12, 9, 8, 8, 10, 3, 1, 4,5),
  yr = c(1981, 1984, 1987, 1987, 1987, 1987, 1988, 1990, 1991, 1992, 1993, 1996, 1996, 2018)
) 

mass_shootings2 <- mass_shootings2 %>%
  mutate(months = 12 * (yr - 1979) + mon,
         approx_date = as.Date(paste(yr, mon, 15, sep = "-")),
         interval = c(NA, diff(approx_date)))

p3 <- ggplot(mass_shootings2, aes(xend = approx_date, x = approx_date)) +
  geom_rect(xmin = as.Date("1996-07-15"), xmax = Inf, ymin = -Inf, ymax = Inf,
            fill = "steelblue", alpha = 0.01) +
  geom_rect(xmax = as.Date("1996-07-15"), xmin = -Inf, ymin = -Inf, ymax = Inf,
            fill = "red", alpha = 0.01) +
  geom_segment(y = -Inf, yend = Inf) +
  scale_x_date(limits = c(min(mass_shootings$approx_date - 50), as.Date("2019-09-15"))) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Approximate date of mass shooting",
       y = "") +
  annotate("text", x = as.Date("2008-01-01"), y = 1, label = "After the gun buy-back",
           colour = "steelblue") +
  ggtitle("Firearm-related homicides in Australia, to September 2019", 
          "Events in which at least 5 persons other than perpetrator died") 

frs::svg_png(p3, "../img/0158-events-with-osmington")