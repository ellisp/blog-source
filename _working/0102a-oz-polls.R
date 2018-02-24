## Changes compared to previous version:
# vectorize mu
# vectorize the polls
# double the variance of the polls to account for total survey error - because worried about http://www.slate.com/articles/news_and_politics/politics/2016/08/don_t_be_fooled_by_clinton_trump_polling_bounces.html
# change the innovations to be based on student_t(4, mu, sigma) rather than normal(mu, sigma)

## Changes that were tried and either couldn't get to work or decided otherwise not to use:
# calculate the poll standard errors from mu, not the poll
# putting mu on a logit scale 
# re-parameterise so an innovation "epsilon" with mean zero is the parameter and mu is a transformed paraeter

library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)
library(boot) # for logit and inv.logit
rstan_options(auto_write = TRUE)
options(mc.cores = 7)

#=========2004 election to 2007 election==============
data(AustralianElectionPolling)
data(AustralianElections)

days_between_elections <- as.integer(diff(as.Date(c("2004-10-09", "2007-11-24")))) + 1

#' Function to plot time series extracted from a stan fit of latent state space model of 2007 Australian election
#' Assumes parameter mu is the state space intent to vote ALP, on logit scale
plot_results <- function(stan_m){
   if(class(stan_m) != "stanfit"){
      stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")
   }
   ex <- as.data.frame(rstan::extract(stan_m, "mu"))
   names(ex) <- 1:d3$n_days
   
   p <- ex %>%
      gather(day, value) %>%
      mutate(day = as.numeric(day),
             day = as.Date(day, origin = "2004-10-08"),
             value = value * 100) %>%
      group_by(day) %>%
      summarise(middle = mean(value),
                upper = quantile(value, 0.975),
                lower = quantile(value, 0.025)) %>%
      ggplot(aes(x = day)) +
      labs(x = "Shaded region shows a pointwise 95% credible interval.", 
           y = "Voting intention for the ALP (%)",
           caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(aes(y = middle)) +
      scale_y_continuous(breaks = 31:54, sec.axis = dup_axis(name = "")) +
      theme(panel.grid.minor = element_blank())
   
   return(p)
}


#----------------no polls inbetween the elections------------
d1 <- list(mu_start = logit(0.3764), mu_finish = logit(0.4338), n_days = days_between_elections)
# d1 <- list(mu_start = logit(0.3764), mu_finish = logit(0.4338), n_days = 100) # used during dev

system.time({
  stan_mod1 <- stan(file = 'oz-polls-1a.stan', data = d1,
  control = list(max_treedepth = 20))
  }) 

# Original version
# 1800 seconds ie 30 minutes for full dataset
# 15 seconds for 100 days, excluding compile

# Change to mu_finish ~ normal(mu[n_days], 0.01);
# No material change in time - 14 seconds for 100 days

# Change to mu[2:n_days] ~ normal(mu[1:(n_days - 1)], 0.25);
# No material change in time - still 14 seconds

# Change to vector[n_days] mu; rather than real mu[n_days]; and timing goes down to 
# 7 seconds for 100 days, 4 minutes for full dataset

# change parameterisation of mu to br logit, timing stays the same
# changed back again when decided didn't like it...

svg("../img/0102a-no-polls.svg", 8, 6)
plot_results(stan_mod1) +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with no use of polling data")
dev.off()


#-------------------all 5 polls--------------------
all_polls <- AustralianElectionPolling %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-08")),  # ie number of days since first election
         p = ALP / 100,
         org = fct_reorder(org, ALP),
         se = sqrt(p * (1 - p) / sampleSize))


poll_orgs <- as.character(unique(all_polls$org))

p1 <- filter(all_polls, org == poll_orgs[[1]])
p2 <- filter(all_polls, org == poll_orgs[[2]])
p3 <- filter(all_polls, org == poll_orgs[[3]])
p4 <- filter(all_polls, org == poll_orgs[[4]])
p5 <- filter(all_polls, org == poll_orgs[[5]])


d3 <- list(
  mu_start = 0.3764,
  mu_finish = 0.4338,
  n_days = days_between_elections,
  inflator =sqrt(2),
  y1_values = p1$p,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se,
  y2_values = p2$p,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se,
  y3_values = p3$p,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se,
  y4_values = p4$p,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se,
  y5_values = p5$p,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se
)


system.time({
  stan_mod3 <- stan(file = 'oz-polls-3a.stan', data = d3, chains = 4, 
                    control = list(max_treedepth = 15),
                    iter = 4000)
}) 


summary(stan_mod3, pars = "sigma")$summary 
summary(stan_mod3, pars = "d")$summary
summary(stan_mod3, pars = "epsilon")$summary

# comes down from about 600 seconds in the non-vectorised version to 200 seconds when vectorised and 
# calculating standard errors;
# goes back up to 400 seconds when the inflation added to poll standard errors
# and up to 600 seconds when the innovation distribution is changed to student_t(4, mu, sigma)

svg("../img/0102a-all-polls-inflator-2.svg", 8, 6)
plot_results(stan_mod3) +
   geom_point(data = all_polls, aes(x = MidDate, y = ALP, colour = org), size = 2) +
   geom_line(aes(y = middle)) +
   labs(colour = "") +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Daily innovations with a Student's t distribution with 4 degrees of freedom; 
total survey variance inflated 2x usual sampling error.")
dev.off()
  












#------------compare house effects--------------------

house_effects <- summary(stan_mod3, pars = "d")$summary %>%
  as.data.frame() %>%
  (function(x){x * 100}) %>%
  round(4) %>%
  mutate(org = poll_orgs,
         source = "Stan") %>%
  dplyr::select(org, mean, `2.5%`, `97.5%`, source)
house_effects
# small choices of prior make quite a big difference eg the prior 
# for variance of the innovations

jackman <- data_frame(
   org = c("Galaxy", "Morgan, F2F", "Newspoll", "Nielsen", "Morgan, Phone"),
   mean = c(-1.2, 2.7, 1.2, 0.9, 0.8),
   `2.5%` = c(-3.1, 1, -0.5, -0.8, -1),
   `97.5%` = c(0.6, 4.3, 2.8, 2.5, 2.3),
   source = "Jackman"
)

d <- rbind(house_effects, jackman) %>%
   mutate(org = fct_reorder(org, mean),
          ypos = as.numeric(org) + 0.1 - 0.2 * (source == "Stan")) 

# indicates the ALP overestimates a bit higher than Jackman's.  I thought it is using
# the logit scale that leads to that change, but taking it out doesn't change it.  So
# it is probably about inflating the sampling error
svg("../img/0102a-compare-house-effects.svg", 8, 5)
d %>%
   ggplot(aes(y = ypos, colour = source)) +
   geom_segment(aes(yend = ypos, x = `2.5%`, xend = `97.5%`)) +
   geom_point(aes(x = mean)) +
   scale_y_continuous(breaks = 1:5, labels = levels(d$org),
                      minor_breaks = NULL) +
   theme(panel.grid.major.y = element_blank(),
         legend.position = "right") +
   labs(x = "House effect for polling firms, 95% credibility intervals\n(percentage points over-estimate of ALP vote)",
        y = "",
        colour = "",
        caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io")  +
   ggtitle("Polling 'house effects' in the leadup to the 2007 Australian election",
           "Basically the same results in new analysis with Stan as in the original Jackman (2009)")
dev.off()



convert_pngs("0102a")
