library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 7)

#=========2004 election to 2007 election==============
data(AustralianElectionPolling)
data(AustralianElections)

days_between_elections <- as.integer(diff(as.Date(c("2004-10-09", "2007-11-24")))) + 1

#' Function to plot time series extracted from a stan fit of latent state space model of 2007 Australian election
plot_results <- function(stan_m){
   if(class(stan_m) != "stanfit"){
      stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")
   }
   ex <- as.data.frame(rstan::extract(stan_m, "mu"))
   names(ex) <- 1:d1$n_days
   
   p <- ex %>%
      gather(day, value) %>%
      mutate(day = as.numeric(day),
             day = as.Date(day, origin = "2004-10-08")) %>%
      group_by(day) %>%
      summarise(mean = mean(value),
                upper = quantile(value, 0.975),
                lower = quantile(value, 0.025)) %>%
      ggplot(aes(x = day)) +
      labs(x = "Shaded region shows a pointwise 95% credible interval.", 
           y = "Voting intention for the ALP (%)",
           caption = "Source: Jackman's pscl R package; analysis at https://ellisp.github.io") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(aes(y = mean)) +
      scale_y_continuous(breaks = 31:54, sec.axis = dup_axis(name = "")) +
      theme(panel.grid.minor = element_blank())
   
   return(p)
}


#----------------no polls inbetween the elections------------
d1 <- list(mu_start = 37.64, mu_finish = 43.38, n_days = days_between_elections)

# returns some warnings first time it compiles; see
# http://mc-stan.org/misc/warnings.html suggests most compiler
# warnings can be just ignored.
system.time({
  stan_mod1 <- stan(file = 'oz-polls-1.stan', data = d1,
  control = list(max_treedepth = 20))
  }) # 1800 seconds ie 30 minutes

svg("../img/0102-no-polls.svg", 8, 6)
plot_results(stan_mod1) +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with no use of polling data")
dev.off()
  
#--------------------AC Nielson-------------------
ac <- AustralianElectionPolling %>%
  filter(org == "Nielsen") %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-08")),  # ie number of days since first election; last election (9 October 2004) is day 1
         p = ALP / 100,
         se_alp = sqrt(p * (1- p) / sampleSize) * 100)

d2 <- list(
  mu_start = 37.64,
  mu_finish = 43.38,
  n_days = days_between_elections,
  y_values = ac$ALP,
  y_days = ac$MidDateNum,
  y_n = nrow(ac),
  y_se = ac$se_alp
)


system.time({
  stan_mod2 <- stan(file = 'oz-polls-2.stan', data = d2,
                   control = list(max_treedepth = 20))
}) # 512 seconds

svg("../img/0102-one-poll.svg", 8, 6)
plot_results(stan_mod2) +
   geom_point(data = ac, aes(x = MidDate, y = ALP)) +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with use of just one firm's polling data (Nielsen)")
dev.off()


#-------------------all 5 polls--------------------
all_polls <- AustralianElectionPolling %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-08")),  # ie number of days since first election
         p = ALP / 100,
         se_alp = sqrt(p * (1- p) / sampleSize) * 100,
         org = fct_reorder(org, ALP))


poll_orgs <- as.character(unique(all_polls$org))

p1 <- filter(all_polls, org == poll_orgs[[1]])
p2 <- filter(all_polls, org == poll_orgs[[2]])
p3 <- filter(all_polls, org == poll_orgs[[3]])
p4 <- filter(all_polls, org == poll_orgs[[4]])
p5 <- filter(all_polls, org == poll_orgs[[5]])


d3 <- list(
  mu_start = 37.64,
  mu_finish = 43.38,
  n_days = days_between_elections,
  y1_values = p1$ALP,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_alp,
  y2_values = p2$ALP,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_alp,
  y3_values = p3$ALP,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_alp,
  y4_values = p4$ALP,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_alp,
  y5_values = p5$ALP,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se_alp
)


system.time({
  stan_mod3 <- stan(file = 'oz-polls-3.stan', data = d3,
                    control = list(max_treedepth = 15,
                                   adapt_delta = 0.8),
                    iter = 4000)
}) # about 600 seconds

svg("../img/0102-all-polls.svg", 8, 6)
plot_results(stan_mod3) +
   geom_point(data = all_polls, aes(x = MidDate, y = ALP, colour = org), size = 2) +
   geom_line(aes(y = mean)) +
   labs(colour = "") +
   ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
           "Latent variable estimated with use of all major firms' polling data")
dev.off()
  
png("../img/0102-house-effects.png", 8 * 600, 8 * 600, res = 600)
par(family = "myfont")
pairs(stan_mod3, pars = c("d", "sigma"))
dev.off()

house_effects <- summary(stan_mod3, pars = "d")$summary %>%
  as.data.frame() %>%
  round(2) %>%
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

svg("../img/0102-compare-house-effects.svg", 8, 5)
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

summary(stan_mod3, pars = "sigma")$summary

convert_pngs("0102")
