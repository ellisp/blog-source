library(tidyverse)
library(googlesheets4)
library(janitor)
library(scales)
library(mgcv)
library(EpiNow2) # remotes::install_github("epiforecasts/EpiNow2")
library(frs)     # remotes::install_github("ellisp/frs-r-package/pkg")
library(patchwork)
library(glue)
library(surveillance) # for backprojNP()

# https://twitter.com/DiseaseEcology/status/1274956876640169988
#-----------------the Victoria data--------------

url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

gd_orig <- read_sheet(url) 

d <- gd_orig %>%
  clean_names() %>% 
  filter(state == "VIC") %>%
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE),
            cumulative_case_count = max(cumulative_case_count, na.rm = TRUE)) %>%
  mutate(tests_conducted_total  = ifelse(tests_conducted_total < 0, NA, tests_conducted_total),
         cumulative_case_count = ifelse(cumulative_case_count < 0, NA, cumulative_case_count)) %>%
  ungroup() %>%
  # filter(!is.na(tests_conducted_total)) %>% 
  # correct one typo, missing a zero
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10"), 1068000, tests_conducted_total)) %>%
  # remove two bad dates 
  filter(!date %in% as.Date(c("2020-06-06", "2020-06-07"))) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         confirm = c(cumulative_case_count[1], diff(cumulative_case_count)),
         pos_raw = pmin(1, confirm / test_increase)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(confirm = 0)) %>%
  mutate(numeric_date = as.numeric(date),
         positivity = pos_raw) %>%
  filter(date > as.Date("2020-02-01")) %>%
  fill(positivity, .direction = "downup") %>%
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasipoisson")),
         ps2 = fitted(loess(positivity ~ numeric_date, data = ., span = 0.1)),
         cases_corrected = confirm * ps1 ^ 0.1 / min(ps1 ^ 0.1)) %>%
  ungroup() %>%
  mutate(smoothed_confirm = fitted(loess(confirm ~ numeric_date, data = ., span = 0.1))) 


the_caption <- "Data gathered by The Guardian; analysis by http://freerangestats.info"  

p_pos <- d %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = pos_raw)) +
  geom_line(aes(y = ps2)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(x = "", y = "Test positivity",
       title = "Positive test rates for COVID-19 in Melbourne, Victoria",
       caption = the_caption)
svg_png(p_pos, "../img/0189-positivity")

# I don't believe the sqrt "corrected" cases helped here so have a much more modest 0.1
p_adj <- d %>%
  select(date, cases_corrected, confirm) %>%
  gather(variable, value, -date) %>%
  mutate(variable = case_when(
    variable == "confirm" ~ "Recorded cases",
    variable == "cases_corrected" ~ "With small adjustment for test positivity"
  )) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_point()  +
  geom_smooth(se = FALSE, span = 0.07) +
  labs(x = "", y = "Number of new cases per day",
       colour = "",
       caption = the_caption,
       title = "Covid-19 cases per day in Melbourne, Victoria",
       subtitle = "With and without a small adjustment for test positivity. No adjustment for delay.")

svg_png(p_adj, "../img/0189-adjusting-positivity")  

#------------------understanding convolution----------------------

x <- c(4,6,8,9,7,5)
pmf <- dpois(0:14, lambda = 3)
pmf <- pmf / sum(pmf)

# create a lagged version of x, with lags determined by a known probability mass function:
y <- blur(x, pmf, scale_pmf = TRUE)
# recover the original version of x, given its blurred version 
# and the original probabilities of delays of various lags:
recovered <- sharpen(y, pmf)


p_conv <- tibble(original_x = x, position = 1:length(x)) %>%
  right_join(recovered, by = "position") %>%
  gather(variable, value, -position) %>%
  mutate(variable = case_when(
    variable == "original_x" ~ "Original (unobserved) values",
    variable == "x" ~ "Original values recovered",
    variable == "y" ~ "Values after a delay"
   )) %>%
  filter(position > -1) %>%
  ggplot(aes(x = position, y = value, colour = variable)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "right") +
  labs(title = "Convolution and deconvolution demonstrated with simulated data",
       colour = "",
       x = "Time",
       y = "Value")

svg_png(p_conv, file = "../img/0189-conv-eg")


# Make a "surveillance time series" of our toy observations
y_sts <- sts(y)
# Back-propagate to get an estimate of the original, assuming our observations
# are a Poisson process
x2 <- backprojNP(y_sts, pmf)

plotit <- function(){
  par(family = "Roboto")
  plot(x2, xaxis.labelFormat = NULL, 
       legend=NULL, lwd=c(1,1,3), lty=c(1,1, 1), 
       col = c("grey80", "grey90", "red"),
       main="", bty = "l",
       ylim = c(0, 10),
       xlab = "Time",
       ylab = "Infections")
  title("Recovering / sharpening unobserved infections with simulated toy data and \nsurveillance::backprojNP()", 
        adj = 0, family = "Sarala", font.main = 1)
  points(1:6, x, col = "orange", cex = 4, pch = "-")
  
  legend(15, 10, legend = c("Observed", "Back-propagated", "Original"),
         pch = c(15, NA, NA),
         lty = c(0, 1, 1),
         lwd = c(0, 3, 3),
         col = c("grey80", "red", "orange"),
         pt.cex = c(2, 2, 2),
         cex = 0.8,
         bty = "n")
}

svg_png(plotit, "../img/0189-toy-backprop")

# note the actual estimates are in x2@upperbound, which adds up to 39, same as the original y


#------------------Back propagation with real data-------------------

pmf_covid <- c(0, dpois(0:20, lambda = 9))

# takes a few minutes
bp_covid <- backprojNP(sts(d$confirm), pmf_covid)

sharpened <- tibble(recovered_x = bp_covid@upperbound) %>%
  mutate(position = 1:n())

p_adj_conv <- d %>%
  mutate(position = 1:n()) %>%
  left_join(sharpened, by = "position") %>%
  select(date, recovered_x, confirm)  %>%
  mutate(recovered_x = replace_na(recovered_x, 0)) %>%
  gather(variable, value, -date) %>%
  mutate(variable = case_when(
    variable == "confirm" ~ "Confirmed cases",
    variable == "recovered_x" ~ "Estimated original cases accounting for delay"
  )) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line(size = 1.5) +
  labs(title = "Back-projection of Victorian Covid-19 infections",
       subtitle = str_wrap("Non-parametric back-projection of incidence cases assuming 
       average delay of 10 days between infection and observation, using methods in 
                           Becker et al (1991). No correction for right truncation of data,
                           so the last 15 days will be badly biased downwards.", 100),
       x = "",
       y = "Number of infections",
       colour = "")
svg_png(p_adj_conv, "../img/0189-delay-adj")


#------------Estimating R with EpiNow2---------------------
reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)


#---------Based on original-------------

estimates <- EpiNow2::epinow(reported_cases = d, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay),
                             horizon = 7, samples = 3000, warmup = 600, 
                             cores = 4, chains = 4, verbose = TRUE, 
                             adapt_delta = 0.95)

my_plot_estimates <- function(estimates, extra_title = ""){
  my_theme <- my_theme +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) 
  
  p <- estimates$plots$summary
  
  p1 <- p$patches$plots[[1]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 1000)) +
    labs(title = glue("Estimated infections based on confirmed cases{extra_title}"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p2 <- p$patches$plots[[2]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 1000)) +
    labs(title = glue("Estimated infections taking delay{extra_title} into account"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p3 <- p$data %>% 
    filter(date > as.Date("2020-04-20")) %>%
    ggplot(aes(x = date, y = median, fill = type)) +
    my_theme +
    geom_hline(yintercept = 1, colour = "steelblue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_ribbon(aes(ymin = bottom, ymax = top), alpha = 0.1) +
    geom_line(aes(colour = type)) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    labs(title = glue("Effective Reproduction Number, correcting for both delay and right truncation{extra_title}"),
         y = bquote("Estimated" ~ R[t]),
         x = "")  +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  pc <- p1 + p2 + p3 + plot_layout(ncol = 1)

  return(pc)
}

pc1 <- my_plot_estimates(estimates)  
svg_png(pc1, "../img/0189-combined-1", h = 10, w = 10)


#---------Based on positivity-adjusted-------------

d2 <- select(d, date, cases_corrected) %>%
  mutate(confirm = round(cases_corrected) )

estimates2 <- EpiNow2::epinow(reported_cases = d2, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay),
                             horizon = 7, samples = 3000, warmup = 600, 
                             cores = 4, chains = 4, verbose = TRUE, 
                             adapt_delta = 0.95)


pc2 <- my_plot_estimates(estimates2, extra_title = " and positivity")
svg_png(pc2, "../img/0189-combined-2", h = 10, w = 10)

