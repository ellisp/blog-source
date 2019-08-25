library(tidyverse)
library(multidplyr)
library(frs)
library(fitdistrplus)
library(knitr)
library(readxl)
library(kableExtra)
library(clipr)
#-----------------simulated data-------------

set.seed(123)
simulated_rate <- 0.005
volumes <- tibble(volume = rexp(n = 10000, rate = simulated_rate))

volumes <- volumes %>% 
  mutate(bin = case_when(volume < 10 ~ "<10",
                         volume < 50 ~ "10-50",
                         volume < 100 ~ "50-100",
                         volume < 1000 ~ "100 - 1000",
                         TRUE ~ ">= 1000"),
         left = case_when(volume < 10 ~ 0,
                          volume < 50 ~ 10,
                          volume < 100 ~ 50,
                          volume < 1000 ~ 100,
                          TRUE ~ 1000),
         right = case_when(volume < 10 ~ 10,
                           volume < 50 ~ 50,
                           volume < 100 ~ 100,
                           volume < 1000 ~ 1000,
                           TRUE ~ NA_real_),
         bin = factor(bin, levels = c("<10", "10-50", "50-100", "100 - 1000", ">= 1000"), ordered = T))

# This is how it would look to the original user
volumes %>% 
  group_by(bin) %>%
  summarise(freq = n()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped") %>%
  write_clip()

volumes_bin <- dplyr::select(volumes, left, right) %>% 
  as.data.frame()

fitted_distribution_gamma <- fitdistcens(volumes_bin, "gamma")

# overall fit:
summary(fitted_distribution_gamma)

# estimated mean
fitted_distribution_gamma$estimate["shape"] / fitted_distribution_gamma$estimate["rate"]


p <- ggplot(volumes) +
  geom_density(aes(x = volume)) +
  stat_function(fun = dgamma, args = fitted_distribution_gamma$estimate, colour = "blue") +
  annotate("text", x = 700, y = 0.0027, label = "Blue line shows modelled distribution; black is density of the actual data.")
frs::svg_png(p, "../img/0157-densities")

# bin_based_mean
volumes %>%
  mutate(mid = (left + replace_na(right, 2000)) / 2) %>%
  summarise(crude_mean = mean(mid)) %>%
  pull(crude_mean)




#------real data - business counts------------

download.file("https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&816502.xls&8165.0&Data%20Cubes&B164DBE8275CCE58CA2583A700121372&0&June%202014%20to%20June%202018&21.02.2019&Latest",
              destfile = "bus_counts.xls", mode = "wb")
bus_counts_orig <- readxl::read_excel("bus_counts.xls" , 
                                      sheet = "June 2018", 
                                      range = "A7:G4976",
                                      col_types = c("text", "text", "text", rep("numeric", 4)))

# see https://en.wikipedia.org/wiki/List_of_companies_of_Australia, largest firm in Australia
# has 220,000 employees (Wesfarmers)
names(bus_counts_orig) <- c("state", "code", "industry", "0-0", "1-19", "20-199", "200-250000")

bus_counts <- bus_counts_orig %>%
  mutate(code = str_pad(code, 4, "left", "0")) %>%
  filter(!grepl("^Total", industry) & code != "9999") %>%
  filter((`0-0` + `1-19` + `20-199` + `200-250000`) > 0) %>%
  gather(employees, freq, -state, -code, -industry) %>%
  separate(employees, by = "-", into = c("left", "right"), remove = FALSE) %>%
  mutate(left = as.numeric(left),
         right = as.numeric(right),
         employees = fct_reorder(employees, -left)) %>%
  left_join(dplyr::select(anzsic_4_abs, anzsic_class_code, anzsic_group_code, anzsic_group, division), 
            by = c("code" = "anzsic_class_code")) %>% 
  group_by(anzsic_group_code, anzsic_group, state, employees, left, right, division) %>%
  summarise(freq = sum(freq)) %>%
  arrange(anzsic_group_code) %>%
  ungroup()
  
# how data will look:
kable(head(bus_counts)) %>%
  kable_styling(bootstrap_options = "striped") %>%
  write_clip()

# demo plot:
p <- bus_counts %>%
  mutate(division2 = fct_lump(division, 10, w = freq),
         division2 = fct_reorder(division2, freq, .fun = sum),
         state2 = fct_lump(state, 5, w = freq),
         division2 = fct_relevel(division2, "Other", after = 0)) %>%
  ggplot(aes(x = division2, fill = employees, weight = freq)) +
  geom_bar() +
  facet_wrap(~state2) +
  coord_flip() +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  labs(y = "Number of firms",
       x = "",
       fill = "Number of employees",
       caption = "Source: ABS Count of Australian Businesses, analysis by freerangestats.info",
       title = "Number of firms by industry division and number of employees") +
  theme(panel.grid.minor = element_blank(),
        panel.spacing = unit(5, "mm"),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Spectral", guide = guide_legend(reverse = TRUE))

frs::svg_png(p, "../img/0157-businesses", w = 10, h = 7)

bus_counts_small <- bus_counts %>%
  filter(division == "Manufacturing" & state %in% c("New South Wales", "Victoria", "Queensland"))


#' @param d a data frame or tibble with columns including left, right and freq
#' @param keepdata passed through to fitdistcens
#' @param ... extra variables to pass to fitdistcens eg starting values for the estimation process
avg_finder <- function(d, keepdata = FALSE, ...){
  d_bin <- as.data.frame(d[rep(1:nrow(d), d$freq), c("left", "right")])
  fit <- fitdistrplus::fitdistcens(d_bin, "nbinom", keepdata = keepdata, ...)
  return(fit$estimate[["mu"]])
}

# Meat manufacturing in NSW:
avg_finder(bus_counts_small[1:4, ])

# Meat manufacturing in Queensland:
avg_finder(bus_counts_small[5:8, ])

cluster <- new_cluster(7)
cluster_library(cluster, "fitdistrplus")
cluster_assign(cluster, avg_finder = avg_finder)

            
# calculate the simple ones using single processing dplyr for ease during dev:
bus_summary_simp <- bus_counts_small %>%
  mutate(middle = (left + right) / 2,
         just_above_left = (right - left) / 10 + left,
         tiny_above_left = left * 1.1) %>%
  group_by(anzsic_group, state) %>%
  summarise(number_businesses = sum(freq), 
            crude_avg = sum(freq * middle) / sum(freq),
            less_crude_avg = sum(freq * just_above_left) / sum(freq),
            another_avg = sum(freq * tiny_above_left) / sum(freq))

# parallel processing for the negatib binomial verions
bus_summary_nb <- bus_counts_small %>%
  group_by(anzsic_group, state) %>%
  partition(cluster = cluster) %>%
  do(nb_avg = avg_finder(.)) %>%
  collect() %>%
  mutate(nb_avg = unlist(nb_avg))

bus_summary <- bus_summary_simp %>%
  left_join(bus_summary_nb, by = c("anzsic_group", "state"))

p1 <- bus_summary %>%
  ggplot(aes(x = crude_avg, y = nb_avg, colour = anzsic_group)) +
  facet_wrap(~state) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  theme(legend.position = "none") +
  labs(x = "Average firm size calculated based on mid point of each bin\nDiagonal line shows where points would be if both methods agreed.",
       y = "Average firm size calculated with negative binomial model",
       title = "Mean number of employees in manufacturing firms estimated from binned data",
       subtitle = "Inference based on mid point of bin delivers massive over-estimates")


p2 <- bus_summary %>%
  ggplot(aes(x = less_crude_avg, y = nb_avg, colour = anzsic_group)) +
  facet_wrap(~state) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  theme(legend.position = "none") +
  labs(x = "Average firm size calculated based on 10% of way from left side of bin to right side\nDiagonal line shows where points would be if both methods agreed.",
       y = "Average firm size calculated with negative binomial model",
       title = "Mean number of employees in manufacturing firms estimated from binned data",
       subtitle = "Using a point 10% of the distance from the left to the right still over-estimates very materially")

p3 <- bus_summary %>%
  ggplot(aes(x = another_avg, y = nb_avg, colour = anzsic_group)) +
  facet_wrap(~state) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  theme(legend.position = "none") +
  labs(x = "Average firm size calculated based on left side of bin x 1.1\nDiagonal line shows where points would be if both methods agreed.",
       y = "Average firm size calculated with negative binomial model",
       title = "Mean number of employees in manufacturing firms estimated from binned data",
       subtitle = "Using the (left-most point of the bin times 1.1) results in under-estimates")

svg_png(p1, "../img/0157-res1", w = 12, h = 5)
svg_png(p2, "../img/0157-res2", w = 12, h = 5)
svg_png(p3, "../img/0157-res3", w = 12, h = 5)



#========Full data========
# Doesn't work for some state-industry combinations
system.time({
bus_summary_full <- bus_counts %>%
  group_by(anzsic_group, state) %>%
  partition(cluster = cluster) %>%
  do(nb_avg = avg_finder(.)) %>%
  collect() %>%
  mutate(nb_avg = unlist(nb_avg))
})
