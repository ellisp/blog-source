library(tidyverse)
library(janitor)
library(scales)
library(Cairo)
library(mgcv)
library(EpiEstim)
library(patchwork)
library(frs)
library(tabulizer)
library(lubridate)
library(ggrepel)
CairoWin()



#=================Reprdocuing the NIH paper on malaria=============
# Nice article by https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4809590/ looking at relationship
# between test positivity rate and case rates

# Taken from Table 1. The paper seems to be missing the six-monthly Confirmed Malaria Case Rate,
# only shows the average over the 24 month period
malaria_aux <- tribble(~village, ~population, ~cmcr,
                       "Bugoye", 1549, 135.7,
                       "Bunyangoni", 1644, 56.4,
                       "Ihani", 714, 37,
                       "Izinga", 833, 273.7,
                       "Kanyanamigho", 1808, 102.2,
                       "Katumba", 1110, 11.7,
                       "Kibirizi", 1113, 6.9,
                       "Kihindi", 936, 4.1, 
                       "Kikokera", 588, 59.6,
                       "Kisamba", 3280, 1.3,
                       "Maghoma", 2688, 3.2,
                       "Muhambo", 1272, 12.1,
                       "Muramba", 3576, 65.9,
                       "Ndughutu", 2538, 48.4,
                       "Rwaking", 1968, 67.3
)


download_if_fresh("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4809590/bin/pone.0152410.s001.pdf",
                  destfile = "pone.015410.s001.pdf")

tables <- extract_tables("pone.015410.s001.pdf")

malaria_data_orig <- lapply(1:length(tables), function(i){
      as.data.frame(tables[[i]]) %>%
        mutate(page_number = i) 
    }) %>%
  bind_rows() %>%
  as_tibble()  %>%
  # remove the header row, only on the first page
  slice(-1)

names(malaria_data_orig) <- c(tables[[1]][1,], "page_number")

# A problem is that when a table has no rdt results but only blood_smear (like page 8, 17:21, 70:72, 108:118),
# then the pdf interpreter has no way of realising there is an empty column

bad_pages <- c(8, 17:21, 70:72, 108:118)

malaria_data <- malaria_data_orig %>%
  clean_names() %>%
  # fix the problem noted above of pages with no RDT values having blood_smear values go into them
  mutate(blood_smear = ifelse(page_number %in% bad_pages, rdt, blood_smear),
         rdt = ifelse(page_number %in% bad_pages, NA, rdt)) %>%
  # some observations are misread and everythingth rdt column is displaced into blood_smear
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         # rainy season not defined in the paper so I used 
         # https://weather-and-climate.com/average-monthly-Rainfall-Temperature-Sunshine-in-Uganda
         rainy_season = as.numeric(month(date) %in% c(3:5, 10:12)),
         sex = case_when(
           sex == "F" ~ "Female",
           sex == "M" ~ "Male"
         ),
         age = as.numeric(age),
         village = str_to_title(village),
         # One village had its name abbreviated:
         village = ifelse(village == "Kanyan", "Kanyanamigho", village),
         # numberic versions of the Rapid Diagnostic Test and Blood Smear test, with NA when no results
         rdt_n = case_when(
           rdt == "Pos" ~ 1,
           rdt == "Neg" ~ 0
           ),
         blood_smear_n = case_when(
           blood_smear == "Pos" ~ 1,
           blood_smear == "Neg" ~ 0
           ),
         either_n = case_when(
           # sequencing matters here:
           rdt_n == 1 | blood_smear_n == 1 ~ 1,
           rdt_n == 0 | blood_smear_n == 0 ~ 0
         )
  ) 

filter(malaria_data_orig, Village == "KIHINDI") %>% View()

# Some checks that awkward villages (with I & II or A and B variants) came through ok
stopifnot(
  malaria_data %>%
    filter(grepl("Muramba", village)) %>%
    distinct(village) %>%
    nrow() == 1
)

stopifnot(
  malaria_data %>%
    filter(grepl("Rwaking", village)) %>%
    distinct(village) %>%
    nrow() == 1
)

# check all the villages in the "auxiliary" info are in the actual data
stopifnot(
  malaria_data %>%
    distinct(village) %>%
    inner_join(malaria_aux, by = "village") %>%
    nrow() ==
    nrow(malaria_aux)
)

# Recreate Table 1. There are a few small discrepencies eg I have 1510 tested in Bugoye, original
# paper had only 1,499, and some of the RDTs I have one less than they report. 'Rainy Season' is
# wrong too. But generally is pretty close (this table was how I found the 'bad_pages' problem
# adjusted for above)
malaria_data %>%
  filter(!is.na(either_n)) %>%
  inner_join(malaria_aux, by = "village") %>%
  group_by(village) %>%
  summarise(Population = unique(population),
            Tested = n(),
            `Age < 5` = percent(mean(age < 5, na.rm = TRUE)),
            `Rainy Season` = percent(mean(rainy_season)),
            TPR = percent(mean(either_n)),
            `Total RDTs` = sum(!is.na(rdt_n)),
            `RDT PR (%)` = percent(mean(rdt_n, na.rm = TRUE)),
            `Total Slides` = sum(!is.na(blood_smear_n)),
            `SPR (%)` = percent(mean(blood_smear_n, na.rm = TRUE)),
            CMCR = unique(cmcr))

# Can reproduce the regressions and Figure 1 because the six-monthly CMCR values are missing

village_data <- malaria_data %>%
  filter(!is.na(either_n)) %>%
  group_by(village) %>%
  summarise(tpr = mean(either_n)) %>%
  inner_join(malaria_aux, by = "village") %>%
  arrange(tpr)

mod <- lm(log(cmcr) ~ log(tpr), data = village_data)
mod_data <- tibble(tpr = seq(from = 0.01, to = max(village_data$tpr) + 0.01, length.out = 1000))
pred <- predict(mod, newdata = mod_data, se.fit = TRUE)
mod_data <- mod_data %>%
  mutate(fitted = pred$fit,
         lower_ci = fitted - pred$se.fit * 1.96,
         upper_ci = fitted + pred$se.fit * 1.96,
         lower_pi = fitted - sqrt(pred$se.fit ^ 2 + pred$residual.scale ^ 2) * 1.96,
         upper_pi = fitted + sqrt(pred$se.fit ^ 2 + pred$residual.scale ^ 2) * 1.96) %>%
  mutate_all(exp) %>%
  mutate(tpr = log(tpr))

mp <- ggplot(mod_data, aes(x = tpr)) +
  geom_point(data = village_data, aes(y = cmcr)) +
  geom_ribbon(aes(ymin = lower_pi, ymax = upper_pi), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "grey50", alpha = 0.4) +
  geom_line(aes(y = fitted)) +
  geom_text_repel(data = filter(village_data, village %in% c("Bugoye", "Ihani")),
                  aes(y = cmcr, label = village)) +
  coord_cartesian(ylim = c(0, 300)) +
  scale_x_continuous(label = percent) +
  labs(x = "Test-Positivity Rate",
       y = "Confirmed Malaria Case Rate\nFrom health system case records (per thousand)",
       title = "Non-linear relationship of test positivity and malaria incidence in 15 Ugandan villages",
       subtitle = "Shaded area shows 95% confidence interval and 95% prediction interval.
Labelled villages are outliers due to data issues discussed in the original article.",
       caption = "Source: Boyce et al, 'Practical Implications of the Non-Linar Relationship between the Test Positivity Rate and Malaria Incidence'
Original article at https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4809590/; graphic by http://freerangestats.info")
  

svg_png(mp, "../img/0178-malaria", w = 8, h = 6)

#==================Covid Data prep================================

#------------------Import data------------------

states_orig <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") 
states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv")

states <- states_orig %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  clean_names() %>%
  # force total number of tests to be at least as many as the number of positives:
  mutate(total_test_results_increase = pmax(positive_increase, total_test_results_increase)) %>%
  mutate(pos_rate = positive_increase / total_test_results_increase) %>%
  arrange(date) %>%
  mutate(date_n = as.numeric(date))  %>%
  left_join(select(states_info, state, state_name = name), by = "state")

# Just the 12 biggest states
states12 <- states %>%
  group_by(state) %>%
  summarise(max_pos = max(positive)) %>%
  arrange(desc(max_pos)) %>%
  slice(1:12) %>%
  inner_join(states, by = "state") %>%
  # state has to be a factor for use in mgcv::gam:
  mutate(state_name = fct_reorder(state_name, positive, .fun = sum)) %>%
  # we want deaths in 7-14 days time as a crude indicator of cases now, for use later
  # Tried various methods and 7 was best. Obviously, if doing this 'for real', 7 should
  # be a parameter we estimate from the data
  group_by(state) %>%
  arrange(date) %>%
  mutate(deaths_x_days_later = lead(death_increase, 7)) %>%
  ungroup()

#-----------------Smooth the positive test rates-----------
mod <- gam(pos_rate ~ state_name + s(date_n, by = state_name), 
           data = states12, 
           family = quasibinomial,
           weights = total_test_results_increase)

states12$pos_rate_smoothed <- predict(mod, newdata = states12, type = "response")

p1 <- states12 %>%
  ggplot(aes(x = date, y = pos_rate)) +
  facet_wrap(~state_name) +
  geom_line(aes(y = pos_rate_smoothed)) +
  geom_point(aes(size = total_test_results_increase), alpha = 0.1) +
  scale_size_area(label = comma, max_size = 12) +
  labs(size = "Number of daily tests", 
       x = "",
       y = "",
       title = "Test-positivity rates for COVID-19 in 12 US states",
       caption = "Source: covidtracking.com, smoothing by freerangestats.info") +
  scale_y_continuous(label = percent, limits = c(0, 1))

svg_png(p1, "../img/0178-smoothed-test-rates", 11, 7)


#==========================Exploring my model for upscaling numbers when test positivity is high================

increase_cases <- function(observed_cases, pos_rate, m, k){
  y <- observed_cases * pos_rate ^ k * m
  return(y)
}

the_data <- states12 %>%
  filter(state_name == "New York") %>%
  mutate(`Simple multiplier\n(confirmed x 6)` = increase_cases(positive_increase, pos_rate, m = 6.35, k = 0),
         `Deaths 7 days later x 100` = deaths_x_days_later * 100,
         `Ratio multiplier` = increase_cases(positive_increase, pos_rate, m = 17.75, k = 1),
         `Generalized adjustment` = increase_cases(positive_increase, pos_rate, m = 10.83, k = 0.5))  %>%
  select(date, `Confirmed cases` = positive_increase, 
         `Simple multiplier\n(confirmed x 6)`:`Generalized adjustment`) %>%
  gather(variable, value, -date) %>%
  mutate(variable = fct_reorder(variable, -value, .fun = last),
         variable = fct_relevel(variable, "Confirmed cases", after = Inf))

p2 <- the_data %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  theme(legend.position = "right") +
  scale_y_continuous(label = comma ) +
  scale_colour_brewer(palette = "Set1") +
  labs(colour = "Adjustment method",
       title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
       subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The 'simple multiplier' method probably overestimates cases when testing is good, and underestimates it when testing is inadequate.",
       y = "Daily new cases",
       x = "",
       caption = "Source: Confirmed cases and testing data from covidtracking.com, analysis by freerangestats.info")

p2a <- p2 + geom_line()
p2b <- p2 + geom_smooth(method = "loess", se = FALSE, span = 0.5)
  

svg_png(p2a, "../img/0178-diff-methods", 10, 6)
svg_png(p2b, "../img/0178-diff-methods-smoothed", 10, 6)

p3 <- the_data %>%
  group_by(variable) %>%
  summarise(total_cases = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(y = variable, x = total_cases, colour = variable)) +
  scale_colour_brewer(palette = "Set1") +
  geom_point() +
  geom_segment(xend = 0, aes(yend = variable)) +
  scale_x_continuous(label = comma) +
  labs(title = "Different methods of adjusting the raw observed daily COVID-19 case count in New York",
       subtitle = "Comparing a simple 'times 10' multiplier with methods that adjust for the ratio of positive test rates.
The four adjustment methods have been calibrated to deliver similar total results for illustrative purposes.",
       y = "Adjustment method",
       x = "Total cases to 1 May 2020") +
  theme(legend.position = "none")
p3
svg_png(p3, "../img/0178-total-cases", 10, 6)

#================Estimating Reff================
# This section copied from https://github.com/CBDRH/ozcoviz/blob/master/nsw_eff_R_data_and_plot_prep.R
# by Tim Churches and Nick Tierney
#
# Get the data on serial interval distribution from Nishiura et al:
download.file("https://raw.githubusercontent.com/CBDRH/ozcoviz/master/get_nishiura_si_sample.R",
              destfile = "get_nishiura_si_sample.R")
source("get_nishiura_si_sample.R")
nishi_si_sample  <- get_nishiura_si_sample()

# configs for eff R estimation
# SI distribution from Nishiura et al.
parametric_si_nishiura_config <- make_config(list(mean_si = 4.7,
                                                  std_si = 2.9))

# values from https://www.nejm.org/doi/full/10.1056/NEJMoa2001316
parametric_si_li_config <- make_config(list(mean_si = 7.5,
                                            std_si = 3.4))

# posterior sample based on Nishiura et al SI data
si_from_sample_nishiura_config <-  make_config(list(n1=500, n2 = 50, seed=2))




# estimate eff R

all_variables <- unique(the_data$variable)
plots <- list()

for(i in 1:length(all_variables)){
  incid <- filter(the_data, variable == all_variables[i]) %>% 
    rename(I = value,
           dates = date) %>%
    select(-variable) %>%
    drop_na() %>%
    arrange(dates)
  
  effr <- estimate_R(incid,
                        method="si_from_sample",
                        si_sample=nishi_si_sample,
                        config = si_from_sample_nishiura_config)
  
  
  p <-  plot(effr, what = "R", legend = FALSE, options_R = list(col = "steelblue")) 
  p$labels$title <- all_variables[i]
  p$labels$x <- ""
  p$labels$y <- "Estimated R for COVID-19 in New York"
  plots[[i]] <- p
  
}

annotation <- "All methods shown here have an unrealistic spike in effective reproduction number
in mid March as testing started to reveal very high numbers of unrecorded cases prevalent in the population. The first day with significant number of tests was 13 March
(2,900, compared to a previous high of 44). Estimates of R, based on a sliding 7 day window, cannot
be taken regarded as useful until 20 March onwards. Estimates based on 'deaths 7 days later' are problematic 
for other reasons." %>%
  str_wrap(., 60)

annotation_plot <- ggplot() +
  annotate("text", x = -1, y = 0, label = annotation, hjust = 0, size = 4) +
  xlim(-1, 1) +
  theme_void() +
  labs(title = str_wrap("These charts show attempts to adjust the estimation of reproduction 
                        number by scaling up case numbers with a high test positivity.", 70))

draw_plots <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + annotation_plot

frs::svg_png(draw_plots, "../img/0178-r-methods", w= 15, h = 8)











