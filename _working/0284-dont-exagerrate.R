
library(tidyverse)
library(tidycensus)


#-------------Do women do 66% of the work?---------------

# https://www.bls.gov/charts/american-time-use/activity-by-sex.htm
avg_per_day <- tibble(
  activity = c("Personal care, including sleep",
              "Eating and drinking",
              "Household activities",
              "Purchasing goods and services",
              "Caring for and helping household members",
              "Caring for and helping nonhousehold members",
              "Working and work-related activities",
              "Educational activities",
              "Organizational, civic and religious activities"),
  men = c(9.59, 1.22, 1.49, 0.55, 0.38, 0.15, 4.17, 0.36, 0.2),
  women = c(10.07, 1.18, 2.32, 0.76, 0.62, 0.19, 2.98, 0.42, 0.28)) |>
  mutate(is_work = c(0,0,1,1,1,1,1,0,0)) |>
  mutate(women_perc = women / (women + men))

sum(avg_per_day$men)
sum(avg_per_day$women)

avg_per_day  |>
  filter(is_work == 1)  |>
  summarise(sum(women) / sum(women + men))

# So women do about 50.5% of the work in USA, if it includes household 
# activities, shoping, caring and work activities.


#-------------Do women have 11% of the income?----------------


v <- load_variables(year = 2020, dataset = "acs5")
View(v)

v |>
  filter(grepl("income", concept, ignore.case = TRUE)) |>
  filter(grepl("mean", label, ignore.case = TRUE)) |>
  View()

v |>
  filter(grepl("income", concept, ignore.case = TRUE)) |>
  filter(grepl("individual", concept, ignore.case = TRUE)) |>
  View()


# B06010_001 to B06010_011 will get us individual income in bins, could
# do some inference with that. Need a US Census Bureau API key.