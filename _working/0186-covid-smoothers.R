library(tidyverse)
library(scales)
library(ggseas)

#-------Our world in data version - country level data only----------
download.file("https://covid.ourworldindata.org/data/ecdc/full_data.csv",
              destfile = "covid_full_data.csv")

owid <- read_csv("covid_full_data.csv")

eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia",
                  "Cyprus", "Czech Republic", "Denmark",
                  "Estonia", "Finland", "France", "Germany",
                  "Greece", "Hungary", "Ireland", "Italy",
                  "Latvia", "Lithuania", "Luxembourg", "Malta",
                  "Netherlands", "Poland", "Portugal", "Romania",
                  "Slovakia", "Slovenia", "Spain", "Sweden") 

stopifnot(sum(!eu_countries %in% owid$location) == 0)

d <- owid %>%
  mutate(broad_location = case_when(
    location %in% eu_countries ~ "European Union",
    location == "United States" ~ "United States"
  )) %>%
  filter(!is.na(broad_location)) %>%
  group_by(date, broad_location) %>%
  summarise_if(is.numeric, sum) %>%
  arrange(date)

#----------------------different ways of summarising / smoothing-----------------

# Rolling averages:
p1 <- d %>%
  ggplot(aes(x = date, y = new_cases, colour = broad_location)) +
  geom_point(alpha = 0.2) +
  stat_rollapplyr(width = 3) +
  stat_rollapplyr(width = 7, linetype = 2) +
  labs(x = "", colour = "", y = "New cases per day",
       title = "COVID-19 cases per day for two country groupings",
       subtitle = "Showing original data (points), 3-day rolling average (line), 7-day rolling average (dashed line)") +
  scale_y_continuous(label = comma)

# Seasonally adjusted (for 7 day seasonality):
p2 <- d %>%
  ggplot(aes(x = date, y = new_cases, colour = broad_location)) +
  geom_point(alpha = 0.2) +
  stat_stl(frequency = 7, s.window = 7) +
  labs(x = "", colour = "", y = "New cases per day",
       title = "COVID-19 cases per day for two country groupings",
       subtitle = "Showing original data (points), seasonally adjusted with STL (line)") +
  scale_y_continuous(label = comma)


# Trend after seasonal adjustement (for 7 day seasonality)
p3 <- d %>%
  ggplot(aes(x = date, y = new_cases, colour = broad_location)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 1/7, se = FALSE) +
  stat_rollapplyr(width = 7, linetype = 2) +
  labs(x = "", colour = "", y = "New cases per day",
       title = "COVID-19 cases per day for two country groupings",
       subtitle = "Showing original data (points), LOESS smoother (line), 7-day rolling average (dotted line)") +
  scale_y_continuous(label = comma)

p3
