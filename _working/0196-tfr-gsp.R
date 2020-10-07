library(tidyverse)
library(readabs)
library(scales)
library(lubridate)
library(ggrepel)

# national accounts by state, from 
# https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-state-accounts/2018-19
na <- read_abs("5220.0")

gsp <- na %>%
  filter(grepl("Gross state product per capita: Chain volume measures ;", series)) %>% 
  mutate(state = gsub("Gross state product per capita: Chain volume measures ;", "", series)) %>%
  mutate(state = str_squish(gsub(";", "", state))) %>%
  select(date, state, value) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(gsp_growth = value / lag(value) - 1) %>%
  mutate(yr = year(date)) %>%
  drop_na()

gsp %>%
  ggplot(aes(x = date, y = gsp_growth)) +
  geom_hline(yintercept = 0 , colour = "steelblue") +
  facet_wrap(~state) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1))


fert <- read_csv("../data/FERTILITY_AGE_STATE_06102020224336072.csv", col_types = cols()) %>%
  mutate(yr_before = Time -1) %>%
  select(yr_before, tfr_year_later = Value, state = Region, Time) %>%
  arrange(Time) %>%
  mutate(tfr_diff = tfr_year_later - lag(tfr_year_later))

fert  %>%
  ggplot(aes(x = Time, y = tfr_year_later)) +
  geom_line() +
  facet_wrap(~state)

fert  %>%
  ggplot(aes(x = Time, y = tfr_diff)) +
  geom_line() +
  facet_wrap(~state)

d0 <- gsp %>%
  inner_join(fert, by = c("yr" = "yr_before", "state"))
  
ps <- 3
p2 <- d0 %>%
  ggplot(aes(x = gsp_growth, y = tfr_year_later)) +
#  geom_path() +
  geom_smooth(method = "lm") +
  scale_colour_viridis_c(option = "D", direction = -1) +
  geom_point(aes(colour = yr), size = ps) +
  geom_point(shape = 1, colour = "black", size = ps) +
  geom_text_repel(aes(label = ifelse(yr %in% range(d0$yr), yr, ""))) +
  theme(legend.position = c(0.85, 0.15),
        panel.spacing = unit(2, "lines")) +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  labs(x = "Annual growth in Gross State Product, chain volume measure",
       y = "Total Fertility Rate one year later",
       colour = "",
       title = "No obvious link between economic activity and fertility",
       subtitle = "If there is a relationship between growth in Gross State Product and fertility, it's a subtle one.") +
  facet_wrap(~state, scales = "fixed")
p2
# we should correct the residuals for auto correlation, but it's already not significant
# even without doing that
mod1 <- lm(tfr_year_later ~ gsp_growth + state, data = d0)
anova(mod1)

mod1 <- lm(tfr_diff ~ gsp_growth + state, data = d0)
anova(mod1)
