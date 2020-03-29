#--------------- Setup--------------------
# Explore changing estimates of 
# Case Fatality Rate = deaths from disease / number of diagnosed cases of disease

devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
library(tidyverse)
library(scales)

the_caption = "Source: WHO and many others via Johns Hopkins University and Rami Krispin's coronavirus R package.\nAnalysis by http://freerangestats.info"

top_countries <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(Country.Region) %>%
  summarise(cases = sum(cases)) %>%
  top_n(8, wt = cases)

#---------------------------global total-------------------

first_non_china_d <- coronavirus %>%
  filter(Country.Region != "China" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

first_italy_d <- coronavirus %>%
  filter(Country.Region == "Italy" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)


d1 <- coronavirus %>%
  group_by(date, type) %>%
  summarise(cases = sum(cases)) %>%
  arrange(date) %>%
  spread(type, cases) %>%
  ungroup() %>%
  mutate(cfr_today = death / confirmed,
         cfr_cumulative = cumsum(death) / cumsum(confirmed))

d1b <- d1 %>%
  filter(date %in% c(first_italy_d, first_non_china_d))
ac <- "steelblue"

d1c <- d1 %>%
  mutate(cc = cumsum(confirmed)) %>% 
  summarise(`10000` = min(date[cc > 10000]),
         `100000` = min(date[cc > 100000])) %>%
  gather(variable, date) %>%
  left_join(d1, by = "date") %>%
  mutate(label = paste0(format(as.numeric(variable), big.mark = ",", scientific = FALSE), "\ncases"))

p1 <- d1 %>%
  ggplot(aes(x = date, y = cfr_cumulative)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  expand_limits(y = 0) +
  geom_point(data = d1b, colour = ac, shape = 1, size = 2) +
  annotate("text", x = first_italy_d, 
           y = filter(d1, date == first_italy_d)$cfr_cumulative - 0.001, 
           label = "First death in Italy",
           hjust = 0, size = 3, colour = ac) +
  annotate("text", x = first_non_china_d, 
           y = filter(d1, date == first_non_china_d)$cfr_cumulative + 0.001, 
           label = "First death outside China",
           hjust = 0, size = 3, colour = ac) +
  geom_text(data = d1c, aes(label = label), 
            size = 3, colour = "grey70", 
            hjust = 0.5, lineheight = 0.9, nudge_y = -0.002) +
  labs(caption = the_caption,
       x = "",
       y = "Observed case fatality rate",
       title = "Steadily increasing case fatality rate of COVID-19 in early 2020",
       subtitle = "Increase probably reflects move of the disease into older populations.
Note that actual case fatality is likely to be much lower due to undiagnosed surviving cases.")

svg_png(p1, "../img/0171-global")

#-----------------Country-specific totals------------------------

d2 <- coronavirus %>%
  group_by(date, Country.Region, type) %>%
  summarise(cases = sum(cases)) %>%
  group_by(date, Country.Region) %>%
  spread(type, cases) %>%
  arrange(date) %>%
  group_by(Country.Region) %>%
  mutate(cfr_cumulative = cumsum(death) / cumsum(confirmed)) %>%
  filter(!is.na(cfr_cumulative)) %>%
  ungroup() %>%
  inner_join(top_countries, by = "Country.Region") 


p2 <- d2 %>%
  ggplot(aes(x = date, y = cfr_cumulative, colour = Country.Region)) +
  geom_line() +
  geom_text(data = filter(d2, date == max(date)), aes(label = Country.Region), 
            hjust = 0, check_overlap = FALSE, size = 3) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.2)) +
  scale_colour_brewer(palette = "Set2") +
  expand_limits(x = max(d2$date) + 4) +
  labs(caption = the_caption,
       x = "",
       y = "Observed case fatality rate",
       title = "Country-specific case fatality rate of COVID-19 in early 2020",
       subtitle = "Eight countries with most diagnosed cases; Iran's early values truncated.\nA high level of uncertainty reflecting rapidly changing denominators as well as many unresolved cases.") +
theme(legend.position = "none")

svg_png(p2, "../img/0171-countries")

