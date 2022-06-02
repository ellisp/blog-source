library(tidyverse)
library(readabs)
library(scales)
library(patchwork)
library(ggrepel)
library(lubridate)
library(Cairo)

CairoWin()

empl <- read_abs_series(c("A84631158X",
                            "A84630870K",
                            "A84423050A"))

d <- casual |>
  select(date, series, value) |>
  spread(series, value) |>
  rename(unemployment = 'Unemployment rate ;  Persons ;',
         total = 'Employee ;  Employed total ;',
         casual = 'Employee without paid leave entitlements ;  Employed total ;') |>
  #filter(!is.na(casual)) |>
  mutate(casual_perc = casual / total,
         unemp_perc = unemployment / 100)


# Matt's original
d |>
  ggplot(aes(x = date, y = casual_perc)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(y = "Percentage of employees without paid leave entitlements",
       x= "")

# Connected scatter plot
d |>
  ggplot(aes(x = unemp_perc, y = casual_perc)) +
  geom_point() +
  geom_path() +
  geom_text(data = filter(d, date %in% range(date) | 
                            unemp_perc %in% range(unemp_perc) |
                            casual_perc %in% range(casual_perc)),
                  aes(label = format(date, "%b %Y"))) +
  theme_minimal() +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(x = "Unemployment rate, seasonally adjusted",
       y = "Percentage of employees without paid leave entitlements",
       title = "Unemployment and casual workforce as a proportion of workforce, Australia",
       caption = "Source: ABS Labour Force Survey")
