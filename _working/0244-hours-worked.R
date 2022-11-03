
library(tidyverse)
library(scales)
library(readabs)

hours_ft_sa <- read_abs(series_id = "A84426278A")
empl_ft_sa <- read_abs(series_id = "A84423041X")

empl_ft_sa |>
  select(date, employed_ft_sa = value) |>
  inner_join(select(hours_ft_sa, date, value)) |>
  rename(hours_ft_sa = value) |>
  mutate(hours_per_ft_empl = hours_ft_sa / employed_ft_sa) |>
  ggplot(aes(x = date, y = hours_per_ft_empl)) +
  geom_line() +
  labs(x ="", 
       y = "Hours",
       subtitle = "Monthly hours of full time workers divided by number of full time workers",
       title = "'Quiet quitting?'",
       caption = "Source: ABS Labour Force Survey A84426278A and A84423041X.
       Both series seasonally adjusted in original.")
