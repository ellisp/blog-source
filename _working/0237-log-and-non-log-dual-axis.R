

library(tidyverse)
library(ggplot2)
library(glue)
library(frs)
library(patchwork)

d <- tibble(date = as.Date(paste0("2022-", c("02-01",
                                            "02-15",
                                            "03-01",
                                            "03-15",
                                            "04-01",
                                            "04-15",
                                            "05-01")))) |>
  mutate(ww = c(1.5 , 2, 100, 400, 500, 100, 105),
         cases = c(1, 600, 2500, 3500,3000, 2500, 2000),
         ww_log = log(ww)) 

with(d, dualplot(date, ww, cases))
with(d, dualplot(date, ww_log, cases))


p1 <- d |>
  gather(variable, value, -date) |>
  filter(variable != "ww") |>
  group_by(variable) |>
  mutate(value = value / value[4]) |>
  mutate(variable = case_when(
    variable == "cases" ~ "Cases",
    variable == "ww_log" ~ "Wastewater traces"
  )) |>
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line(size = 1.5) +
  labs(title = "Wastewater on log scale, converted to index",
       subtitle = "Wastwater value looks relatively higher in later dates than it actually is",
       y = "Transformed value",
       colour = "")

p2 <- d |>
  gather(variable, value, -date) |>
  filter(variable != "ww_log") |>
  group_by(variable) |>
  mutate(value = value / value[4]) |>
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line(size = 1.5) +
  theme(legend.position = "none") +
  labs(title = "Wastewater on original scale, converted to index",
       subtitle = "Wastewater value can be seen to lag the cases and grows and shrinks faster",
       y = "Transformed value",
       caption = "Values on vertical axis have not been back-transformed for labelling")

 
p1 + p2 + plot_layout(ncol = 1)
