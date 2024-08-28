library(rsdmx)
library(tidyverse)
library(janitor)
library(lubridate)

metadata <- tribble(~measure, ~full_measure,
                    "B6GS1M_R_POP", "Real gross disposable income per capita of households and NPISH",
                    "P3S1M_R_POP", "Real final consumption expenditure per capita of households and NPISH",
                    "B1GQ_R_POP", "Real gross domestic product per capita")

the_caption = "Source: OECD dot Stat P3S1M_R_POP, B6GS1M_R_POP, B1GQ_R_POP. 'NPISH' means 'non-profit institutions serving households'."

d <- readSDMX("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_HHDASH@DF_HHDASH_INDIC,1.0/Q.OECD+AUS.B1GQ_R_POP+B6GS1M_R_POP+P3S1M_R_POP.?dimensionAtObservation=AllDimensions") |>
  as_tibble() |>
  clean_names() |>
  mutate(tp = yq(time_period) ) |>
  left_join(metadata, by = "measure") |>
  mutate(ref_area = case_when(
    ref_area == "AUS" ~ "Australia",
    ref_area == "OECD" ~ "OECD average"
  ))


palette <- c("Australia" = "red", `OECD average` = "blue")

p1 <- d |>
  ggplot(aes(x = tp, y = obs_value, colour = ref_area)) +
  facet_wrap(~str_wrap(full_measure, 40)) +
  geom_line()  +
  scale_colour_manual(values = palette)  +
  theme(legend.position = c(0.1, 0.7)) +
  labs(colour = "",
       y = "Index (2007 = 100)",
       x = "",
       title = "Growth in household expenditure and income, Australia and OECD average",
       subtitle = "Indexes set to 100 in 2007. Comparison shows relative growth rates, not absolute difference.",
       caption = the_caption)

svg_png(p1, "../img/0275-inc-exp-gdp-2007", w = 12, h = 5)
# it's interesting how the Covid spike in expenditure is downwards but in income
# it was up (welfare benefits basically)

p2 <- d |>
  group_by(ref_area, measure) |>
  mutate(obs_value = obs_value / obs_value[time_period == "2014-Q1"] * 100) |>
  ggplot(aes(x = tp, y = obs_value, colour = ref_area)) +
  facet_wrap(~str_wrap(full_measure, 40)) +
  geom_line()  +
  scale_colour_manual(values = palette)  +
  theme(legend.position = c(0.1, 0.7)) +
  labs(colour = "",
       y = "Index (2014 = 100)",
       x = "",
       title = "Growth in household expenditure and income, Australia and OECD average",
       subtitle = "Indexes set to 100 in first quarter of 2014. Comparison shows relative growth rates, not absolute difference.",
       caption = the_caption)

svg_png(p2, "../img/0275-inc-exp-gdp-2014", w = 12, h = 5)


p3 <- d |>
  group_by(ref_area, measure) |>
  mutate(obs_value = obs_value / obs_value[time_period == "2014-Q1"] * 100) |>
  group_by(ref_area, tp, time_period) |>
  summarise(inc_exp_ratio = obs_value[measure == "B6GS1M_R_POP"] / obs_value[measure == "P3S1M_R_POP"] * 100) |>
  ggplot(aes(x = tp, y = inc_exp_ratio, colour = ref_area)) +
  geom_line()  +
  scale_colour_manual(values = palette)  +
  theme(legend.position = c(0.3, 0.7)) +
  labs(title = "Relative growth in household income as a proportion of expenditure",
       subtitle = "Ratio set to equal 100 in first quarter of 2014",
       colour = "",
       caption = the_caption,
       x = "",
       y = "Index (2014 = 100)")
# so AUstralian income hasn't been growing as fast as expenditure or in other words
# savings rates are4 declining, squashed between the income and expenditure

svg_png(p3, "../img/0275-inc-exp-ratio", w = 12, h = 5)



p4 <- d |>
  group_by(ref_area, full_measure) |>
  mutate(obs_value = obs_value / obs_value[time_period == "2014-Q1"] * 100) |>
  group_by(full_measure, tp, time_period) |>
  summarise(aus_oecd_ratio = obs_value[ref_area == "Australia"] / obs_value[ref_area == "OECD average"] * 100) |>
  ggplot(aes(x = tp, y = aus_oecd_ratio)) +
  facet_wrap(~str_wrap(full_measure, 40)) +
  geom_line(colour = "steelblue")  +
  theme(legend.position = c(0.3, 0.7)) +
  labs(title = "Australia | OECD average relative growth in various measures",
       subtitle = "Ratio set to equal 100 in first quarter of 2014. Indicator shows a relative comparison of growth rates, not an absolute comparison.",
       colour = "",
       caption = the_caption,
       x = "",
       y = "Index (2014 = 100)")

svg_png(p4, "../img/0275-aus-oecd-ratios", w = 12, h = 5)
