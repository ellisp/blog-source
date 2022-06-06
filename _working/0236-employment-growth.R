
library(tidyverse)
library(scales)
library(DBI)
library(RColorBrewer)
library(glue)
library(readabs)
library(readxl)

readabs::download_abs_data_cube("labour-force-australia-detailed", "EQ05", path = ".")

d <- read_excel("./EQ05.xlsx", sheet = "Data 1", skip = 3) |>
  select(month_ending = `Mid-quarter month`,
         division = `Industry division of main job: ANZSIC (2006) Rev.2.0`,
         ft = `Employed full-time ('000)`,
         pt = `Employed part-time ('000)`) |>
  group_by(month_ending, division) |>
  summarise(employed_thousands = sum(ft + pt)) |>
  arrange(month_ending)



p0 <- d |>
  group_by(division) |>
  mutate(cagr = (employed_thousands[n()] / employed_thousands[1]) ^ (1/ (n() / 4)) - 1,
         abs_gr = mean(employed_thousands[(n() - 3) :n()]) - mean(employed_thousands[1:4])) |>
  ungroup() |>
  mutate(division = fct_reorder(glue("{division}\n{percent(cagr, accuracy = 0.1)}, {comma(abs_gr, accuracy = 1,suffix = 'k')}"), 
                                -abs_gr, .fun = mean)) |>
  ggplot(aes(x = month_ending, y = employed_thousands, colour = cagr))  +
  theme_dark(base_family = "Segoe UI") +
  geom_line(size = 1.4) +
  expand_limits(y = 0) +
  scale_colour_gradientn(colours = brewer.pal(10, "RdBu")[1:10], label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = comma_format(suffix = "k")) +
  theme(legend.position = c(0.92, 0.11)) +
  labs(title = "Employment in Australia",
       subtitle = "Full or part time employed people.\nIndustry division ordered from greatest growth in absolute employment to least.\nNumbers show compound annual growth rate, absolute growth from first to last 12 month period (i.e. from 1991 to 2022).",
       x = "",
       y = "",
       colour = "Average annual\ngrowth rate",
       caption = "Source: ABS Labour Force Survey Detailed")

p1 <- p0 + facet_wrap(~division, scales = "fixed") 

p2 <- p0 + facet_wrap(~division, scales = "free_y") 

p1
p2
