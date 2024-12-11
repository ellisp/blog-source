

library(tidyverse)
library(janitor)

# World Energy Outlook
d <- read_csv("WEO2024_AnnexA_Free_Dataset_World.csv") |>
  clean_names()


d
count(d, scenario)
count(d, category, product, flow)

d |>
  filter(flow == "Total energy supply" & category == "Energy") |>
  filter(product == "Renewables") |>
  # convert from EJ to Gigawatt hours:
  ggplot(aes(x = year, y = value *  277777.77778, colour = scenario)) +
  geom_line()


# World Energy Investment