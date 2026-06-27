
# download latest versio of Maddison data from
# https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023

library(tidyverse)
library(readxl)
library(scales)
library(glue)

gdppc <- read_excel("mpd2023_web.xlsx", sheet = "GDPpc", skip = 2)

glimpse(gdppc)

the_country <- c("AUS", "Australia")
the_country <- c("USA", "United States")


data_tc <- gdppc |> 
  select(year, the_country[1]) |> 
  filter(year >= 1900)
names(data_tc)[2] <- "value"

constant_growth <- data_tc |>
  arrange(year) |> 
  summarise(n = max(year) - min(year),
            start = value[1],
            end = value[n()],
            mid_point = (end + start) / 4) |> 
  mutate(growth_rate = (end / start) ^ (1 / n) - 1)

cgcol <- "red"
dcol <- "blue"

p1 <- data_tc |> 
  ggplot(aes(x = year, y = value)) +
  annotate("segment", x = min(data_tc$year), xend = max(data_tc$year),
            y = constant_growth$start, yend = constant_growth$end, 
           colour = cgcol, linetype = 2) +
  geom_line(colour = dcol) +
  annotate("text", x = 1900, y = constant_growth$mid_point, 
           label = glue("Constant growth of {percent(constant_growth$growth_rate, accuracy = 0.1)}"), 
           colour = cgcol, hjust = 0) +
  annotate("text", x = 2010, y = constant_growth$mid_point, 
            label = "Actual GDP per capita", 
            colour = dcol, hjust = 1) +
   scale_y_log10(label = dollar_format(accuracy = 1), breaks = 0:6 * 10000) +
  labs(x = "",
       y= "",
       title = glue("Long term historical growth in GDP per person in {the_country[2]}"),
       subtitle = "GDP per capita, purchasing power parity, 2011 prices.",
       caption = "Source: Maddison Project Database 2023")

svg_png(p1, glue("../img/0326-{the_country[2]}"), w = 9, h = 5)
