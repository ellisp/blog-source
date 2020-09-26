library(shiny)
library(gtools)
library(ggtern)

d <- expand.grid(p1 = 1:99 /100, p2 = 1:99 / 100) %>%
  mutate(p3 = 1 - p1 - p2) %>%
  filter(p3 >= 0.01) 

