
library(tidyverse)
library(googlesheets4)
library(ggrepel)

d <- read_sheet("https://docs.google.com/spreadsheets/d/18oTWyHi9Wxz40hfX8ZoYuS5ezgLTBsD_0eT8JaGV62k/edit#gid=172673943",
                 sheet = "Final OECD and Oxfam table")


d |>
  ggplot(aes(x = `Percentage point reduction - rank (OECD)`, y = `Oxfam rank (of 27)`,
             label = Country)) +
  geom_smooth(method = "lm") +
  geom_point() +
  geom_text_repel() +
  labs(title = "Two ways of looking at income of tax on inequality",
       subtitle = "No relationship between OECD ranking of tax and transfer and the Oxfam rank of tax")
