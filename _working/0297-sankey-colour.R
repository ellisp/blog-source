library(tidyverse)
library(PantaRhei)
library(janitor)
library(glue)

d <- read_csv("../data/complicated-sankey-data.csv", col_types = "ccccd", na = "missing") |> 
  clean_names()
summary(d)

nodes <- select(d, week = week_from, severity = severity_from) |>
  rbind(select(d, week = week_to, severity = severity_to)) |> 
  distinct(week, severity) |> 
  mutate(label = glue("Severity Week {week}: {severity}"),
         id = paste(week, severity, sep = ":"),
         x = as.numeric(week),
         y = if_else(
           severity == "NA", 1, as.numeric(severity) + 1
         )
  )
         

summary(nodes)

# what's wrong with the original?
# positions of nodes not in sequency of severity
# colours don't show severity or direction of severity
# horizontal positions don't reflect the increasing amount of time