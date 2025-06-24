library(tidyverse)
library(PantaRhei) # one approach to flow diagrams
library(janitor)
library(glue)
library(RColorBrewer)
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey) # another approach to sankey charts / flow diagrams

d <- read_csv("../data/complicated-sankey-data.csv", col_types = "ccccd", na = "missing") |> 
  clean_names()

#------------ggsankey approach---------------
extras <- tibble(
  week_from = 6,
  week_to = NA, 
  severity_from = c("NA", 1:7),
  severity_to = NA,
  value = 1
)

scaling_totals <- d |> 
  group_by(severity_to, week_to) |> 
  summarise(incoming = sum(value)) |> 
  ungroup()

d |> 
  rbind(extras) |> 
  left_join(scaling_totals, by = c("severity_from" = "severity_to", "week_from" = "week_to")) |> 
  group_by(severity_from, week_from) |> 
  mutate(value = ifelse(is.na(incoming), value, value / sum(value) * sum(incoming))) |> 
  ungroup() |>   
  mutate(value = round(value * 100)) |> View()
  uncount(weights = value) |> 
  mutate(week_from = case_when(
    week_from == 0 ~ "zero",
    week_from == 1 ~ "one",
    week_from == 3 ~ "three",
    week_from == 6 ~ "six"
  ),
  week_to = case_when(
    week_to == 0 ~ "zero",
    week_to == 1 ~ "one",
    week_to == 3 ~ "three",
    week_to == 6 ~ "six"
  )
  ) |> 
  mutate(severity_from = factor(severity_from, levels = c("NA", 1:7)),
         severity_to = factor(severity_to, levels = c("NA", 1:7))) |> 
  ggplot(aes(x = week_from, 
             next_x = week_to,
             node = severity_from, 
             next_node = severity_to,
             fill = severity_from,
             label = severity_from)) +
  geom_sankey() +
  geom_sankey_label() +
  theme_sankey()



#------------PantaRhei approach--------------
d1 <- select(d, week = week_from, severity = severity_from, value)
d2 <- select(d, week = week_to, severity = severity_to, value)

aspect <- 30
sc <- 1

sizes <- rbind(d1, d2) |> 
  group_by(week, severity) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(ID = paste(week, severity)) |> 
  arrange(week, severity) |> 
  mutate(y = cumsum(value)) |> 
  select(ID, y)

nodes <-  d1 |> 
  rbind(d2) |> 
  distinct(week, severity) |> 
  # note that sankey() does not work with characters of class glue
  mutate(label = as.character(glue("Severity: {severity}   ")),
         ID = paste(week, severity),
         x = as.numeric(week) * aspect * sc) |> 
  select(ID, label, x) |> 
  left_join(sizes,by = "ID") |> 
  mutate(label_pos = "left")

flows <- d |> 
  mutate(from = paste(week_from, severity_from),
         to = paste(week_to, severity_to)) |> 
  mutate(substance = severity_from) |> 
  select(from, to, substance, quantity = value) 
  
colors <-  tibble(
  substance = c("NA", 1:7),
  color = c("grey", brewer.pal(7, "RdYlBu"))
)

png("../img/0297-better-sankey.png", width = 8000, height = 5000, res = 600, type = "cairo-png")
sankey(nodes, flows, colors, max_width = 0.5, legend = TRUE)
dev.off()


# what's wrong with the original?
# positions of nodes not in sequency of severity
# colours don't show severity or direction of severity
# horizontal positions don't reflect the increasing amount of time