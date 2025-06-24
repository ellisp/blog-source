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
extras <- d |> 
  filter(week_to == "6") |> 
  mutate(
    week_from = "6",
    week_to = NA, 
    severity_from = severity_to)

weekf <- function(x){
  x <-  case_when(
    x == 0 ~ "Week zero",
    x == 1 ~ "Week one",
    x == 3 ~ "Week three",
    x == 6 ~ "Week six"
  )
  x <- factor(x, levels = c("Week zero","Week one","Week three", "Week six"))
}


# we have some adjustments to deal with because of having made up data
total_people <- 1

# there should be the same total number of people each week
d2 <- d |> 
  rbind(extras)  |> 
  mutate(week_from =  weekf(week_from),
         week_to = weekf(week_to))
  
for(i in 1:5){
  d2 <- d2 |> 
    group_by(week_from) |> 
    mutate(value = value / sum(value) * total_people) |> 
    group_by(week_to) |> 
    mutate(value = value / sum(value) * total_people) |> 
    ungroup()   
  
  tot_arrived <- d2 |>  
    group_by(week_from, severity_from) |> 
    mutate(arrived_sev_from = sum(value)) |> 
    group_by(week_to, severity_to) |> 
    mutate(arrived_sev_to = sum(value)) |> 
    ungroup() |>  
    distinct(week_to, severity_to, arrived_sev_to)
  
  d2 <- d2 |> 
    left_join(tot_arrived, by = c("week_from" = "week_to",
                                  "severity_from" = "severity_to")) |> 
    group_by(week_from, severity_from) |> 
    mutate(value = if_else(is.na(arrived_sev_to), value, 
                                 value / sum(value) * unique(arrived_sev_to)) ) |> 
    select(-arrived_sev_to) |> 
    ungroup()
}
d2

# these should be  basically the same numbers
filter(tot_arrived, week_to == "Week one" & severity_to == 4)
filter(d2, week_to == "Week one" & severity_to == 4) |> summarise(sum(value))
filter(d2, week_from == "Week one" & severity_from == 4) |> summarise(sum(value))




pallette <-  c("grey", brewer.pal(7, "RdYlBu")[7:1])
names(pallette) <- c("NA", 1:7)

d2 |> 
  mutate(value = round(value * 1000)) |> 
  uncount(weights = value) |> 
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
  theme_sankey() +
  scale_fill_manual(values = pallette)



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