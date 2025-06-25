library(tidyverse)
library(janitor)
library(glue)
library(RColorBrewer)
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey) # one fairly straightforward approach to sankey charts / flow diagrams

# read in some data. This was very crudely hand-entered with some
# rough visual judgements based from a chart that I don't know the
# origin of I saw on the internet. So treat as made-up example data:
d <- read_csv("https://raw.githubusercontent.com/ellisp/blog-source/refs/heads/master/data/complicated-sankey-data.csv", 
              col_types = "ccccd",
              # we want the NAs in the original to be characters, not actual NA:
              na = "missing") |> 
  clean_names()

#------------tidying up data---------------
# we have some adjustments to deal with because of having made up data

# An extra bunch of rows of data that are needed by the Sankey function to
# to make the week 6 nodes show up:
extras <- d |> 
  filter(week_to == "6") |> 
  mutate(
    week_from = "6",
    week_to = NA, 
    severity_from = severity_to)

#' Convenience relabelling function for turning week numbers into a factor:
weekf <- function(x){
  x <-  case_when(
    x == 0 ~ "Week zero",
    x == 1 ~ "Week one",
    x == 3 ~ "Week three",
    x == 6 ~ "Week six"
  )
  x <- factor(x, levels = c("Week zero","Week one","Week three", "Week six"))
}

# going to start by treating all flow widths as proportions 
total_people <- 1

# add in the extra data rows to show the final week of nodes,
# and relabel the weeks:
d2 <- d |> 
  rbind(extras)  |> 
  mutate(week_from =  weekf(week_from),
         week_to = weekf(week_to))

# there should be the same total number of people each week,
# and the same number of people leaving each "node" (a severity-week
# combination) as arrived at it on the flow from the last week.
# we have a little iterative process to clean this up. If we had
# real data, none of this would be necessary; this is basically
# because I made up data with some rough visual judgements:
for(i in 1:5){
  # scale the data so the population stays the same week by week
  # (adds up to total_people, which is 1, so are proportions)
  d2 <- d2 |> 
    group_by(week_from) |> 
    mutate(value = value / sum(value) * total_people) |> 
    group_by(week_to) |> 
    mutate(value = value / sum(value) * total_people) |> 
    ungroup()   
  
  # how many people arrived at each node (week-severity combination)?
  tot_arrived <- d2 |>  
    group_by(week_from, severity_from) |> 
    mutate(arrived_sev_from = sum(value)) |> 
    group_by(week_to, severity_to) |> 
    mutate(arrived_sev_to = sum(value)) |> 
    ungroup() |>  
    distinct(week_to, severity_to, arrived_sev_to)
  
  # scale the data leaving the node to match what came in:
  d2 <- d2 |> 
    left_join(tot_arrived, by = c("week_from" = "week_to",
                                  "severity_from" = "severity_to")) |> 
    group_by(week_from, severity_from) |> 
    mutate(value = if_else(is.na(arrived_sev_to), value, 
                                 value / sum(value) * unique(arrived_sev_to)) ) |> 
    select(-arrived_sev_to) |> 
    ungroup()
}

# manual check - these should all be  basically the same numbers
filter(tot_arrived, week_to == "Week one" & severity_to == 4)
filter(d2, week_to == "Week one" & severity_to == 4) |> summarise(sum(value))
filter(d2, week_from == "Week one" & severity_from == 4) |> summarise(sum(value))


#--------------draw plot-------------

# palette that is colourblined-ok and shows sequence. This
# actually wasn't too bad in the original, but it got lost
# in the vertical shuffling of all the severity nodes:
pal <-  c("grey", brewer.pal(7, "RdYlBu")[7:1])
names(pal) <- c("NA", 1:7)

# draw the actual chart:
p <- d2 |> 
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
  geom_sankey(alpha = 0.8) +
  geom_sankey_label() +
  theme_sankey(base_family = "Roboto") +
  scale_fill_manual(values = pal) +
  labs(title = "Severity of an unknown disease shown in a Sankey chart",
       subtitle = "Chart is still cluttered, but decreasing severity over time is apparent.
To achieve this, it's important that vertical sequencing and colour are both meaningfully mapped to severity.",
       x = "") +
  theme(legend.position = "none",
        plot.title = element_text(family = "Sarala"))

svg_png(p, "../img/0297-polished-sankey", w= 10, h = 6)

#------------PantaRhei approach--------------
# I didn't get this to work with necessary degree of polish,
# but it looks more powerful and effective
# library(PantaRhei) # one approach to flow diagrams
# 
# d1 <- select(d, week = week_from, severity = severity_from, value)
# d2 <- select(d, week = week_to, severity = severity_to, value)
# 
# aspect <- 30
# sc <- 1
# 
# sizes <- rbind(d1, d2) |> 
#   group_by(week, severity) |> 
#   summarise(value = sum(value)) |> 
#   ungroup() |> 
#   mutate(ID = paste(week, severity)) |> 
#   arrange(week, severity) |> 
#   mutate(y = cumsum(value)) |> 
#   select(ID, y)
# 
# nodes <-  d1 |> 
#   rbind(d2) |> 
#   distinct(week, severity) |> 
#   # note that sankey() does not work with characters of class glue
#   mutate(label = as.character(glue("Severity: {severity}   ")),
#          ID = paste(week, severity),
#          x = as.numeric(week) * aspect * sc) |> 
#   select(ID, label, x) |> 
#   left_join(sizes,by = "ID") |> 
#   mutate(label_pos = "left")
# 
# flows <- d |> 
#   mutate(from = paste(week_from, severity_from),
#          to = paste(week_to, severity_to)) |> 
#   mutate(substance = severity_from) |> 
#   select(from, to, substance, quantity = value) 
#   
# colors <-  tibble(
#   substance = c("NA", 1:7),
#   color = c("grey", brewer.pal(7, "RdYlBu"))
# )
# 
# png("../img/0297-better-sankey.png", width = 8000, height = 5000, res = 600, type = "cairo-png")
# sankey(nodes, flows, colors, max_width = 0.5, legend = TRUE)
# dev.off()


# what's wrong with the original?
# - positions of nodes not in sequency of severity
# - colours show severity ok but not colorblind-ok, and lack of vertical positioning 
#   means you only notice the mapping of colour with careful attention
# - horizontal positions don't reflect the increasing amount of time