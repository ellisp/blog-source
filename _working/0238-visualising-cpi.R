

library(tidyverse)
library(scales)
library(readxl)
library(htmlwidgets)
library(visNetwork)
library(RColorBrewer)

download.file("https://www.abs.gov.au/statistics/research/consumer-price-index-17th-series-weighting-pattern/64730.xls",
              destfile = "65730.xls", mode = "wb")

d <- read_excel("65730.xls", sheet = "Table 1", skip = 5,
                col_names = c("group", "sub_group", "class", "v1", "v2", "value")) |>
  fill(group, sub_group, class) |>
  filter(!is.na(value)) |>
  filter(group !=  'ALL GROUPS CPI') |>
  select(-v1, -v2) 


d |>
  ggplot(aes(x = value, y = group, fill = sub_group)) +
  geom_col(position = "stack") +
  ggplot2::scale_fill_discrete()


sum(d$class %in% d$sub_group)

#-----------network graph version-----------
# display.brewer.all(colorblindFriendly = TRUE)
pal <- brewer.pal(3, "Dark2")

nodes_simp <- d |>
  distinct(group, sub_group, class) |>
  gather(variable, label) |>
  rename(group = variable) |>
  distinct() |>
  mutate(id = 1:n()) 
  

edges <- distinct(d, from_lab = group, to_lab = sub_group) |>
  mutate(from_group = "group", to_group = "sub_group") |>
  rbind(
    mutate(
      distinct(d, from_lab = sub_group, to_lab = class),
    from_group = "sub_group",
    to_group = "class"
    )
    ) |>
  left_join(rename(nodes_simp, from = id), by = c("from_lab" = "label", "from_group" = "group")) |>
  left_join(rename(nodes_simp, to = id), by = c("to_lab" = "label", "to_group" = "group")) |>
  select(from, to)

nodes <- nodes_simp |>
  mutate(title = label) 

visNetwork(nodes, edges, width = "1700px", height = "900px") %>%
  visPhysics(stabilization = FALSE, timestep = .3,
             barnesHut = list(centralGravity = 0.6,
                              gravitationalConstant = -4000,
                              springLength = 80, 
                              avoidOverlap = 0)) |>
  visOptions(highlightNearest = TRUE, collapse = TRUE) |>
  visGroups(groupname = "group", color = pal[1], shape = "box", font = list(color = "white", size = 23)) |>
  visGroups(groupname = "sub_group", color = pal[2], shape = "ellipse") |>
  visGroups(groupname = "class", color = pal[3], shape = "text") |>
  visEdges(smooth = FALSE, arrows = "end") 
  

#----------Older file-----------------

nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
                    group = sample(c("A", "B"), 10, replace = TRUE))
edges <- data.frame(from = c(2,5,10), to = c(1,2,10))

nodes <- read.xlsx("ethnic05.xlsx")
names(nodes) <- c("id", "label")


edges <- expand.grid(nodes$id, nodes$id, stringsAsFactors = FALSE) %>%
  rename(from = Var1, to = Var2)%>%
  mutate(f_length = str_length(from)) %>%
  filter((str_length(to) == f_length + 1) | str_length(to) == 5 & f_length == 3) %>%
  filter(str_sub(to, 1, f_length) == from) %>%
  select(from, to) 

nodes <- nodes %>%
  mutate(
    # `title` is for tooltips:
    title = id, 
    # `group` is used for colour, so we use it to indicate the level of the classification:
    group = str_length(id))

pal <- brewer.pal(5, "YlOrRd")[4:1]


visNetwork(nodes, edges, width = "700px", height = "500px") %>%
  # visNetwork(nodes, edges, width = "1700px", height = "900px") %>%
  visPhysics(stabilization = FALSE, timestep = .3,
             barnesHut = list(centralGravity = 0.6,
                              gravitationalConstant = -4000,
                              springLength = 80, 
                              avoidOverlap = 0)) %>%
  visOptions(highlightNearest = TRUE, collapse = TRUE) %>%
  visGroups(groupname = "1", color = pal[1], shape = "box", font = list(color = "white", size = 23)) %>%
  visGroups(groupname = "2", color = pal[2], shape = "ellipse") %>%
  visGroups(groupname = "3", color = pal[3], shape = "ellipse") %>%
  visGroups(groupname = "5", color = pal[4], shape = "ellipse") %>%
  visEdges(smooth = FALSE, arrows = "end") %>%
  # saveWidget(file = "../img/0117-ethnicity.html", selfcontained = TRUE, title = "Stats NZ ethnicity classification")
  saveWidget(file = "../img/0117-ethnicity-large.html", selfcontained = TRUE, title = "Stats NZ ethnicity classification")
