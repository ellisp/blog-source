library(tidyverse)
library(openxlsx)
library(htmlwidgets)
library(stringr)
library(visNetwork)
library(RColorBrewer)

# See http://archive.stats.govt.nz/methods/classifications-and-standards/classification-related-stats-standards/ethnicity.aspx


download.file("http://archive.stats.govt.nz/~/media/Statistics/surveys-and-methods/methods/class-stnd/ethnicity/ETHNIC05-v2-classification-all.xlsx",
              mode = "wb", destfile = "ethnic05.xlsx")

nodes <- read.xlsx("ethnic05.xlsx")
names(nodes) <- c("id", "label")
unlink("ethnic05.xlsx")


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

# see https://stackoverflow.com/questions/34439928/embedding-an-r-htmlwidget-into-existing-webpage
