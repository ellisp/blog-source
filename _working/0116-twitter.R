library(twitteR)
library(data.table)
library(tidyverse)
library(igraph)
library(tidygraph)
library(htmlwidgets)
library(threejs)
library(testthat)
library(stringr)
library(visNetwork)

me <- getUser("ellis2013nz")


all_my_followers <- me$getFollowerIDs()          # about 1 second for c.1250 IDs
their_details  <- lookupUsers(all_my_followers)  # about 15 seconds
length(all_my_followers)
length(their_details) # less - probably because are not allowed to access the details for all IDs


people <- do.call("rbind", lapply(their_details, as.data.frame))
expect_equal(nrow(people), length(their_details))

people <- people %>%
  mutate(desc_clean = iconv(description, "latin1", "ASCII", sub=""),
         name_clean = iconv(name, "latin1", "ASCII", sub=""),
         name_desc = paste0(name_clean, ": ", desc_clean))

View(people)
summary(people)

# alternative is to a) fix the encoding and then b) map to emoji names at https://github.com/lyons7/emojidictionary/blob/master/emoji_dictionary.csv

try(dir.create("who_follows_whom"))

for(i in 950:nrow(people)){
  
  cat(paste0(i, " "))
  # number of calls left to make under Twitter's limits
  lims <- getCurRateLimitInfo()
  x <- as.numeric(filter(lims, resource == "/friends/ids")$remaining)
  while(x == 0)  {
    lims <- getCurRateLimitInfo()
    x <- as.numeric(filter(lims, resource == "/friends/ids")$remaining)
    message("Waiting")
    Sys.sleep(60) # wait 15 minutes (or 1 minute, 15 times)
  }
  
  twit_id <- people[i, "id"]
  
  they_follow <- NULL
  try(they_follow <- their_details[[i]]$getFriendIDs()) # sometimes you don't have permission to access who they follow...
  
  if(!is.null(they_follow)){
    tmp <- data.frame(id = twit_id, they_follow = they_follow, stringsAsFactors = FALSE)
    fwrite(tmp, file = paste0("who_follows_whom/", twit_id, ".csv"))    
  }

}


#================load data back in========================
who_follows_whom <- data.frame(id = character(), they_follow = character(),
                               stringsAsFactors = FALSE)

files <- list.files("who_follows_whom", pattern = "\\.csv$", full.names = TRUE)
for(j in 1:length(files)){
  tmp <- as.data.frame(fread(files[j], colClasses = c("character", "character")))
  names(tmp) <- c("id", "they_follow")
  who_follows_whom <- rbind(who_follows_whom, tmp)
}

wfw <- who_follows_whom %>%
  filter(they_follow %in% all_my_followers) 

# note some people IDs are in all_my_followers but not in people$id - I think due to privacy settings

nrow(wfw)

edges_this_network <- wfw %>%
  group_by(they_follow) %>%
  summarise(edges = n()) %>%
  ungroup() %>%
  rename(id = they_follow)


# people with the most connections - number of people in my network who follow them
edges_this_network %>%
  left_join(people, by = "id") %>%
  select(name, edges) %>%
  arrange(desc(edges))


#===========================3d graph===========================
set.seed(123)
g <-   wfw %>%
  sample_n(300) %>%
  as_tbl_graph()

connected_people <- data_frame(id = names(V(g))) %>%
  left_join(people, by = "id") %>%
  left_join(edges_this_network, by = "id")

                             
nv <- nrow(connected_people)
# table(connected_people$location)
# see http://kateto.net/network-visualization
saveWidget(graphjs(g, 
                   vertex.label = connected_people$name_desc, 
                   vertex.shape = connected_people$screenName,
                   # vertex.shape = connected_people$id, 
                   vertex.color = rep("steelblue", nv),
                   vertex.size = pmax(sqrt(connected_people$edges) / 50, 0.1),
                   edge.color = "grey80", bg = "white"),
           "0116-graph-3d.html")
# should be able to highlight selected vertices with brush = TRUE but I couldn't get this to work

#===============interactive 2d=================
set.seed(123)
nodes <- data.frame(id = unique(c(wfw$id, wfw$they_follow))) %>%
  sample_n(300) %>%
  left_join(people, by = "id") %>%
  mutate(value = ifelse(is.na(followersCount), 1, sqrt(followersCount) / 10),
         title = str_wrap(name_desc, 35),
         title = gsub("\\\n", "<br>", title),
         font.size = 12) %>%
  mutate(label = screenName) %>%
  select(id, label, value, title)

edges <- wfw %>% 
  rename(from = id, to = they_follow) %>%
  # this filtering is in so during dev can work with a subset of the nodes:
  filter(from %in% nodes$id & to %in% nodes$id)

visNetwork(nodes, edges, width = "1700px", height = "900px") %>%
  visIgraphLayout(physics = FALSE) %>%
  visNodes(shape = "box", borderWidth = 0,
           color = list(background = "steelblue", highlight = "red", hover = "orange"),
           font = list (color = "white")) %>%
  visEdges(smooth = FALSE, arrows = "middle",
           color = list(highlight = "red", hover = "orange", opacity = 0.5)) %>%
  saveWidget(file = "0116-graph-2d-static.html")

visNetwork(nodes, edges, width = "1700px", height = "900px") %>%
  visPhysics(stabilization = FALSE, timestep = .3,
             barnesHut = list(centralGravity = 0.6,
                              gravitationalConstant = -4000,
                              springLength = 80, 
                              avoidOverlap = 0)) %>%
  visEdges(smooth = FALSE, arrows = "middle") %>%
  saveWidget(file = "0116-graph-2d-wobbly.html")



