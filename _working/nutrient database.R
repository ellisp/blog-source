


library(tidyverse)
library(rsdmx)
library(janitor)
library(xml2)
library(cluster)
library(dendextend)
library(ape)
library(TreeAndLeaf) # from https://bioconductor.org/packages/release/bioc/html/TreeAndLeaf.html
library(igraph)
library(RedeR)
library(RColorBrewer)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-19/")

if(!exists("pndb_raw")){
  # This is quite slow - several minutes - but the slow part is apparently parsing
  # the XML in the as_tibble
  pndb_raw <- readSDMX(providerId = "PDH", 
                       resource = "data", 
                       flowRef = "DF_FOOD_NUTRIENTS") 
}


# you can get the structure here but it is in awkward XML and not very fast
# pndb_str <- readSDMX(providerId = "PDH", resource = "datastructure", flowRef = "DF_FOOD_NUTRIENTS")

# way faster and tidied into readable format using my own function here
pndb_codes <- get_pdh_codelists("DF_FOOD_NUTRIENTS") |>
  #filter(category_en == "Codelist for food categories") |>
  select(id, name)

# hm a problem here, not all the names are unique:
filter(pndb_codes, name == "Crab, mud, fresh, raw")

count(pndb_codes, name, sort = TRUE)
count(pndb_codes, id, sort = TRUE)

pndb_tb <-  pndb_raw |>
  as_tibble() |>
  clean_names()


pndb <- pndb_tb |>
  left_join(pndb_codes, by = c("food" = "id")) |>
  rename(food_name = name) |>
  left_join(pndb_codes, by = c("indicator" = "id")) |>
  rename(indicator_name = name) |>
  select(food, food_name, indicator_name, obs_value) |>
  spread(indicator_name, obs_value, fill = 0) |>
  clean_names()

colnames(pndb)

#-----------------------------------analysis------------------

rdp <- RedPort()
calld(rdp)


selected <- pndb |>
  select(food_name, energy_kcal, fat, protein, dietary_fibre, 
         cholesterol, calcium, iron, magnesium, niacin, potassium, retinol,
         riboflavin, sodium, thiamin, vit_e, vitamin_b12, vitamin_c, zinc) |>
  group_by(food_name) |>
  summarise(across(where(is.numeric), mean)) |>  sample_n(75)


m <- selected[ , -1] |>
  as.matrix() |>
  scale()

rownames(m) <- substr(selected$food_name, 1, 50)




c2 <- hclust(dist(m), "ave")
tal <- treeAndLeaf(c2) |>
  att.mapv(dat = selected, refcol = 1)


pal <- brewer.pal(9, "Reds")
tal <- att.setv(g = tal, from = "fat", to = "nodeColor",
                cols = pal, nquant = 10)

tal <- att.setv(g = tal, from = "energy_kcal", to = "nodeSize",
                cols = pal, nquant = 10)

tal <- tal  |>
  # att.addv("nodeFontSize", value = 5) |>
  att.adde("edgeWidth", value = 2)

resetd(rdp)

#--- Send the tree-and-leaf to the interactive R/Java interface
addGraph(obj = rdp, g = tal, gzoom = 80)

#--- prvent inner nodes from relaxing
#selectNodes(rdp, V(tal)$name[!V(tal)$isLeaf], anchor=TRUE)

#--- Call 'relax' to fine-tune the leaf nodes

relax(rdp, p1 =10, p7 = 50)

addLegend.color(obj = rdp, tal, title = "Fat content per 100g",
                position = "bottomright")
addLegend.size(obj = rdp, tal, title = "Energy per 100g (KCal)")








c1 <- diana(m)
cd1 <- as.dendrogram(c1)

pdf("../img/nutrients.pdf", 22, 16)
plot(hang.dendrogram(cd1), horiz = TRUE, yaxt = "n")
dev.off()

pdf("../img/nutrients-fan.pdf", 22, 16)
plot(as.phylo(cd1), type = "fan", cex = 0.2, edge.color = "grey50", edge.width = 0.3)
dev.off()


pdf("../img/nutrients-unrooted.pdf", 22, 16)
plot(as.phylo(cd1), type = "unrooted", cex = 0.2, edge.color = "grey50", edge.width = 0.3,
     tip.color = "steelblue", lab4ut = "axial")
dev.off()

pc <- prcomp(m, scale = TRUE)
biplot(pc, choices = 1:2, pc.biplot = TRUE, col = c("darkblue", "grey75"), cex = 0.7,
       xlabs = substr(rownames(m), 1, 15))

m2 <- as_tibble(m) |>
  mutate(food_name = rownames(m),
         pc1 = predict(pc)[, 1],
         pc2 = predict(pc)[, 2])


