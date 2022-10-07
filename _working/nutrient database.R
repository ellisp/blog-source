


library(tidyverse)
library(rsdmx)
library(janitor)
library(xml2)
library(cluster)
library(dendextend)
library(ape)

if(!exists("pndb_raw")){
  # This is quite slow - several minutes - but the slow part is apparently parsing
  # the XML in the as_tibble
  pndb_raw <- readSDMX(providerId = "PDH", 
                       resource = "data", 
                       flowRef = "DF_FOOD_NUTRIENTS") 
}


pndb_tb <-  pndb_raw |>
  as_tibble() |>
  clean_names() |>
  rename(id = food)

#------------------getting the structure------------------------------  
# The actual English or French code names don't come down with rsdmx and I haven't
# worked out how to get around this. But we can get it directly from the web

pndb_str <- read_xml("https://stats-nsi-stable.pacificdata.org/rest/dataflow/SPC/DF_FOOD_NUTRIENTS/1.0?references=all")

st <- pndb_str |>
  xml_child(2) |>
  as_list() 

codelists <- st$Codelists

# What are these codelists:
as.character(sapply(codelists, function(x) x[1]$Name))

# Number 1 is "Codelist for food categories"
codelists1 <- codelists[[1]]
# first 2 items in this list aren't needed for our purposes
for(i in 1:2){codelists1[[1]] <- NULL}

food_cats <- tibble(
  food_name = as.character(sapply(codelists1, function(x){x$Name})),
  id = sapply(codelists1, function(x){attr(x, "id")})
)

# Number 8 is the food nutirents indicators
codelists8 <- codelists[[8]]
for(i in 1:2){codelists8[[1]] <- NULL}

food_inds <- tibble(
  indicator_name = as.character(sapply(codelists8, function(x){x$Name})),
  indicator = sapply(codelists8, function(x){attr(x, "id")})
)

pndb <- pndb_tb |>
  left_join(food_cats, by = "id") |>
  left_join(food_inds, by = "indicator") |>
  select(id, food_name, indicator_name, obs_value) |>
  spread(indicator_name, obs_value, fill = 0) |>
  clean_names()

colnames(pndb)

#-----------------------------------analysis------------------


selected <- pndb |>
  select(food_name, energy_kcal, fat, protein, dietary_fibre, 
         cholesterol, calcium, iron, magnesium, niacin, potassium, retinol,
         riboflavin, sodium, thiamin, vit_e, vitamin_b12, vitamin_c, zinc) |>
  arrange(desc(energy_kcal))

m <- selected[ , -1] |>
  as.matrix() |>
  scale()

set.seed(123)
rownames(m) <- selected$food_name

c1 <- diana(m)
cd1 <- as.dendrogram(c1)
plot(hang.dendrogram(cd1), horiz = TRUE, yaxt = "n")

plot(as.phylo(cd1), type = "fan", cex = 0.5)


pc <- prcomp(m, scale = TRUE)
biplot(pc, choices = 1:2, pc.biplot = TRUE, col = c("darkblue", "grey75"), cex = 0.7,
       xlabs = substr(rownames(m), 1, 15))

m2 <- as_tibble(m) |>
  mutate(food_name = rownames(m),
         pc1 = predict(pc)[, 1],
         pc2 = predict(pc)[, 2])
