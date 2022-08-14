library(tidyverse)
library(sf)
library(Cairo)
library(janitor)
library(ggrepel)
library(maps)
library(extrafont)
library(rnaturalearth) # for downloading dateline
library(rgdal)         # support for rnatural earth

#-----------------------land borders-------------------

# make two worlds together for drawing pacific-centred, adapted from
# https://stackoverflow.com/questions/34011100/plot-pacific-ocean-and-continents-with-ggplot2borders
mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
mp <- rbind(mp1, mp2) |>
  filter(long > 90  & long <360 & lat <50 & lat > -60) |>
  st_as_sf()

ggplot(mp) +
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_map()

#------------------international date line-----------------------------

if(!exists("glines")){
  glines <- ne_download(type = "geographic_lines", 
                        category = "physical", 
                        returnclass = "sf") |>
    st_shift_longitude() |>
    filter(name == "International Date Line")
}

#------------------------exclusive economic zones------------------------


fn1 <- "global_ffa_spc_sla_pol_june2022_kml.zip"
fn2 <- gsub("_kml\\.zip", ".kml", fn1)

if(!file.exists(fn1)){
  url <- "https://pacificdata.org/data/dataset/a89d83bc-378d-4679-b1fb-7096e76f2e30/resource/4dad0629-4cf3-498e-9f56-75f5c49c2763/download/global_ffa_spc_sla_pol_june2022_kml.zip"
  download.file(url, destfile = fn1, mode = "wb")
}

if(!file.exists(fn2)){
  unzip(fn1)
}

if(!exists("eez")){
  eez <- st_read(fn2)
}

sf_use_s2(FALSE)
pac <- eez |>
  slice(c(67, 245:282)) |>
  clean_names() |>
  filter(!grepl("Joint", name)) |>
  filter(!grepl("Triangle between", name)) |>
  filter(!grepl("Australia", name)) |>
  filter(!grepl("New Zealand", name)) |>
  filter(!grepl("Howland and Baker", name)) |>
  filter(!grepl("Palmyra", name)) |>
  filter(!grepl("Wake Island", name)) |>
  filter(!grepl("Matthew and Hunter", name)) |>
  filter(!grepl("Jarvis", name)) |>
  filter(!grepl("Hawaii", name)) |>
  st_shift_longitude() |>
  mutate(name2 = gsub(" Exclusive Economic Zon.*", "", name)) |>
  mutate(name2 = gsub("n$", "", name2)) |>
  mutate(name2 = ifelse(name2 %in% c("Niuea", "Fijia", "Naurua", "Kiribatia", "Tuvalua"),
                 str_sub(name2, end = -2),
                 name2)) |>
  mutate(id = 1:n()) |>
  # next 3 lines are for combining the countries that were split by 180 degrees into one
  # eg Tuvalu
  group_by(id, name2) |>
  dplyr::summarise(across(geometry, ~ sf::st_union(., by_feature = TRUE))) |>
  ungroup() 

pac_c <- st_centroid(pac)

pac_a <- st_area(pac) 

pac <- cbind(pac, st_coordinates(pac_c), area = pac_a) |>
  mutate(area2 = as.numeric(area))
#-------------------------combined map------------------------
ff <- "Roboto"

sf_use_s2(FALSE) # so reticules still drawn on right half of map
m1 <- ggplot(pac) +
  geom_sf(aes(fill = area2), colour = "grey70", alpha = 0.9) +
  geom_polygon(data = mp,
               aes(x = long, y = lat, group = group),
               fill = "white") +
  geom_sf(data = glines, colour = "grey30", linetype = 3) +
  annotate("text", x = 182, y = 38, label = "International date line", 
           colour = "grey30", hjust = 0, family = ff, size = 3) +
  geom_text(aes(label = name2, x = X, y = Y),
            colour = "grey20", family = ff, size = 3, angle = 15) +
  theme_minimal(base_family = ff) +
  scale_fill_viridis_c() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "lightsteelblue", colour = NA)) +
  coord_sf(xlim = c(120, 290),  ylim = c(-50, 50)) +
  labs(title = "Exclusive economic zones of selected Pacific countries and territories",
       subtitle = "Colours are just to make distinct areas easier to see. Included in this map are SPC Pacific Island countries and territories.",
       x = "",
       y = "")

svg_png(m1, "../img/0241-map1", w = 12, h = 7)


