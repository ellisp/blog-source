library(tidyverse)
library(sf)
library(Cairo)
library(janitor)




sf_use_s2(FALSE)

CairoWin()

fn <- "global_ffa_spc_sla_pol_june2022_kml.zip"
if(!file.exists(fn)){
  url <- "https://pacificdata.org/data/dataset/a89d83bc-378d-4679-b1fb-7096e76f2e30/resource/4dad0629-4cf3-498e-9f56-75f5c49c2763/download/global_ffa_spc_sla_pol_june2022_kml.zip"
  download.file(url, destfile = fn, mode = "wb")
}

unzip(fn)

eez <- st_read(gsub("_kml\\.zip", ".kml", fn))

pac <- eez |>
  slice(253:282) |>
  clean_names() |>
  filter(!grepl("Joint", name)) |>
  filter(!grepl("New Zealand", name)) |>
  filter(!grepl("Howland and Baker", name)) |>
  filter(!grepl("Palmyra", name)) |>
  filter(!grepl("Wake Island", name)) |>
  filter(!grepl("Matthew and Hunter", name)) |>
  filter(!grepl("Jarvis", name)) |>
  st_shift_longitude() |>
  mutate(name2 = gsub(" Exclusive Economic Zon.*", "", name)) |>
  mutate(name2 = gsub("n$", "", name2)) |>
  mutate(name2 = ifelse(name2 %in% c("Niuea", "Fijia", "Naurua", "Kiribatia"),
                 str_sub(name2, end = -2),
                 name2)) 

pac_c <- st_centroid(pac)

pac_c <- cbind(pac_c, st_coordinates(pac_c)) |>
  clean_names()




#https://stackoverflow.com/questions/34011100/plot-pacific-ocean-and-continents-with-ggplot2borders
mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
mp <- rbind(mp1, mp2)




ggplot(pac) +
  geom_sf(aes(fill = name2), colour = NA) +
  geom_polygon(data = filter(mp, long > 110 & long < 300 & lat < 20 & lat > -50),
               aes(x = long, y = lat, group = group),
               fill = "white") +
  geom_text(data = pac_c, aes(label = name2, x = x, y = y), colour = "grey40") +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "lightsteelblue", colour = NA))
