library(glue)
library(sf)
library(tidyverse)

sf_use_s2(FALSE) # needed to load in the USA shapefile
aus_eez <- st_read("https://pacificdata.org/data/dataset/1cdf7b81-c0fa-4981-a106-c06a1e3fc74c/resource/17350800-9c8d-48da-ba74-5c3bee2778e6/download/au_eez_pol_april2022.kml")



# downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=8492
Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE=500)
#ind_eez <- st_read("MarineRegions-eez.kml")  |>
ind_eez <- st_read("ind-eez.json")  |>
  st_simplify(dTolerance = 1000) |>
  st_make_valid()
# ggplot(ind_eez) + geom_sf()


tl_eez <- st_read("east-timor-eez.json")   |>
  st_make_valid()
# ggplot(tl_eez) + geom_sf()

# download from https://www.marineregions.org/gazetteer.php?p=details&id=8322
phil_eez <- st_read("phil-eez.json")   |>
  st_make_valid()
# ggplot(phil_eez) + geom_sf()

# download from https://marineregions.org/gazetteer.php?p=details&id=8455
nz_eez <- st_read("nz-eez.json")   |>
  st_make_valid() |>
  st_shift_longitude()
# ggplot(nz_eez) + geom_sf()

# download from https://www.marineregions.org/gazetteer.php?p=details&id=8483
mal_eez <- st_read("malaysia-eez.json")   |>
  st_make_valid() |>
  st_shift_longitude()
# ggplot(mal_eez) + geom_sf()

# download from https://marineregions.org/gazetteer.php/gazetteer.php?p=details&id=26521
brn_eez <- st_read("brunei-eez.json")   |>
  st_make_valid() |>
  st_shift_longitude()
# ggplot(brn_eez) + geom_sf()

if(!file.exists("usa.zip")){
  download.file("https://maritimeboundaries.noaa.gov/downloads/USMaritimeLimitsAndBoundariesSHP.zip",
                destfile = "usa.zip", mode = "wb")
}
unzip("usa.zip")

usa_eez <- st_read("USMaritimeLimitsNBoundaries.shp") |>
  filter(EEZ == 1) |>
  filter(REGION %in% c(
#                       "Hawaiian Islands", 
                       "Howland Island, Baker Island and Jarvis Island",
                       "Johnston Atoll",
                       "Palmyra Atoll and Kingman Reef",
                       "Wake Island")) |>
  st_cast("POLYGON") |>
  st_make_valid() |>
  group_by(REGION) |>
  summarise() |>
  st_shift_longitude()

ggplot(usa_eez) + geom_sf()

#-------sf#------------------------exclusive economic zones------------------------
# Note for future users - this is dated June 2022, so presumably it changes from time to time.
# Check out the Pacific Data Hub for later versions.

fn1 <- "country_boundary_eez.geojson"

if(!file.exists(fn1)){
  url <- glue("https://pacificdata.org/data/dataset/964dbebf-2f42-414e-bf99-dd7125eedb16/resource/dad3f7b2-a8aa-4584-8bca-a77e16a391fe/download/{fn1}")
  download.file(url, destfile = fn1, mode = "wb")
}

if(!exists("eez")){
  eez <- st_read(fn1) |>
    st_shift_longitude()
  
  centers <- st_centroid(eez) |>
    st_shift_longitude() |>
    st_coordinates() |>
    as_tibble()
  
  eez <- cbind(eez, centers)
                              

}

non_forum <- c("ASM", "WLF", "GUM", "MNP", "TKL", "PCN")
eez <- eez |>
  mutate(forum = ifelse(ISO_Ter1 %in% non_forum, "non_pif", "pif"))
stopifnot(length(unique(filter(eez, forum == "non_pif")$GeoName)) == 6)

corners1 <- data.frame(lon = c(157, 360 -150), lat = c(-19, 10))
corners2 <- data.frame(lon = c(130, 157), lat = c(0, 5))


pockets_rect1 = st_polygon(
  list(
    cbind(
      corners1$lon[c(1,2,2,1,1)], 
      corners1$lat[c(1,1,2,2,1)])
  )
) |>
  st_sfc(crs = st_crs(eez)) |>
  st_shift_longitude()

pockets_rect2 = st_polygon(
  list(
    cbind(
      corners2$lon[c(1,2,2,1,1)], 
      corners2$lat[c(1,1,2,2,1)])
  )
) |>
  st_sfc(crs = st_crs(eez)) |>
  st_shift_longitude()



not_pockets <- st_union(eez, aus_eez) |>
  st_union(usa_eez) |>
  st_union(ind_eez) |>
  summarise()

pockets <- st_union(pockets_rect1, pockets_rect2) |>
  st_difference(not_pockets)

pockets |>
  ggplot() +
  geom_sf(fill = "blue", colour = "white")



# convenience function to calculate and round the total area in an sf object
ar <- function(x){
  format(as.numeric(round(sum(st_area(x) / 1e12), digits = 1)), nsmall = 1)
}

hs_col <- "blue"
us_col <- "darkgreen"

m <- ggplot(eez)  +
  geom_sf(data = pockets_rect1, fill = hs_col) +
  geom_sf(data = pockets_rect2, fill = hs_col) +
  geom_sf(data = phil_eez, fill = "grey65", colour = "white") +
  geom_sf(data = ind_eez, fill = "grey60", colour = "white") +
  geom_sf(data = mal_eez, fill = "grey70", colour = "white") +
  geom_sf(data = brn_eez, fill = "grey55", colour = "white") +
  geom_sf(data = tl_eez, fill = "grey50", colour = "white") +
  geom_sf(data = aus_eez, fill = "grey55", colour = "white") +
  geom_sf(data = nz_eez, fill = "grey65", colour = "white") +
  geom_sf(data = usa_eez, fill = us_col, colour = "white") +
  geom_sf(aes(fill = forum), colour = "white") +
  geom_text(aes(x = X, y = Y, label = ISO_Ter1), colour = "steelblue", size = 3) +
  borders(regions = 'Australia', fill = "white") +
  annotate("text", x = 196, y = 20, label = "US islands (excluding Hawai'i and SPC territories)", colour = us_col, hjust =0) +
  annotate("text", x = 206, y = 12, label = "High sea pockets under WCPO", colour = hs_col, hjust =0) +
  annotate("text", x = 191, y = -34, label = "Pacific Community (SPC) member\nPacific Island Countries and Territories (PICTs)", 
           colour = "steelblue", hjust = 0, lineheight = 0.9) +
  annotate("text", x = 231, y = -18, label = "PICTs that are not\nForum members", 
           colour = "turquoise", hjust = 0, lineheight = 0.9) +
  labs(x = "", y = "",
       title = "Some boundaries for the 'Blue Pacific Continent'? PICTs and neighbouring EEZ and high sea pockets",
       subtitle = glue("Area of EEZs in millions of square km: PICTs = {ar(eez)}, US islands = {ar(usa_eez)}, high sea pockets = {ar(pockets)}. Total area of 'Blue Pacific Continent', if around 35, is just under a quarter of the total Pacific.
All UN members' EEZ in the world is about 140; all EEZ + land area is about 275; total Pacific Ocean is 165; total Earth is around 510.")) +
  coord_sf(ylim = c(-35, 22)) +
  scale_fill_manual(values = c("pif" = "lightblue", "non_pif" = "turquoise")) +
  theme(legend.position = "none")

svg_png(m, "../img/0270-high-seas-map", w = 14, h = 7)
