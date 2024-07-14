library(glue)
library(sf)
library(tidyverse)
library(smoothr)

aus_eez <- st_read("https://pacificdata.org/data/dataset/1cdf7b81-c0fa-4981-a106-c06a1e3fc74c/resource/17350800-9c8d-48da-ba74-5c3bee2778e6/download/au_eez_pol_april2022.kml")



# downloaded from https://www.marineregions.org/gazetteer.php?p=details&id=8492
ind_eez <- st_read("MarineRegions-eez.kml")  |>
  st_simplify(dTolerance = 0.1)
# ggplot(ind_eez) + geom_sf()

download.file("https://maritimeboundaries.noaa.gov/downloads/USMaritimeLimitsAndBoundariesSHP.zip",
              destfile = "usa.zip", mode = "wb")
unzip("usa.zip")

usa_eez <- st_read("USMaritimeLimitsNBoundaries.shp") |>
  st_shift_longitude() |>
  filter(EEZ == 1) |>
  filter(REGION %in% c(
#                       "Hawaiian Islands", 
                       "Howland Island, Baker Island and Jarvis Island",
                       "Johnston Atoll",
                       "Palmyra Atoll and Kingman Reef",
                       "Wake Island")) |>
  st_cast("POLYGON") |>
  st_make_valid()


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

# about 5.3m square km
ar <- function(x){
  format(as.numeric(round(sum(st_area(x) / 1e12), digits = 1)), nsmall = 1)
}

hs_col <- "blue"
us_col <- "brown"

ggplot(eez)  +
  geom_sf(data = pockets_rect1, fill = hs_col) +
  geom_sf(data = pockets_rect2, fill = hs_col) +
  geom_sf(data = ind_eez, fill = "grey60", colour = "white") +
  geom_sf(data = aus_eez, fill = "grey55", colour = "white") +
  geom_sf(data = usa_eez, fill = us_col, colour = "white") +
  geom_sf(fill = "lightblue", colour = "white") +
  geom_text(aes(x = X, y = Y, label = ISO_Ter1), colour = "steelblue", size = 3) +
  borders(regions = 'Australia', fill = "white") +
  annotate("text", x = 196, y = 20, label = "US islands (excluding Hawai'i and SPC territories)", colour = us_col, hjust =0) +
  annotate("text", x = 206, y = 12, label = "High sea pockets under WCPO", colour = hs_col, hjust =0) +
  annotate("text", x = 181, y = -31, label = "Pacific Community (SPC) member\nPacific Island Countries and Territories (PICTs)", colour = "steelblue", hjust =0) +
  labs(x = "", y = "",
       title = "Some boundaries for the 'Blue Pacific Continent'? PICTs and neighbouring EEZ and high sea pockets",
       subtitle = glue("Area of EEZs in millions of square km: PICTs = {ar(eez)}, US islands = {ar(usa_eez)}, high sea pockets = {ar(pockets)}. Total area of 'Blue Pacific Continent', if around 35, is just under a quarter of the total Pacific.
All UN members' EEZ in the world is about 140; all EEZ + land area is about 275; total Pacific Ocean is 165; total Earth is around 510."))
