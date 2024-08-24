
library(tidyverse)
library(mapproj)
library(sf)
library(extrafont)
library(rnaturalearth)
library(ggforce) # for geom_circle
library(glue)


# Function for summarising numbers
nf <- function(x){
  y <- case_when(
    x > 1e6 ~ glue::glue("{round(x / 1e6, 1)}m"),
    TRUE ~ scales::comma(signif(x, 2))
  )
  return(y)
}


# obtained CRS from https://spatialreference.org/ref/epsg/?search=polar
south_crs <- st_crs(3031)
north_crs <- st_crs(3411)


# 27MB download:
download.file("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/geonames-all-cities-with-a-population-1000/exports/csv?lang=en&timezone=UTC&use_labels=true&delimiter=%3B",
              destfile = "cities.txt")

cities <- read_delim("cities.txt", delim = ";")


d <- cities |>
  separate(Coordinates, sep = ",", into = c("lat", "long")) |>
  mutate(lat = as.numeric(str_squish(lat)),
         long = as.numeric(str_squish(long))) |>
  select(Name, `Country name EN`, `Country Code`, Population, lat, long) 

most_north <- d |>
  arrange(desc(lat)) |>
  mutate(max_pop = cummax(Population)) |>
  filter(Population >= max_pop) |>
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = st_crs("WGS84"))

most_north <- st_transform(most_north, crs = north_crs)
most_north <- cbind(most_north, st_coordinates(most_north))


most_south <- d |>
  arrange(lat) |>
  mutate(max_pop = cummax(Population)) |>
  filter(Population >= max_pop) |>
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = st_crs("WGS84"))

most_south <- st_transform(most_south, crs = south_crs)
most_south <- cbind(most_south, st_coordinates(most_south))

# borders of countries north of Shanghai
north_world <- ne_countries() |>
  filter(label_y > 30) |>
  st_transform(crs = north_crs)

# borders of countries south of Shanghai
south_world <- ne_countries() |>
  filter(label_y < 35) |>
  st_transform(crs = south_crs)

ff <- "Calibri"

m1 <- most_north |>
  mutate(zero = 0,
         radius = sqrt(X ^ 2 + Y ^ 2),
         label = glue("{Name}, {nf(Population)}")) |>
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "steelblue") +
  geom_sf(data = north_world, fill = "grey90", colour = NA) +
  geom_circle(aes(x0 = zero, y0 = zero, r = radius, colour = radius)) +
  geom_sf() +
  geom_text_repel(aes(x = X, y = Y, label = label), family = ff, seed = 123) +
  # these limits are in the transformed coordinates, were chosen by hand / trial and error:
  coord_sf(ylim = c(-3000000  , 7000000), lims_method = "orthogonal",
           xlim = c(-6000000, 6000000   )) +
  theme_void() +
  #  annotate("text", x = 0, y = 0, label = "Pole", family = ff) +
  scale_colour_viridis_c(direction = -1) +
  labs(title = "Settlements that have no larger settlement further north of them") +
  theme(legend.position = "none")




svg_png(m1, file = "../img/0272-most-north", w = 9, h = 9)






m2 <- most_south |>
  mutate(zero = 0,
         radius = sqrt(X ^ 2 + Y ^ 2),
         label = glue("{Name}, {nf(Population)}")) |>
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "steelblue") +
  geom_sf(data = south_world, fill = "grey90", colour = NA) +
  geom_circle(aes(x0 = zero, y0 = zero, r = radius, colour = radius)) +
  geom_sf() +
  geom_text_repel(aes(x = X, y = Y, label = label), family = ff, seed = 123) +
  # these limits are in the transformed coordinates, were chosen by hand / trial and error:
  coord_sf(ylim = c(-11000000  , 11000000), lims_method = "orthogonal",
           xlim = c(-7000000, 20000000   )) +
  theme_void() +
#  annotate("text", x = 0, y = 0, label = "Pole", family = ff) +
  scale_colour_viridis_c(direction = -1) +
  labs(title = "Settlements that have no larger settlement further south of them") +
  theme(legend.position = "none")

svg_png(m2, file = "../img/0272-most-south", w = 9, h = 9)


