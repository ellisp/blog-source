# minimal adaption from Jenny Richmond
# https://jenrichmond.github.io/maps/2025-11-04_mydata/

#----------------setup-------------
library(tidyverse)
library(maps)
library(tidygeocoder)
library(ggeasy)
library(ggrepel)
library(glue)

#-----------Map-------------------

mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble() |> 
  filter(long > 100  & long <330 & lat < -0 & lat > -50) 

#-------------------Places------------------
# tidygeocoder is magic! make a df of places, feed it to geocode() and it will add lat/long columns!

places <- tibble::tribble(
  ~name,                 ~places,                  ~period,      ~order, ~display_place, 
  "Childhood",          "Perth, Australia",        "1972-1994",     1, "Perth",
  "First real job",     "Brisbane, Qld",           "1994-1996",     2, "Brisbane",
  "Home again",         "Perth, Australia" ,       "1996",          3, "Perth",
  "AusAID",             "Canberra, Australia",     "1997-2000",     4, "Canberra",
  "Home again",         "Fremantle, Australia" ,   "2000-2001",     5, "Fremantle",
  "AusAID",             "Canberra, Australia",     "2001-2004",     6, "Canberra",
  "Posting",            "Dili, East Timor" ,       "2004-2006",     7, "Dili",
  "AusAID",             "Canberra, Australia" ,    "2006-2007",     8, "Canberra",
  "NZAID/MBIE/etc",     "Wellington, New Zealand", "2007-2018",     9, "Wellington",
  "Nous Group",         "Melbourne, Australia" ,   "2018-2022",    10, "Melbourne",
  "SPC",                "Noumea, New Caledonia" ,  "2022-present", 11, "Noumea"
)

lat_longs <- places %>%
  geocode(places, method = 'osm', lat = latitude , long = longitude) %>%
  mutate(longitude = ifelse(longitude < 0, longitude + 360, longitude)) # transform to get pacific centred

pairs <- lat_longs %>%
  mutate(next_longitude = lead(longitude),
    next_latitude = lead(latitude),
    next_place = lead(places),
    next_order = lead(order)) 

place_labels <- lat_longs |> 
  count(display_place, latitude, longitude) |> 
  mutate(display_place = ifelse(n > 1, glue("{display_place} x {n}"), display_place))


# draw map
m <- ggplot(mp1) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white") +
  coord_map() +
  geom_segment(data = pairs, 
              aes(x = longitude, y = latitude, xend = next_longitude, yend = next_latitude, colour = order),
              arrow = arrow(length = unit(0.3, "cm"), type = "open", ends = "last"),
              linewidth = 0.5,
              arrow.fill = NULL) +
  geom_point(data = lat_longs, aes(x = longitude, y = latitude), 
           size = 2, color = "black", fill = "black", shape = 21, 
           inherit.aes = FALSE) +
  geom_label_repel(data = place_labels, 
                 aes(x = longitude, y = latitude, label = display_place), 
                 size = 3, label.size = 0, fill = "wheat") +
  scale_colour_viridis_c(option = "A", direction = -1) +
  easy_remove_gridlines() +
  easy_remove_axes() +
  theme(panel.background = element_rect(fill = "lightsteelblue"),
        legend.position = "none")  +
  labs(title = "Places I've lived",
       subtitle = "Darker arrows are more recent.")

svg_png(m, "../img/0308-map", w = 8 , h  = 5)
