---
layout: post
title: Polar-centred maps
date: 2024-08-24
tag: 
   - Distributions
   - Simulations
   - Transformations
description: I draw maps of the largest settlements closest to the north pole and to the south pole, based on an idea by 'Brilliant Maps'. 
image: /img/0274-most-south.png
socialimage: https:/freerangestats.info/img/0274-most-south.png
category: R
---
I stumbled across [this page by Brilliant Maps](https://brilliantmaps.com/no-settlement-further-north/?fbclid=IwY2xjawE2h7NleHRuA2FlbQIxMAABHdcWMCUYztUNNvniU4XJvlfREJo24ulRyp4qks8c_cWKOpwspzjYCXa4uQ_aem_hy058OxYQsPH_Ay_SX8t5Q) showing settlements with no larger settlement to their north. The author noted that Helsinki was omitted by error. I wanted to address that error, and as a resident of the southern half of the globe myself, to produce a similar map with relation to the south pole.

First there's a matter of sourcing data on cities' and towns' populations. There are several possible sources but I chose [this dataset on opendatasoft, of all cities with a population > 1000](https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/table/?disjunctive.cou_name_en&sort=name). This doesn't go to the tiny settlements in the original Brilliant Maps image, but it's good enough for me.

Here's my prep stage, downloading that data and getting a few things ready for later use in spatial transformations (coordinate reference systems that are centred on the norht and south poles) and plot polishing (abbreviating numbers, fonts, etc).

{% highlight R lineanchors %}
#-----------------prep----------------
library(tidyverse)
library(sf)
library(extrafont)
library(rnaturalearth)
library(ggforce) # for geom_circle
library(glue)


# Function for summarising numbers
nf <- function(x){
  y <- case_when(
    x > 1e6 ~ glue::glue("{round(x / 1e6, 1)}m"),
    x > 1e4 ~ glue::glue("{round(x / 1e3, 0)}k"),
    TRUE ~ scales::comma(signif(x, 2))
  )
  return(y)
}

# Font to use
ff <- "Roboto"

#-------------------download cities data-------------

# obtained polar-centred coordinate reference systems (CRS) for later use, from 
# https://spatialreference.org/ref/epsg/?search=polar
south_crs <- st_crs(3031)
north_crs <- st_crs(3411)

# 27MB download:
if(!file.exists("cities.txt")){
  download.file("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/geonames-all-cities-with-a-population-1000/exports/csv?lang=en&timezone=UTC&use_labels=true&delimiter=%3B",
                destfile = "cities.txt")
}

if(!exists("cities")){
  cities <- read_delim("cities.txt", delim = ";")
}

d <- cities |>
  separate(Coordinates, sep = ",", into = c("lat", "long")) |>
  mutate(lat = as.numeric(str_squish(lat)),
         long = as.numeric(str_squish(long))) |>
  select(Name, `Country name EN`, `Country Code`, Population, lat, long) 
{% endhighlight %}

Next is the job of identifying cities that are bigger than any others to their north. This was a bit of a fiddly problem which I addressed by sorting all the cities by distrance from the north pole (highest latitude first), making a new column which is the cumulative maximum population so far, and retaining only those cities that are equal to or higher than that maximum population. This gets me a list of just 20 cities.

I then turn them into simple features objects with geometry (based on the latitude and longitude), and transform them to coordinates that will represent the world flattened out centred around the north pole, using the coordinate reference system I calculated before.

Then, I source country boundaries from the invaluable Natural Earth repository of map data (accessed via the `rnaturalearth` R package), and give them the same transformation, and turn both data sets into a single map.  

Here's the code doing those things:

{% highlight R lineanchors %}
#-----------------most north-------------

most_north <- d |>
  arrange(desc(lat)) |>
  mutate(max_pop = cummax(Population)) |>
  filter(Population >= max_pop) |>
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = st_crs("WGS84"))

most_north <- st_transform(most_north, crs = north_crs)
most_north <- cbind(most_north, st_coordinates(most_north))

# borders of countries north of Shanghai, from Natural Earth
north_world <- ne_countries() |>
  filter(label_y > 30) |>
  st_transform(crs = north_crs)

m1 <- most_north |>
  mutate(zero = 0,
         radius = sqrt(X ^ 2 + Y ^ 2),
         label = glue("{Name}, {nf(Population)}")) |>
  ggplot() +
  # draw the sea as background:
  geom_circle(data = tibble(x = 0, r = 8.5e6),
              aes(x0 = x, y0 = x, r = r), fill = "steelblue", colour = NA) +
  geom_sf(data = north_world, fill = "grey90", colour = NA) +
  geom_circle(aes(x0 = zero, y0 = zero, r = radius, colour = radius)) +
  geom_sf() +
  geom_text_repel(aes(x = X, y = Y, label = label), family = ff, seed = 123, size = 3) +
  # these limits are in the transformed coordinates, were chosen by hand / trial and error:
  coord_sf(ylim = c(-3000000  , 7000000), lims_method = "orthogonal",
           xlim = c(-6000000, 6000000   )) +
  theme_void(base_family = ff) +
  scale_colour_viridis_c(direction = -1) +
  labs(title = "Settlements that have no larger settlement further north of them") +
  theme(legend.position = "none")
{% endhighlight %}

And that results in this map:

<object type="image/svg+xml" data='/img/0274-most-north.svg' width='100%'><img src='/img/0274-most-north.png' width='100%'></object>

It's a little different from the original. As expected, I am missing settlements with less than 1,000 people, and I have correctly inserted Helsinki. But there are other differences which come from ambiguities in the population size of cities. For example, if you google "Tokyo 37 million" you are prompted by Google with the frequently searched question "Is Tokyo's population 14 or 37 million?". It turns out the "Tokyo Metropolis" is 14m and "Tokyo metropolitan area" is 37 million.

Obviously these are numbers that depend on decisions about boundaries, which as soon as you start thinking them through are revealed as being at least to a degree arbitrary. My data source opted on the 14m side for Tokyo. New York is also defined as a smaller area than in the Brilliant Maps original. I'm not particularly concerned with (or qualified to talk about) these urban definitional matters, so I'm just going to stick to the definitions used by my data source.

My first effort at drawing these maps did the transformation on the fly using `coord_map(projection='orthographic')` which worked ok, although it was harder (not impossible) to colour in the sea, and the constant-latitude circles I tried to draw for each city were crude and polygons. But when I tried to do the map centred on the south pole this method tripped up, and noting that `coord_map()` is now deprecated I went the route of doing the transformation explicitly and in advance first. This gave a nicer result, particularly for the circles, and control over the bounding box of the map. So it was worth the half hour or so of cursing as I worked to understand the method. R's power in geocomputation is growing so fast that anything on the web older than a couple of years is likely to be out of date - not necessarily wrong, but likely to miss a simpler and more powerful way of doing what you are trying.

Anyway, the "explicit transformation done before drawing the map" method worked well for any arbitrary projection, and here is the south pole version:

{% highlight R lineanchors %}
#------------most south---------------

most_south <- d |>
  arrange(lat) |>
  mutate(max_pop = cummax(Population)) |>
  filter(Population >= max_pop) |>
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = st_crs("WGS84"))

most_south <- st_transform(most_south, crs = south_crs)
most_south <- cbind(most_south, st_coordinates(most_south))


# borders of countries south of Shanghai
south_world <- ne_countries() |>
  filter(label_y < 35) |>
  st_transform(crs = south_crs)

m2 <- most_south |>
  mutate(zero = 0,
         radius = sqrt(X ^ 2 + Y ^ 2),
         label = glue("{Name}, {nf(Population)}")) |>
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "steelblue") +
  geom_sf(data = south_world, fill = "grey90", colour = NA) +
  geom_circle(aes(x0 = zero, y0 = zero, r = radius, colour = radius)) +
  geom_sf() +
  geom_text_repel(aes(x = X, y = Y, label = label), family = ff, seed = 123, size = 3) +
  # these limits are in the transformed coordinates, were chosen by hand / trial and error:
  coord_sf(ylim = c(-11000000  , 11000000), lims_method = "orthogonal",
           xlim = c(-7000000, 20000000   )) +
  theme_void(base_family = ff) +
  scale_colour_viridis_c(direction = -1) +
  labs(title = "Settlements that have no larger settlement further south of them") +
  theme(legend.position = "none")

{% endhighlight %}

...which gets me this map. Nice.

<object type="image/svg+xml" data='/img/0274-most-south.svg' width='100%'><img src='/img/0274-most-south.png' width='100%'></object>

This map goes well over the equator and in some ways gives a distorted view of the world; the sphere has been badly flattened and warped to show Shanghai and the south pole on the same rectangle. But I'm not going to worry about it for now.

OK that's all folks.
