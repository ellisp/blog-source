---
layout: post
title: Pacific island choropleth map
date: 2022-10-13
tag: 
   - DataVisualization
   - WorkRelated
   - Pacific
description: Drawing an annotated choropleth map of Pacific Island countries and territories.
image: /img/0241-map1.png
socialimage: https:/freerangestats.info/img/0241-map1.png
category: R
---

I wanted to draw my own nice, clean map of the Pacific Island countries and territories that I work with in my day job; and a workflow I could use for producing statistical graphics with it, particularly choropleth maps. I am going to build this into my `frs` R package to make it easier to re-use, but today's blog is my prototype putting it together from first principles.

First, here's the end product. As sample data I've used population per square kilometre of the exclusive economic zone, which is a slightly unusual metric that I was interested in at the time. 

<object type="image/svg+xml" data='/img/0241-map1.svg' width='100%'><img src='/img/0241-map1.png' width='100%'></object>

So here's how I made that. There was quite a bit involved but having worked it out I will package it up nicely (future post) so it can be easy to do for future.


## Land masses

To start with I need a simplified set of polygons showing where the Earth is land rather than sea. In my polished map the land is going to be coloured pale grey, nearly white, in the background. This would be easy if I wanted to centre my map on the Atlantic ocean, but because I'm at the other end of the Earth I need to centre the map somewhere in the Pacific. This comes up against the well-known anti-meridian mapping problem - the annoying glitch where many published spatial datasets representing the Earth have a problem with polygons that cross 180 degrees of longitude, causing all sorts of ugliness. This problem is so common that it clutters up Google searches for anything to do with how to draw maps centred in the Pacific.

There are of course many ways of dealing with this, but I used [this nice method](https://stackoverflow.com/questions/34011100/plot-pacific-ocean-and-continents-with-ggplot2borders) from a Stackoverflow answer, which basically creates data frame of all the points to connect to draw the world, and does it twice (with the second set having 360 added to all the longitude values), before filtering down to the area we actually want which will include some values at their original longitude (between -180 and 190) and some that have had 360 added to them - so the final longitudes after filtering are all between 0 and 360. Thinking about this as though a map is a scatter plot (which basically is indeed the case), with the 0 degree line going through Greenwich in the UK, having longitudes of 0 to 360 means we can have the UK on the far left of the plot area where we want it, whereas having them from -180 to 190 means UK has to be in the centre.

Here's the code that sets everything up for the session and gets going on that map:

{% highlight R lineanchors %}
library(tidyverse)
library(sf)
library(Cairo)
library(janitor)
library(ggrepel)
library(maps)
library(extrafont)
library(rnaturalearth) # for downloading dateline
library(rgdal)         # support for rnatural earth
library(rsdmx)
library(ISOcodes)
library(scales)
library(RColorBrewer)
library(glue)

mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
mp <- rbind(mp1, mp2) |>
  filter(long > 90  & long <360 & lat <50 & lat > -60) 

ggplot(mp) +
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_map()
{% endhighlight %}

That looks like this, which is pretty much perfect for my purposes:

<object type="image/svg+xml" data='/img/0241-land-map.svg' width='100%'><img src='/img/0241-land-map.png' width='100%'></object>

## International Date Line

Next, you may have noticed my target map has the International Date Line drawn through it. In our part of the world, this is important! Areas on the right side of that line are a day behind those on the left e.g. while I am sitting in my office in Noumea on a Monday, if I pick up the phone to talk to someone in Cook Islands, it will be Sunday there.

The Date Line is not a simple straight line because of the understandable desire of some countries not be split in two by it. Most obviously, Kiribati has arranged for the dateline to leap out to the east to include the area around Kiritimati atoll so they can be in the same day as the majority of the people on Tarawa, in the west.

The exact location of the international dateline and how to draw it is one of those things that's difficult to google because of clutter from the polygons-split-by-the-antimeridian problem referenced above, but eventually I tracked down an easy version to use, in ["Natural Earth"](https://www.naturalearthdata.com/). Natural Earth is a fantastic resource, providing free vector and raster map data at a range of scales.  The `rnaturalearth` package makes it super easy to download so long as you know exactly what you are looking for. This snippet of code does the job for the Date Line:

{% highlight R lineanchors %}
if(!exists("glines")){
  glines <- ne_download(type = "geographic_lines", 
                        category = "physical", 
                        returnclass = "sf") |>
    st_shift_longitude() |>
    filter(name == "International Date Line")
}
{% endhighlight %}

You'll see I've wrapped this, and some other bits of code in this with an `if(!exists...` statement so the downloading only happens the first time in an R session, saving valuable bits of download bandwidth and some time when I hit the "source" button to run my whole script.



## Polygons of the exclusive economic zones 

Now it gets a bit more complicated. I need data on the actual shapes of the world's exclusive economic zones, including the Pacific Island Countries and Territories I want to show. This is also available from the Pacific Data Hub, in several formats including KML or Keyhole Markup Language, a useful XML variant for geographic data used by the likes of Google Earth.

In this next chunk of code I:

- download the EEZ polygons as a KML file and read them into R as a "simple features" object (if you don't know what simple features is and how it has changed the world of geocomputation for the much, much better then Google it and be amazed), 
- shift its longitude so it will be centred on the Pacific (using the useful `st_shift_longitude()` function from the `sf` R package, that function being built for just this purpose), 
- knock it down to just the countries and territories of interest in the Pacific (which I established as polygons numbered 67 and 245 to 282 by inspecting it visually)
- cut it down further to exclude a few bits and pieces like Hawaii, Australia and Wake Island that I decided while might be useful one day, not for today.
- tidy up the names from reading (for example) "Fijian Exclusive Economic Zone" to be "Fiji". The code that does this using regex and some ad hoc fixes is a bit clunky, but works.
- does a clever (at least I think so) `group_by` and `summarise` trick to merge together a few shapes which are otherwise separated by our old friend, the antimeridian which would show up as a white line in our map without this fix.
- joins the final data frame to the official ISO 3166 country codes so I can use it later in combination with the statistical data from the Pacific Data Hub

{% highlight R lineanchors %}
#------------------------exclusive economic zones------------------------
# Note for future users - this is dated June 2022, so presumably it changes from time to time.
# Check out the Pacific Data Hub for later versions.

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
  mutate(name2 = case_when(
    name2 == "Micronesia" ~ "Micronesia, Federated States of",
    name2 == "Northern Mariana" ~ "Northern Mariana Islands",
    name2 == "Pitcairn Islands" ~ "Pitcairn",
    TRUE ~ name2
  )) |>
  mutate(id = 1:n()) |>
  # next 3 lines are for combining the countries that were split by 180 degrees into one
  # eg Tuvalu
  group_by(id, name2) |>
  dplyr::summarise(across(geometry, ~ sf::st_union(., by_feature = TRUE))) |>
  ungroup() 

stopifnot(
  pac |>
    anti_join(ISO_3166_1, by = c("name2" = "Name")) |>
    nrow() == 0)

{% endhighlight %}

## Population and EEZ area
So we've done the hardest spatial stuff, and now we can start thinking about colouring in the EEZs and adding nice labels of country names.

I need to extract the centroids of all of my polygons, so I've got x and y coordinates to place the labels on the map. In the code below this object is called `pac_c`

Then I got the data on latest population and the *area* (as a number, in square kilometres) of the exlusive economic zones from the "POCKET" (as in "pocket summary") data flow in the ".Stat" indicator database of the Pacific Data Hub [PDH.Stat](https://stats.pacificdata.org/), which is maintained by my team at work. I wrote a bit about this data source in [my last post](../2022-08-14-population-pyramids.html). The `rsdmx` R package gives easy access to the data from this, and other similar .Stat tools around the world that use the SDMX standard for disseminating statistical indicators. 

In the code below I download that data, pick the indicators I want and pivot it wider into a format with one row per country or territory called `pop_pdh`, and calculate the number of people per thousand square kilometres of EEZ. Then I join that object to the coordinates of the polygons centres, and I have a straightforward data frame of the actual, simple statistical data I want to represent with my map.

Finally (in this chunk), I calculate the upper and lower limit of seven categories I am going to use for setting the actual colour used on the map, and I store these for use in the object `quantiles`.

{% highlight R lineanchors %}
# get the centroids of each EEZ polygon
pac_c <- st_centroid(pac)

if(!exists("pop_pdh")){
  pocket <- readSDMX(providerId = "PDH", 
          resource = "data", 
          flowRef = "DF_POCKET")  |>
    as_tibble() |>
    clean_names() 
  
  pop_pdh <- pocket |>
    filter(indicator %in% c("EEZ", "MIDYEARPOPEST", "POPDENS")) |>
    group_by(indicator, geo_pict) |>
    arrange(desc(obs_time)) |>
    slice(1) |>
    select(geo_pict, indicator, obs_value) |>
    spread(indicator, obs_value) |>
    # people per thousand km2 of EEZ (different to land)
    mutate(pop_dens_eez = MIDYEARPOPEST / EEZ * 1000) |>
    ungroup()
}

pac <- cbind(pac, st_coordinates(pac_c)) |>
  left_join(select(ISO_3166_1, Name, geo_pict = Alpha_2, iso3 = Alpha_3), by = c("name2" = "Name")) |>
  # note this is EEZ of whole country, so Kiribati has all in one number not split in 3,
  # and will have 3 reps of the same number
  left_join(pop_pdh, by = "geo_pict") 
  
quantiles <- quantile(pac$pop_dens_eez, prob = seq(0, 1, length = 8), type = 5)
quantiles[1] <- 0
quantiles[length(quantiles)] <- quantiles[length(quantiles)] + 1
{% endhighlight %}


## How much of the world is in the Pacific?

There are a few calculated numbers I wanted to include in my subtitles:

- the proportion of the world's surface that is Pacific Island EEZs
- the proportion of the world's surface that is Pacific Island land area
- the proportion of the world's population that lives in Pacific Island Countries and territories

These are all interesting numbers that I think should be more widely known! So here's the calculation of them, and storing them in the objects `prop_surface`, `prop_land` and `prop_pop` respectively. I had to calculate the land area of the countries by dividing the population by the population density, as I don't believe the land area (as opposed to the EEZ area) is in the "Pocket" summary data I downloaded from the PDH.

{% highlight R lineanchors %}
#-----------how much of the world is there in the Pacific?-------------

# what proportion fo the world's surface (which is 510 million km2):
prop_surface <- percent(sum(pop_pdh$EEZ) / 510e6, accuracy = 0.1)

# proportion of world's population (which is 7.9 billion)
prop_pop <- percent(sum(pop_pdh$MIDYEARPOPEST) / 7.9e9, accuracy = 0.01)


total_land_area <- pop_pdh |>
  mutate(land_area = MIDYEARPOPEST / POPDENS) |>
  ungroup() |>
  summarise(land_area = sum(land_area)) |>
  pull(land_area)
# reality check - PNG is about 463k km2 so this (531k) looks right for all PICTs

# proportion of the world's total land area (which is 149 million km2)
prop_land <- percent(total_land_area / 149e6, accuracy = 0.01)

{% endhighlight %}

## Let's draw a map

OK, finally we are ready to draw our map. All the statistical calculations are done apart from the final calculation of which colour to shade each polygon, which is done in the code below just before the `ggplot` statement. All the rest of the code is polish and annotations of the map. A few things to note:

- the coloured EEZ polygons are drawn first, and because they are a simple features object they are drawn with the `geom_sf` geom
- the pale grey landmasses that perceptually are in the background are drawn second and are in fact semi-transparent white. Because these are not simple features but a straight data frame (remember we created this earlier as the very first step, a data frame of polygons of land masses centred on the Pacific) these land forms are drawn with `geom_polygon`. Having the white land mass drawn subsequently (so on top of) the coloured EEZ polygons was counter-intuitive to me but gives a better visual effect given how little land there is in the area of interest.
- The international date line is a simple features object (albeit just a line) so it is drawn with `geom_sf`

{% highlight R lineanchors %}
#-------------------------combined map------------------------
ff <- "Roboto" # font face of choice in one spot so easy to change if we want

sf_use_s2(FALSE) # so reticules still drawn on right half of map

pac |>
  mutate(dens_cat = cut(pop_dens_eez, breaks = round(quantiles), dig.lab = 5),
         dens_cat = fct_reorder(gsub(",", "-", dens_cat), pop_dens_eez)) |> 
  ggplot() +
  geom_sf(aes(fill = dens_cat), colour = "grey70", alpha = 0.9) +
  geom_polygon(data = mp,
               aes(x = long, y = lat, group = group),
               fill = "white",
               alpha = 0.8) +
  geom_sf(data = glines, colour = "steelblue", linetype = 1, alpha = 0.5) +
  annotate("text", x = 182, y = 38, label = "International date line", 
           colour = "steelblue", hjust = 0, family = ff, size = 3) +
   geom_text(aes(label = name2, x = X, y = Y),
             colour = "black", family = ff, size = 3, angle = 15) +
  theme_minimal(base_family = ff) +
  scale_fill_manual(values = brewer.pal(9, "Oranges")) +
  theme(legend.position = c(0.8, 0.7),
        panel.background = element_rect(fill = "lightsteelblue", colour = NA),
        panel.grid = element_blank(),
        plot.caption = element_text(colour = "grey50")) +
  coord_sf(xlim = c(120, 290),  ylim = c(-50, 50)) +
  labs(title = "Exclusive economic zones (EEZs) of Pacific Community island countries and territories",
       subtitle = glue("Together, the EEZs make up around {prop_surface} of the world's surface, {prop_pop} of the world's population and the land is {prop_land} of the world's total land area."),
       x = "",
       y = "",
       fill = "People per 1,000\nsquare km of EEZ",
       caption = "Source: https:/freerangestats.info with data from the Pacific Data Hub")
{% endhighlight %}

<object type="image/svg+xml" data='/img/0241-map1.svg' width='100%'><img src='/img/0241-map1.png' width='100%'></object>

Feedback is always appreciated! And to be clear, although I am in fact responsible for one of the key concentrations of experts on Pacific Island statistics, my actual job doesn't involve making maps and I am blogging very much in my personal capacity. None of the experts who work with me have checked this work. So all errors are my personal fault, not that of the institution I work for!
