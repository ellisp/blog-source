---
layout: post
title: Showing women proportion of Parliamentarians on a map
date: 2023-05-23
tag: 
   - Spatial
   - DataVisualisation
description: I have a go at showing the proportion of members of Parliament that are women on a map of the world, as an experiment in a tricky data visualisation
image: /img/0245-wom-map.svg
socialimage: http://freerangestats.info/img/0245-wom-map.png
category: R
---


In a workshop session on data visualization yesterday, Christophe Bontemps of the Statistical Institute for Asia and Pacific (SIAP) used as an example a range of infographics attempting to show, on a map of Asia and the Pacific, the proportion of members of Parliament that are women. The point of the exercise was to illustrate the challenges with the various ways it could be done - using colours, shapes or sizes of glyphs drawn in the countries centres, or as a choropleth map - none of the methods were particularly satisfactory.

I did think the most promising method was when the proportion was shown as a vertical bar for each country. As used by Christophe this wasn't great because it was difficult to compare the heights of the bars scattered all over the map (which was the point of his illustration). But mixing this up with an idea he'd mentioned earlier in his talk, I thought the bars could work if you could add a reference line or shape showing 50%, and also use the colour coding. Plus a participant asked for numbers added to the map, which we agreed could work so long as it didn't get cluttered.

I had some sleep-deprived time in an airport the morning after so thought I'd give a go to implementing this approach, and came up with this, which I think is pretty good!

<object type="image/svg+xml" data='/img/0245-wom-map.svg' width='100%'><img src='/img/0245-wom-map.png' width='100%'></object>

I did cheat a bit by knocking out the Middle East because the map got really cluttered over there. For many purposes Middle East is treated differently to Asia by various international organisations, so there is some justification in this. There's also an issue that my bars' heights are calculated in degrees latitude, and with the projection of the map onto flat screen that should mean that the bars in countries with higher latitudes will be shorter. But this doesn't seem to be a visual problem, because of the effectiveness of the pale blue 50% reference blocks.

There's a couple of spots where the clutter stopped the numbers being readable in my first iteration - for North Korea and for Cambodia and Laos - and I had to code in some ad hoc adjustments for them. But overall the effect was pretty good, even before I made those corrections.

In terms of the substance here, not much to say. It really stands out how New Zealand, Australia and (perhaps more suprisingly to some) Timor Leste are the front runners with near-equity in gender representation in Parliament. Uzbekhistan, Nepal and Vietnam make up the second tier with 30% or higher. Pacific Island countries, Japan and South Asia (other than Nepal) stand out in the other direction.

Here's the code that makes the map, all in one chunk. It turned out to be pretty straightforward. The main trick is I had to pre-calculate coordinates for the heights of the 50% rectangle and the segments showing the actual height, rather than letting ggplot calculate it on the fly like it would for a bar chart.

{% highlight R lineanchors %}
library(tidyverse)
library(rnaturalearth)
library(sf)
library(WDI)
library(extrafont)

# Data on proportion of women in Parliament from World Development Indicators:
wom_raw <- WDI(indicator = "SG.GEN.PARL.ZS")

# Get the latest year for each country:
wom <- wom_raw |>
  as_tibble() |>
  rename(wom_parl = SG.GEN.PARL.ZS) |>
  filter(!is.na(wom_parl)) |>
  group_by(iso3c) |>
  arrange(desc(year)) |>
  slice(1) |>
  ungroup() 

# maps for Asia and Pacific from Natural earth
sf_use_s2(FALSE)
m1 <- ne_countries(scale = "medium", returnclass = "sf", continent = "asia") 
m2 <- ne_countries(scale = "medium", returnclass = "sf", continent = "oceania") 
m <- rbind(m1, m2)

  # extract the centres:
centers <- st_centroid(m, of_largest_polygon = FALSE) |>
  # turn them into straight numbers, not sf geometries:
  st_coordinates() |> 
  # add those coordinates as columns to the original map:
  cbind(m) |>
  # join to the women in parliament data:
  left_join(wom, by = c("iso_a3" = "iso3c")) |>
  rename(x = X, y = Y) |>
  # a couple of manual adjustments to make things more readable
  mutate(x = case_when(
    geounit == "North Korea" ~ x - 1.5,
    geounit == "Vietnam" ~ x + 1,
    geounit == "Laos" ~ x - 0.8,
    TRUE ~ x
  )) |>
  # create scaled latitude coordinates for our rectangles and segments:
  mutate(sc = 5,
         yend1 = y + wom_parl / sc,
         yend0 = y + 50 / sc) |>
  filter(!is.na(wom_parl)) |>
  # knock out the middle east because it's cluttered :):
  filter(x > 60)

xw <- 1
the_font <- "Calibri"
wom_map <- ggplot(centers) +
  geom_sf(colour = "grey50") +
  # background 50% reference rectangle:
  geom_rect(aes(xmin = x - xw, xmax = x + xw, ymin = y, ymax = yend0),
            fill = "steelblue", alpha = 0.5) +
  # segment/line/bar showing the actual data:
  geom_segment(aes(x = x, xend = x, y = y, yend = yend1, colour = wom_parl),
               linewidth = 2) +
  geom_text(aes(label = round(wom_parl), x = x, y = yend1 + 1, colour = wom_parl), 
            vjust = 0, size = 3, family = the_font) +
  theme_void(base_family = the_font) +
  xlim(60, 180) +
  scale_colour_viridis_c(option = "B", limits = c(0, 60), 
                         label = number_format(suffix = "%")) +
  theme(legend.position = c(0.2, 0.35)) +
  labs( colour = "",
        title = "Percentage of parliamentarians who are women",
        subtitle = "Blue bars represent 50%")

print(wom_map)
{% endhighlight %}

That's all.
