---
layout: post
title: Showing women proportion of Parliamentarians on a map
date: 2023-05-26
tag: 
   - Spatial
   - Pacific
   - Visualisation
description: I have a go at showing the proportion of members of Parliament that are women on a map of the world, as an experiment in a tricky data visualisation
image: /img/0245-wom-map.svg
socialimage: http://freerangestats.info/img/0245-wom-map.png
category: R
---


In a workshop session on data visualization earlier this week, Christophe Bontemps of the Statistical Institute for Asia and Pacific (SIAP) used as an example a range of infographics attempting to show, on a map of Asia and the Pacific, the proportion of members of Parliament that are women. The point of the exercise was to illustrate the challenges with the various ways it could be done - using colours, shapes or sizes of glyphs drawn in the countries centres, or as a choropleth map - none of the methods were particularly satisfactory.

## Building a nice map showing proportions

I did think the most promising method was when the proportion was shown as a vertical bar for each country. As used by Christophe this wasn't great because it was difficult to compare the heights of the bars scattered all over the map (which was the point of his illustration). But mixing this up with an idea he'd mentioned earlier in his talk, I thought the bars could work if you could add a reference line or shape showing 50%, and also use the colour coding. Plus a participant asked for numbers added to the map, which we agreed could work so long as it didn't get cluttered.

I had some sleep-deprived time in an airport the morning after so thought I'd give a go to implementing this approach, and came up with this, which I think is pretty good!

<object type="image/svg+xml" data='/img/0245-wom-map.svg' width='100%'><img src='/img/0245-wom-map.png' width='100%'></object>

I did cheat a bit by knocking out the Middle East because the map got really cluttered over there. For many purposes Middle East is treated differently to Asia by various international organisations, so there is some justification in this. There's also an issue that my bars' heights are calculated in degrees latitude, and with the projection of the map onto flat screen that should mean that the bars in countries with higher latitudes will be shorter. But this doesn't seem to be a visual problem, because of the effectiveness of the pale blue 50% reference blocks.

There's some spots where the clutter stopped the numbers being readable in my first iteration - for North Korea and for Cambodia and Laos and a few others - and I had to code in some ad hoc adjustments for them. But overall the effect was pretty good, even before I made those corrections.

In terms of the substance here, not much to say. It really stands out how New Zealand, Australia and (perhaps more suprisingly to some) Timor Leste are the front runners with near-equity in gender representation in Parliament. Uzbekhistan, Nepal and Vietnam make up the second tier with 30% or higher. Pacific Island countries, Japan and South Asia (other than Nepal) stand out in the other direction.

Nepal and Timor Leste both have formal quota requirements for women in Parliament. Check out the [Global Database of Gender Quotas](https://www.idea.int/data-tools/data/gender-quotas) in Parliaments worldwide, maintained by International IDEA, the Inter-Parliamentary Union and Stockholm University.

Here's the code that makes the map, all in one chunk. It turned out to be pretty straightforward. The main trick is I had to pre-calculate coordinates for the heights of the 50% rectangle and the segments showing the actual height, rather than letting ggplot calculate it on the fly like it would for a bar chart.

*Post continues after R code*
{% highlight R lineanchors %}
library(tidyverse)
library(rnaturalearth)
library(sf)
library(WDI)
library(extrafont)
library(RColorBrewer)

#-----------------data prep--------------------

# Data on proportion of women in Parliament from World Development Indicators:
wom_raw <- WDI(indicator = "SG.GEN.PARL.ZS", start = 2010)

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
centers <- st_centroid(m, of_largest_polygon = TRUE) |>
  # turn them into straight numbers, not sf geometries:
  st_coordinates() |> 
  # add those coordinates as columns to the original map:
  cbind(m) |>
  # join to the women in parliament data:
  left_join(wom, by = c("iso_a3" = "iso3c")) |>
  rename(x = X, y = Y) |>
  mutate(x = case_when(
    # put Tonga and Samoa back on the map (the land won't show but the bars will)
    x < 0 ~ x + 360,
    # Put Kiribati where Tarawa is rather than its spatial centre:
    country == "Kiribati" ~ 173,
    TRUE  ~ x
  )) |>
  # a couple of manual adjustments to make things more readable, avoiding overlaps
  mutate(x = case_when(
    geounit == "North Korea" ~ x - 1.5,
    geounit == "Vietnam" ~ x + 1,
    geounit == "Laos" ~ x - 0.8,
    geounit == "Indonesia" ~ x - 1.8,
    geounit == "Malaysia" ~ 101.7, # location of Kuala Lumpur
    TRUE ~ x
  )) |>
  # create scaled latitude coordinates for our rectangles and segments:
  mutate(sc = 5,
         yend1 = y + wom_parl / sc,
         yend0 = y + 50 / sc) |>
  filter(!is.na(wom_parl)) |>
  # knock out the middle east because it's cluttered :):
  filter(x > 60)

#------------------map--------------------------

xw <- 1

the_font <- "Calibri"
wom_map <- ggplot(centers) +
  geom_sf(colour = "grey50") +
  geom_rect(aes(xmin = x - xw, xmax = x + xw, ymin = y, ymax = yend0),
            fill = "steelblue", alpha = 0.5) +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend1, colour = wom_parl),
               linewidth = 2) +
  geom_text(aes(label = round(wom_parl), x = x, y = yend1 + 1, colour = wom_parl), 
            vjust = 0, size = 3, family = the_font) +
  # It was useful while troubleshooting to add country names;
  # uncomment the below if you want them:
  # geom_text(aes(label = country, x = x, y = y)) +
  theme_void(base_family = the_font) +
  xlim(60, 200) +
  scale_colour_viridis_c(option = "B", limits = c(0, 60), 
                         label = number_format(suffix = "%")) +
  theme(legend.position = c(0.2, 0.35)) +
  labs( colour = "",
        title = "Percentage of parliamentarians who are women",
        subtitle = "Blue bars represent 50%")

print(wom_map)
{% endhighlight %}

## Alternative

I do like my map but there's no denying it's not an efficient way of transmitting the full information. Great for a sense of global spread, not so good for identifying individual countries (for example, it took me quite a while to realise the Timor Leste bar was Timor Leste, not Indonesia). If we want to compare countries using their names and the spatial element is secondary, we're better with a more straightforward representation like this one:

<object type="image/svg+xml" data='/img/0245-wom-dots.svg' width='100%'><img src='/img/0245-wom-dots.png' width='100%'></object>

Created with this code:

{% highlight R lineanchors %}
#----------------alternative visualisation-------------
p <- centers |>
  mutate(clps_income = case_when(
    # collapse OECD and non-OECD high income and upper middle together,
    # as they are quite small categories
    grepl("High income", income_grp) | grepl("Upper middle", income_grp) ~ "High or middle income",
    TRUE ~ gsub("[0-9]\\. ", "", income_grp)
  )) |>
  mutate(clps_income = fct_relevel(clps_income, "High or middle income", after = Inf)) |>
  mutate(country = fct_reorder(country, -wom_parl),
         subregion = fct_reorder(subregion, wom_parl)) |>
  ggplot(aes(x = wom_parl, y = country)) +
  geom_segment(aes(xend = 0, yend = country, colour = subregion), linewidth = 1.5) +
  geom_point() +
  geom_text(aes(label = round(wom_parl)), family = the_font, 
            size = 2.5, vjust = 0.5, hjust = 0, nudge_x = 1) +
  facet_wrap(~clps_income, scales = "free_y", ncol = 2) +
  # expand = c(0,0) is needed to put the country labels close to the plot
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 60),
                     label = number_format(suffix = "%")) +
  # exclude the bright yellow from Brewer Set1 colours, which I don't like:
  scale_colour_manual(values = brewer.pal(9, "Set1")[-6]) +
  theme_minimal(base_family = the_font) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y=element_text(margin=margin(r=0)),
        legend.position = c(0.8, 0.2)) +
  labs(y = "",
       x = "Proportion of Parliamentarians that are women",
       colour = "Sub-region",
       title = "Women parliamentarians in the Asia-Pacific")

print(p)
{% endhighlight %}

That's all for today.
