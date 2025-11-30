---
layout: post
title: Visual summaries of population in Pacific islands 
date: 2025-11-30
tag: 
   - Demography
   - WorkRelated
   - Pacific
   - DataVisualization
description: Accessing population data for the Pacific and drawing two visual summaries of its recent and projected growth and absolute size, as used recently in a side event before the Pacific Heads of Planning and Statistics meeting in Wellington.
image: /img/0305-population-scatter-highlighted.svg
socialimage: https:/freerangestats.info/img/0305-population-scatter-highlighted.png
category: R
---

This will be the first of several posts where I post some code and visualisations of population issues in the Pacific. The analysis and visualisations are pretty simple. Between them, they'll show how to make (with publicly available data) all the statistical images used in a presentation I recently gave in Wellington on [migration and mobility in the Pacific](https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/6a/6ad1c5b68b49b1462b9eb172a056c46c.pdf?sv=2015-12-11&sr=b&sig=TNWwFi4l8um7rBJavVHx41%2BhJvT5AI4PQ7OvWAoP1e0%3D&se=2026-05-27T18%3A44%3A49Z&sp=r&rscc=public%2C%20max-age%3D864000%2C%20max-stale%3D86400&rsct=application%2Fpdf&rscd=inline%3B%20filename%3D%22HOPS7_Opening_Day_Seminar_Pacific_priorities_and_change___Population_mobility_priorities_and_trends.pdf%22). 

This was for a side event before the Pacific "Heads of Planning and Statistics" meeting, which takes place every two years and is the biggest event my team at the Pacific Community (SPC) organises. [All the papers and presentations considered the meeting are available online](https://sdd.spc.int/events/2025/11/7th-regional-conference-heads-planning-and-statistics-hops-7), which is definitely transparency in action. 

It was fun at this side event to have the chance for once to talk about the substantive issues the data shows, rather than (as is the usual focus of my meetings) how to improve the data, improve its use, and generally strategise and prioritise to improve statistics. These things are important and (arguably) fun too, but it's nice to put them aside and talk about some actual development issues now and then. My talk was followed by a great panel discussion with speakers from academia, a UN organisation, Stats NZ and a Pacific island national planner.

Today's post is pretty straightforward and is just about producing two statistical charts (one of them with both a "bare" and a "highlighted" version), setting the scene for population in the Pacific.

### Downloading data

First, I download and tidy up the data. Everything I need for these charts is already in the Pacific Data Hub, making this pretty straightforward. The thing that takes a bit of fiddling is converting the country codes to user-friendly country names; and classifying each country into one of Melanesia, Polynesia or Micronesia.

{% highlight R lineanchors %}
# This script produces a couple of general use plots on population growth in the Pacific
# for use in presentations on data issues

library(tidyverse)
library(rsdmx)
library(scales)
library(janitor)
library(ISOcodes)
library(glue)
library(spcstyle)
library(extrafont)
library(Cairo)
library(ggrepel)

# general use caption and font:
the_caption <- "Source: UN World Population Prospects, via the Pacific Data Hub"
the_font <- "Roboto" 

# Download all the mid year population estimates from PDH.stat
d <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A.AS+CK+FJ+PF+GU+KI+MH+FM+NR+NC+NU+MP+PW+PG+PN+WS+SB+TK+TO+TV+VU+WF+_T+MEL+MIC+POL+_TXPNG+MELXPNG.MIDYEARPOPEST._T._T?startPeriod=1950&endPeriod=2050&dimensionAtObservation=AllDimensions") |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(time_period = as.numeric(time_period))

# Some subregional classifications.
mel <- c("Melanesia", "Papua New Guinea", "Fiji", "Solomon Islands", "Vanuatu", "New Caledonia")
pol <- c("Polynesia", "Tonga", "Samoa", "Cook Islands", "Tuvalu", "American Samoa", "Pitcairn", "Wallis and Futuna", "French Polynesia", "Niue", "Tokelau")

# lookup table with country codes, names, and which subregion they are in
pict_names <- tribble(~Alpha_2, ~Name,
                      "_T", "All PICTs",
                      "MEL", "Melanesia",
                      "_TXPNG", "Total excluding PNG",
                      "POL", "Polynesia",
                      "MIC", "Micronesia")  |> 
  bind_rows(select(ISO_3166_1, Alpha_2, Name)) |> 
  rename(geo_pict = Alpha_2,
         pict = Name) |> 
  mutate(region = case_when(
    pict %in% mel ~ "Melanesia",
    pict %in% pol ~ "Polynesia",
    grepl("^_T", geo_pict) ~ "Total",
    TRUE ~ "Micronesia"
  ))

# Dataset that combines the original PDH.stat data with the country names and regional classifications
d2 <- d |> 
  mutate(era = ifelse(time_period <= 2025, "Past", "Future")) |> 
  inner_join(pict_names, by = "geo_pict") |> 
  mutate(pict = gsub("Federated States of", "Fed. States", pict)) |> 
  # Order country names from smallest to largest population in 2050:
  mutate(pict = fct_reorder(pict, obs_value, .fun = last)) 
{% endhighlight %}

### Line plot

This puts us in a position to just draw our first plot:

<object type="image/svg+xml" data='/img/0305-population-line.svg' width='100%'><img src='/img/0305-population-line.png' width='100%'></object>

It's very intuitive, and I think a necessary introduction to all the countries and territories we're talking about. When we first made a version of this plot I thought it would never be neat enough to use in a presentation, but in fact it works ok on a big conference screen, so long as we exclude (as I have) the various regional and sub-regional totals.

All the hard work to produce this plot had been done earlier in the data management, so producing the plot is just a single chunk of code:

{% highlight R lineanchors %}
#----------------------time series line plot-------------

# This version just has 21 individual PICTs, no subregional totals. 21 fits
# ok on the screen in 3 rows of 7:
d2 |> 
  # remove subregional and regional totals, so only actual countries
  filter(!(pict %in% c("Micronesia", "Polynesia", "Melanesia") | 
             grepl("total", pict, ignore.case = TRUE) | 
             pict %in% c("All PICTs", "Pitcairn"))) |> 
  ggplot(aes(x = time_period, y = obs_value, colour = era)) +
  facet_wrap(~pict, scales = "free_y", ncol = 7) +
  geom_line() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'plain'),
        plot.caption = element_text(colour = "grey50")) +
  scale_y_continuous(label = comma) +
  scale_colour_manual(values = spc_cols(c(4, 2))) +
  # force all y axes to go to zero (but because free_y in the facet_wrap call, 
  # they will be on different scales for readability):
  expand_limits(y = 0) +
  labs(x = "", y = "",
       title = "Population in the Pacific, 1950 to 2050",
       subtitle = "Countries listed in sequence of projected population in 2050",
       caption = the_caption) 
{% endhighlight %}

### Scatter plot

The line plot's a nice introduction to population and most importantly, it's easily understood. But unless people look carefully at the vertical axis labels it gives no sense of the absolute size of the different countries, and only a very rough visual sense of the differing growth rates. 

In looking for a single image that would summarise two things I came up with this chart:

<object type="image/svg+xml" data='/img/0305-population-scatter.svg' width='100%'><img src='/img/0305-population-scatter.png' width='100%'></object>

This is something we'd prepared earlier well before this talk and not yet needed to use, but it was for exactly this sort of use case&mdash;a single slide summary of Pacific island countries and territories' absolute size and growth rates. 

It takes a little bit of explanation for, and concentration from, an audience&mdash;in particular, explaining why the negative growth area is shaded and what that means. The logarithmic scale for population size means people probably won't realise just how overwhelmingly big Papua New Guinea is compared to the rest of the Pacific; to show that properly, we really need a different chart. But overall, this is straightforward enough for people to grasp.

What I like about this plot is that it makes clear the two broad categories of Pacific island countries and territories in population terms: relatively large (meaning >100,000 people!) and growing, which is all of Melanesia and a few others; and small and shrinking, comprising most of Polynesia and parts of Micronesia. Tonga, population estimated around 104,000, is the borderline case&mdash;all the countries larger than Tonga are growing in population terms; and nearly all those smaller than it are shrinking.

There's two territories I dropped from this plot because the UN 2024 population projections, which is the data used, are materially out of date and I didn't want to get side-tracked into explaining why in the talk. We'll be able to include them in future versions of the pot hopefully soon.

Again, it was pretty simple to create the plot with the data we've already got. Here's the R code to do that:

{% highlight R lineanchors %}
#----------------scatter plot comparing growth to totals---------------
# Summary data as one row per country for use in scatter plot
d3 <- d2 |> 
  group_by(pict, region) |> 
  summarise(pop2025 = obs_value[time_period == 2025],
            pop2020 = obs_value[time_period == 2020]) |> 
  mutate(cagr = (pop2025 / pop2020) ^ (1/5) - 1) |> 
  mutate(point_type = if_else(pict %in% c("Micronesia", "Polynesia", "Melanesia") | region == "Total", "total_like", "country"),
         # font type has to use identity scale, no scale to map it        
         font_type = ifelse(point_type == "total_like", 4, 1),
         # couldnt' get Melanesia in the right spot with ggrepel so have to make a specific adjustment for it:
         adjusted_x = ifelse(pict == "Melanesia", pop2025 * 1.35, pop2025))

# For a presentation used at HOPS7, I want
# 1) scatter plot but without the region and subregions, to avoid clutter
# 2) as 1 but with the shared sovereign countries highlighted eg with a circle around them.
#
# I also excldued two countries that had conspicuously out-of-date data that I didn't
# want visually prominent.

d4 <- d3 |> 
  filter(point_type == "country") |> 
  # two countries/territories have materially wrong estimates that
  # are distracting, better to just drop them from the chart
  filter(!pict %in% c("Tokelau", "Micronesia, Fed. States"))

p2b <- d4 |> 
  ggplot(aes(x = pop2025, y = cagr, colour = region)) +
  # Draw a pale (transparent, alpha) background rectangle for the negative growth countries:
  annotate("rect", xmin = 30, xmax = Inf, ymin = 0, ymax = -Inf, alpha = 0.1, fill = "red") +
  # Largish points for each country:
  geom_point(size = 2.5) +
  # labels for each country:
  # geom_label_repel(aes(label = pict), seed = 7, family = the_font, size = 2.7, label.size = 0, fill = "transparent") +
  geom_text_repel(aes(x = adjusted_x, label = pict, fontface = font_type), seed = 6, family = the_font, size = 2.7) +
  # For the smaller countries, use actual populations as the points for markers on the axis.
  # For larger than 10,000, there are too many countries and it would be cluttered, so use 3, 10, 30, 100, etc.
  scale_x_log10(label = comma, 
                breaks = signif(c(sort(unique(d3$pop2025))[c(1:4, 8, 9, 12, 23:25)], 3e5), 3)) +
  scale_y_continuous(label = percent) +
  # Use SPC colours for the four subregion types:
  scale_colour_manual(values = c("Micronesia" = spc_cols(1), "Polynesia" = spc_cols(3), "Melanesia" = spc_cols(4), "Total" = "grey50")) +
  # Readable x axis tick marks (at an angle); and not too many vertical gridlines:
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "grey50")) +
  # labels for the axes, plot title, legend:
  labs(x = "Population in 2025 (logarithmic scale)",
       y = "Compound annual population growth rate 2020 to 2025",
       colour = "",
       title = "Current population and recent growth in the Pacific",
       subtitle = "Populations of the Pacific Island country and territory members of the Pacific Community (SPC). 
",
       caption = the_caption)
{% endhighlight %}

There's a few tricks used here, most important of which is probably the way I've used the exact population sizes as horizontal axis labels. This is something that works well with a small number of points, and which I learned from a Tufte book.

### Scatter plot with highlights

Finally for today, I wanted a version of the same plot that highlighted the countries that have easy mobility to a larger a richer country&mdash;that is, France (three territories), the USA (three territories and three self-governing countries), New Zealand (three members of the "Realm of New Zealand") or the UK (Pitcairn). One of the themes of my talk was the way that in countries where people *can* move, a certain number of them generally *do*. This is a very politically and culturally sensitive point, and it's not one I'm going to try to explore the reasons for here, but we can certainly note it as a dominant fact of importance for understanding the demographic dynamics of the Pacific. It's one of two or three critical big picture points that explain many of the differences between Kiribati (very densely populated on Tarawa and relatively poor) and Marshall Islands (less obvious excessive population density, higher standard of living), for example.

My plot with the highlights&mdash;which are just oversized point geoms using shape number 1, a hollow circle&mdash;shows this nicely I believe:

<object type="image/svg+xml" data='/img/0305-population-scatter-highlighted.svg' width='100%'><img src='/img/0305-population-scatter-highlighted.png' width='100%'></object>

And here is the code for that plot:

{% highlight R lineanchors %}
easy_mobility <- c("Pitcairn", 
                   "Niue", "Tokelau", "Cook Islands", 
                   "Wallis and Futuna", "New Caledonia", "French Polynesia",
                   "Guam", "Northern Mariana Islands", "American Samoa",
                   "Marshall Islands", "Palau", "Micronesia, Fed. States")

# check all are in data apart from the two we deliberately dropped
stopifnot(sum(!easy_mobility %in% d4$pict) == 2)

d4 |> 
  ggplot(aes(x = pop2025, y = cagr, colour = region)) +
  # Draw a pale (transparent, alpha) background rectangle for the negative growth countries:
  annotate("rect", xmin = 30, xmax = Inf, ymin = 0, ymax = -Inf, alpha = 0.1, fill = "red") +
  # Largish points for each country:
  geom_point(size = 2.5, alpha = 0.5) +
  # labels for each country:
  # geom_label_repel(aes(label = pict), seed = 7, family = the_font, size = 2.7, label.size = 0, fill = "transparent") +
  geom_text_repel(aes(x = adjusted_x, label = pict, fontface = font_type), seed = 6, family = the_font, size = 2.7) +
  # For the smaller countries, use actual populations as the points for markers on the axis.
  # For larger than 10,000, there are too many countries and it would be cluttered, so use 3, 10, 30, 100, etc.
  geom_point(data = filter(d4, pict %in% easy_mobility), size = 6, shape = 1, colour = "black") +
  scale_x_log10(label = comma, 
                breaks = signif(c(sort(unique(d3$pop2025))[c(1:4, 8, 9, 12, 23:25)], 3e5), 3)) +
  scale_y_continuous(label = percent) +
  # Use SPC colours for the four subregion types:
  scale_colour_manual(values = c("Micronesia" = spc_cols(1), "Polynesia" = spc_cols(3), "Melanesia" = spc_cols(4), "Total" = "grey50")) +
  theme_minimal(base_family = the_font) +
  # Readable x axis tick marks (at an angle); and not too many vertical gridlines:
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "grey50")) +
  # labels for the axes, plot tile, legend:
  labs(x = "Population in 2025 (logarithmic scale)",
       y = "Compound annual population growth rate 2020 to 2025",
       colour = "",
       title = "Current population and recent growth in the Pacific",
       subtitle = "Populations of the Pacific Island country and territory members of the Pacific Community (SPC). 
Countries and territories with easy migration access to a larger country are highlighted.",
       caption = the_caption)
{% endhighlight %}

That's all for today. In subsequent blogs I'll show how I drew the other charts in the original presentation, with net migration, diaspora sizes, Pacific Islander populations in various world cities, and remittances.
