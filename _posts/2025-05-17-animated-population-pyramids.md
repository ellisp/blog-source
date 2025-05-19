---
layout: post
title: Animated population pyramids for the Pacific
date: 2025-05-17
tag: 
   - Demography
   - DataFromTheWeb
   - Pacific
   - Animations
   - OpenData
   - WorkRelated
description: How to produce an animation of demographic patterns in Pacific island countries and territories from 1950 to 2050, in just a few lines of code.
image: /img/0291-pac_pyramids.gif
socialimage: https:/freerangestats.info/img/0291-pac_pyramids.gif
category: R
---

A short blog post today which is just all about producing this animation, which I used for a work presentation yesterday:

<img src="/img/0291-pac_pyramids.gif" width="100%" alt="Animated population pyramids of the Pacific islands from 1950 to 2050">

It's super easy and it's worth putting out there how to do it, if only to encourage people to think more about demography in the Pacific. And animated population pyramids are cool.

Regarding the substance, it's a familiar pattern for those in the field. The populations start at small, fast growing and heavy in young people; and over time become more evenly spread over age groups and indeed in some cases positively elderly. There are a few interesting quirks such as the very high number of males in Guam early in the period, reflecting the predominance of the US military in the population numbers at that time. Some smaller countries with read outwards immigration options, like Niue, have been shrinking right from the very beginning of the period.

It's a simple three step process. First, we download the data from the [Pacific Data Hub "PDH.stat"](https://stats.pacificdata.org/), and do some formatting and reshaping to make it easy to draw the chart with the right country names and population totals in the facet labels:

{% highlight R lineanchors %}
library(tidyverse)
library(rsdmx)
library(scales)
library(janitor)
library(ISOcodes)
library(glue)
require(spcstyle)

#============Animated population pyramids=====================

if(!exists("proj_raw")){
  # This is quite slow - several minutes - but the slow part is apparently parsing
  # the XML in the as_tibble
  proj_raw <- readSDMX(providerId = "PDH", 
                       resource = "data", 
                       flowRef = "DF_POP_PROJ")  |>
    as_tibble() |>
    clean_names()
}

#' Format a number as millions or thousands
format_num <- function(x){
  y <- dplyr::case_when(
    # had to use round explicitly because of something funny happening later with pops' creation:
    x > 1e6 ~ paste0(format(round(x / 1e6, digits = 1), nsmall = 1, scientific = FALSE), "m"),
    x > 1e3 ~ paste0(format(round(x / 1e3, digits = 1), nsmall = 1, scientific = FALSE), "k"),
    TRUE    ~ as.character(round(x))
  )
  return(str_squish(y))
}

# test that function works as expected
stopifnot(format_num(1234567.1234) == "1.2m")
stopifnot(format_num(1234.1234) == "1.2k")
stopifnot(format_num(12) == "12")

# create a pops data frame, complete with country names that include
# the population in that year
pops <- proj_raw |>
  # eliminate the totals we only want male, female, and particular age groups:
  filter(sex != "_T" & age != "_T") |>
  # eliminate the subregional groupings - we only want countries / territories
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC")) |>
  # choose the right indicator
  filter(indicator == "MIDYEARPOPEST") |>
  # fiddle a bit with the age categories to make sure we have them correctly
  # in order:
  mutate(age = gsub("^Y", "", age)) |>
  separate(age, into = c("from", "to"), sep = "T", remove = FALSE) |>
  mutate(age = gsub("T", "-", age),
         age = gsub("-999", "+", age, fixed = TRUE),
         sex = case_when(
           sex == "M" ~ "Male",
           sex == "F" ~ "Female"
         )) |>
  mutate(age = factor(age),
         sex = fct_relevel(sex, "Male"))|>
  # join the geo_pict country codes to the full country names:
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  # calculate total population in any given year, for displaying in facet label:
  group_by(geo_pict, obs_time) |> 
  mutate(total_pop = sum(obs_value)) |> 
  # population in 2024 which we'll use for ordering the facets in the chart
  # (we want the ordering to be stable over time):
  group_by(geo_pict) |> 
  mutate(total_pop_2024 = sum(obs_value[obs_time == 2024])) |> 
  ungroup() |> 
  # Fiddle a bit with the country names so they fit nicely
  mutate(short_name = gsub("Federated States of", "Fed Sts", Name),
         short_name = gsub("Mariana Islands", "Marianas", short_name)) |> 
  mutate(pict = glue("{short_name}: {format_num(total_pop)}")) |> 
  mutate(pict = fct_reorder(pict, total_pop_2024))
{% endhighlight %}

Second, we draw 101 plots, one for each year, and save them as images in a temporary folder. I like to do this explicitly (saving each frame of the animation in a loop) as I find it generally easier to troubleshoot this way:

{% highlight R lineanchors %}
#-----------------------Draw plot--------------------
# see https://blog.datawrapper.de/gendercolor/
pal <- c("#D4855A", "#C5CB81")
names(pal) <- c("Female", "Male")

# Reverse order so Male appears on left in legend:
pal <- pal[2:1]

ff <- "Calibri"

dir.create("tmp_pyramids", showWarnings = FALSE)

for(y in 1950:2050){

  p1 <- ggplot(filter(pops, obs_time == y), aes(y = age, fill = sex)) +
    facet_wrap(~pict, scales = "free_x", ncol = 7) +
    geom_col(data = filter(pops, sex == "Male" & obs_time == y), 
             aes(x = -obs_value)) +
    geom_col(data = filter(pops, sex == "Female" & obs_time == y), 
             aes(x = obs_value)) +
    scale_fill_manual(values = pal) +
    scale_x_continuous(label = comma) +
    theme_void(base_family = ff) +
    theme(axis.text.y = element_text(hjust = 1, size = 6),
          axis.title.x = element_text(),
          legend.position = "top",
          plot.caption = element_text(hjust = 0.5, colour = "grey20"),
          panel.background = element_rect(fill = "grey95", colour = NA),
          plot.margin = unit(c(3,3,3,3), "mm")) +
    labs(title = glue("Population estimates and projections in {y}"),
         subtitle = "Pacific Island Country and Territory members of the Pacific Community",
         x = "Number of people",
         fill = "",
         caption = "Source: UN Population Projections in the Pacific Data Hub")
  
  sc <- 5
  png(glue("tmp_pyramids/{y}.png"), 
      width = 7000 / sc, height = 4000 / sc, res = 600 / sc, type = "cairo-png")
  print(p1)
  dev.off()

}
{% endhighlight %}

Finally, we use [ImageMagick](https://imagemagick.org/index.php) to convert those 101 PNG images into a single GIF. This is a single line of code on the command line; if we want to do it without leaving R it is easy enough:

{% highlight R lineanchors %}
# next step requires imagemagick to be installed. Takes about 30 seconds.
wd <- setwd("tmp_pyramids")
system('magick -loop -50 -delay 10 *.png "0291-pac_pyramids.gif"')
setwd(wd) # go back to original working directory
{% endhighlight %}

And that's it! 

The source for these population projections (at least right now) is the UN Population Prospects. It would be easy to adapt this code to work with any other combination of countries, of course. I've just used the version in PDH.stat because I wanted to highlight that for work, and it made it easier for me to get just the countries and age groups I needed for a presentation that was put together in a bit of a rush.
