---
layout: post
title: Simpler drawing of Pacific choropleth maps
date: 2023-06-17
tag: 
   - Spatial
   - DataVisualization
   - Animations
   - WorkRelated
   - Pacific
description: I demonstrate the function I use to make it simpler to draw choropleth maps based on Pacific Island countries' and territories' exclusive economic zones.
image: /img/0249-hdi-map.svg
socialimage: https:/freerangestats.info/img/0249-hdi-map.png
category: R
---

## Simple use of the `draw_pac_map()` function

Last year [I blogged about making a choropleth map of the Pacific](/blog/2022/10/13/pacific-map), dealing with a few technical details like getting the map centred on the Pacific, using exclusive economic zones (EEZs) to colour things in for good visibility, adding the international dateline, etc.

Soon after that blog post I abstracted that work and added it to the `frs` R package, my miscellaneous collection of bits and pieces associated with this Free Range Statistics blog. I reasoned that as I will be in the Pacific for at least a few years I am likely to want to keep drawing these maps, and they might be useful at work too. You can see: 
- [the code for the function itself](https://github.com/ellisp/frs-r-package/blob/master/pkg/R/draw_pac_map.R); and
- [preparation of the various data objects called by the function](https://github.com/ellisp/frs-r-package/blob/master/processing/prep-pac-map.R). 

It's easy to install with `remotes::install_github("ellisp/frs-r-package/pkg")`.

Here's the minimal demo of the function. By default it will draw all the countries' EEZs grey:

*Post continues after R code*
{% highlight R lineanchors %}
library(tidyverse)
library(frs) # needs to be version 0.8.2 or higher
p <- draw_pac_map(country_label_size = 3)
print(p)

{% endhighlight %}


<object type="image/svg+xml" data='/img/0249-blank-map.svg' width='90%'><img src='/img/0249-blank-map.png' width='90%'></object>

For a more realistic use case, here's a map showing the [Human Development Index](https://hdr.undp.org/data-center/human-development-index#/indicies/HDI) or HDI for 2021, for all Pacific countries that have one:

<object type="image/svg+xml" data='/img/0249-hdi-map.svg' width='90%'><img src='/img/0249-blank-map.png' width='90%'></object>

You can see that the HDI, which is produced by the UNDP, only is calculated for countries that are full UN members; so members of the Realm of New Zealand (Niue, Tokelau and Cook Islands) or French overseas territories (New Caledonia, French Polynesia and Wallis and Futuna) for example aren't included.

On the other hand, Nauru *is* a full member of the UN, but it is one of four such countries that lack an HDI score because it is missing data on one of the components - in Nauru's case, mean years of education. The other countries missing an HDI score for similar reasons are North Korea, Monaco and Somalia.

Here's the code that downloads that HDI data and draws the map:

*Post continues after R code*
{% highlight R lineanchors %}
# Human Development Index
library(readxl)
library(ISOcodes)
download.file("https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Statistical_Annex_HDI_Table.xlsx",
              destfile = "hdi.xlsx", mode = "wb")

hdi <- read_excel("hdi.xlsx", skip = 7, sheet = "Table 1") |>
  rename(Name = `VERY HIGH HUMAN DEVELOPMENT`,
         hdi2021 = ...3) |>
  select(Name, hdi2021) |>
  mutate(hdi2021 = as.numeric(hdi2021)) |>
  filter(!is.na(hdi2021)) |>
  left_join(select(ISO_3166_1, iso3 = Alpha_3, Name), by = "Name")

# check to see if any failed to match names
#View(filter(d, is.na(iso3)))

p2 <- draw_pac_map(fill_df = hdi, join_col = "iso3", fill_col = "hdi2021",
                   fill_col_label = "Human Development\nIndex 2021",
                   family = "Calibri",
                   country_label_col = "white") +
  scale_fill_viridis_c(option = "A")
print(p2)
{% endhighlight %}

You can see that it's non trivial to get a good combination of colours that works complete with the country labels. There are options for most of the things you'd want to change in the initial call to `draw_pac_map` itself; and because it outputs a `ggplot2` object you can modify it by adding scales, labels, themes etc the usual way (as I do in the above by adding `scale_fill_viridis_c()` to set the scale of colours to use for the fill aesthetic).

Mental note to self - it would be entirely possible to use the data on SDGs 3, 4.3, 4.4 and 8.5 we have in the Pacific Data Hub to create estimates of HDI for at least some of the territories that the UNDP doesn't calculate it for. But that's enough of a job to leave for another time.

## Combining with Pacific Data Hub data for an animation of SDGs

OK, here's a more complex use case. I wanted to show off the Pacific Data Hub at a presentation last year to attendees of the Pacific Community's Committee of Representatives of Governments and Administrations - effectively the board of the Pacific Community or SPC, where I work. I decided it would be nice to do this while also showing that there is a lot of work to be done to improve official statistics production in the region. So I opted to use a sequence of maps, pulling data from the Pacific Data Hub for all the Sustainable Development Goal (SDG) indicators that the Pacific had agreed were priorities to measure. I limited it to those that made sense to represent on a one-dimensional choropleth map (e.g. where a single number for each country is a meaningful thing to compare).

So the result was this GIF, which basically becomes eye candy in the background while I talked about the issues:

<img src='/img/0249-pac-maps-sdgs.gif' width = '100%'>

I'm going to present all the code that does this animation in one chunk rather than try to explain it line by line. Hopefully it makes sense as-is. The key, like any animation, is to save one frame at a time; then I use ImageMagick to knit them all up into an actual animation. You'll see I opted to not clutter up this particular set of maps with country labels.

{% highlight R lineanchors %}
#==================animated map===================
#-----------------------Get the SDG data and metadata---------------
library(rsdmx)
library(janitor)
library(glue)

dataflow <- "DF_SDG"

# metadata ie code lists. Warning this next function, in frs, is a bit of a hack
# and is subject to change!
d_code_list <- get_pdh_codelists(dataflow = dataflow, version = "3.0")

# data
d_raw <- readSDMX(providerId = "PDH", resource = "data", flowRef = dataflow) |> 
  as_tibble()

# join the data to the code list so we have labels
d <- d_raw |>
  left_join(filter(d_code_list, category_en == "Codelist for SDG indicators"),
            by = c("INDICATOR" = "id")) |>
  select(-category_en, -category_fr) |>
  rename(indicator_full = name) |>
  clean_names() 

# for simplicity, limit ourselves just to indicators that are totals eg all
# age groups, not individual age groups, not disaagregated by sex, etc.
# this loses a lot of data of course but is ok because it is just for illustration
# to coose the indicators most suitable for choropleth matps
d_totals <- d |>
  filter(
    sex == "_T" &
      age == "_T" &
      urbanization == "_T" &
      income == "_T" &
      education == "_T" &
      occupation == "_T" &
      composite_breakdown == "_Z"  &
      disability == "_T"
  ) |>
  group_by(indicator_full, geo_pict) |>
  arrange(desc(obs_time)) |>
  slice(1) |>
  ungroup()

indicator_summary <- d_totals |>
  group_by(indicator, indicator_full) |>
  summarise(countries = length(unique(geo_pict)),
            years = length(unique(obs_time)),
            n = n()) |>
  arrange(desc(countries))

# what indicators are we going to use, in a simple vector
inds <- pull(indicator_summary, indicator_full)

#--------------------draw the maps------------------

dir.create("pac-maps", showWarnings = FALSE)

for(i in 1:length(inds)){
  the_ind <- inds[i]
  the_data <- filter(d_totals, indicator_full == the_ind) 
  
  units <- the_data |>
    count(unit_measure, sort = TRUE) |>
    pull(unit_measure)
  
  if(length(units) > 1){
    warning("More than 1 unit measure, choosing the most common")
    the_data <- filter(the_data, unit_measure == units[1])
  }
  
  unit_title <- stringr::str_to_title(gsub("_", " ", units[1])) |>
    str_replace("Usd", "USD") |>
    str_replace("Ha ", "Hectares ") |>
    str_replace("Ha$", "Hectares") |>
    str_replace("Bool$", "Yes (1)/No (0)") |>
    str_replace("Km", "Kilometres") 
  
  m <- draw_pac_map(the_data, 
                    family = "Calibri",
                    base_size = 14,
                    country_labels = FALSE,
                    fill_col = "obs_value",
                    fill_col_label = unit_title,
                    ylim = c(-40, 30)
  )  +
    scale_fill_viridis_c(label = comma) +
    labs(title = the_ind,
         subtitle = paste(sort(unique(the_data$obs_time)), collapse = ", ")) +
    theme(axis.text = element_blank())
  
  png(glue("pac-maps/{i + 1000}-{indicator_summary[i, ]$indicator}.png"), 1500, 900, res = 150, type = "cairo-png")
  print(m)
  dev.off()
  
}

# Convert all the single frames into a GIF.
# Requires ImageMagick to be installed. Can uncomment and run it here or do 
# it directly in a system / shell window
# projdir <- setwd("pac-maps")
# system('magick -loop 0 -delay 150 *.png "pac-maps-sdgs.gif"')
# setwd(projdir)
{% endhighlight %}

That's it! See you next time.
