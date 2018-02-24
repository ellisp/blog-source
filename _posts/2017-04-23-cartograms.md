---
layout: post
title: Cartograms of New Zealand census data
date: 2017-04-23
tag: 
   - NewZealand
   - Spatial
   - Shiny
   - R
description: Choropleth maps are useful ways of using fill colour to show densities, proportions and growth rates by political or economic boundaries, but can be visually problematic when large geographic areas represent few people, or small areas (ie cities) represent many.  One solution is a cartogram, and I have a go at using them to present New Zealand census data in this post and accompanying shiny app.
image: /img/0094-reg-cart-3.svg
socialimage: http://ellisp.github.io/img/0094-reg-cart-3.png
category: R
---

I've been trying to catch up with mapping functionality in R and its extended ecoverse, so might do a few posts on this in the next month or so.  First up (more or less by accident) is  [cartograms](https://en.wikipedia.org/wiki/Cartogram) - maps where some other numeric variable is substituted for land area, while still trying to preserve regions' basic shapes and relative locations.

Cartograms are particularly useful as a variant on choropleth maps (regions' fill colour specified based on a numeric variable) for socio-economic comparison.  Straight choropleth maps can give a misleading comparison between large sparsely populated areas and small densely populated areas (ie cities).  This is a problem for statistical maps of New Zealand, for example, with the Auckland City and Regional Council making up a quarter of the country's population but much less of its area.

In this post, I use illustrative data from the 2013 New Zealand Census by Statistics New Zealand.  Last year I took their "meshblock" census data set and re-shaped it into the [`nzcensus` R package](http://ellisp.github.io/blog/2016/08/04/nzcensus-gam-elastic-lm); only available from GitHub (as it's too large for CRAN).  In the process of preparing today's blog post I made some small additions to the variables in that package - numbers of dwellings, households and individuals, and cartogram outlines of New Zealand regions (and I hope to follow up with other spatial categories.)

## Rectangles

My first exploration into this was via the `recmap` R package, which does rectangular cartograms.  These are a simplified version that just needs to know the centres of regions, the numeric variable you want their size to be proportionate to, and aspect ratios for rectangles.  Here's the out-of-the box version, showing the proportion of individuals who are receiving unemployment benefits on census night 2013:

<img src='/img/0094-rect.svg' width = '100%'>

Note that the proportion of all people receiving benefits is *lower* than the unemployment rate because there is a different denominator.

This is an interesting start, but I don't find this that satisfactory for New Zealand's 16 regions; and the result is even less satisfactory for 67 Territorial Authorities.  The algorithm is quick, but the rectangles just don't cut it for today's high expectations of data visualisation; and the adjacency of various regions has been lost (most significantly, Wellington relegated to the bottom of the map, rather than the middle where it more intuitively belongs).  There are ways to over-ride both of these issues, but I'll put `recmap` aside for now - potentially but not immediately useful for my purposes.

Here's the code for those rectangles, plus some helper functions for colour palettes and legends I'll use throughout the post:

{% highlight R %}
# Install the latest version of the nzcensus package if not already installed:
devtools::install_github("ellisp/nzelect/pkg2")

# Install Ministry of Business, Innovation and Employment's NZ maps package,
# only needed for region_simpl, used to illustrate a "normal" map.
devtools::install_github("nz-mbie/mbiemaps-public/pkg")

library(recmap)
library(nzcensus)
library(tidyverse)
library(viridis)
library(mbiemaps) 

# Helper functions:
colour_scale <- function(x, palette = viridis(100)){
   levs <- round(x / max(x, na.rm = TRUE) * length(palette))
   return(as.character(palette[levs]))
}

make_legend <- function(x, palette = viridis(100), title = NULL, 
                        location = "right", 
                        multiplier = 100, digits = 1, ...){
   y <- seq(from= min(x), to = max(x), length.out = 5)
   levsy <- round(y / max(x, na.rm = TRUE) * length(palette))
   legend(location, legend = round(y * multiplier, digits), 
          pch = 15, col = palette[levsy], text.col = palette[levsy],
          bty = "n", title = title, ...)
   title(xlab = "Source: Statistics New Zealand Census 2013, in nzcensus R package",
         adj = 1, col.lab = "grey50", cex.lab = 0.8)
}

#=========rectangle cartogram==========
tmp <- with(filter(REGC2013, !grepl("Area Outside", REGC2013_N)), 
            data.frame(x = WGS84Longitude,
                       y = WGS84Latitude,
                       dx = 12,
                       dy = 8,
                       z = ResidentPop2013,
                       name = gsub(" Region", "", as.character(REGC2013_N)),
                       value = PropUnemploymentBenefit2013,
                       stringsAsFactors = FALSE)) %>%
   mutate(colour = colour_scale(value))

tmp %>%
   recmap() %>%
   plot(col.text = "grey10", col = tmp[, "colour"], border = "white")
   title(main = "Unemployment by region; regions sized by usual resident population")

make_legend(tmp$value, title = "Proportion of all individuals\non unemployment benefit",
            location = "left", cex = 0.8)
{% endhighlight %}

## ScapeToad and cartograms

Turning a shapefile into a cartogram is a serious job for serious GIS software, and there's no obvious way to do it from R.  However, the [ScapeToad](http://scapetoad.choros.ch/) software (written in Java) is freely available under a GPL license and is simple to operate.  I've half started a project for R and ScapeToad to interact but I doubt it will get far or be worthwhile.

ScapeToad imports layers from an ESRI format shapefile, and then reshapes the borders for you based on a numeric variable that is included as part of the polygon-level data in that shapefile.  R works adequately for reading and writing shapefiles, and of course is unparalleled for managing numeric data in general.  The cartogram calculation is easy to specify in ScapeToad but is computationally intensive (at the time of writing, my laptop has been whirring away at the Territorial Authority level for four hours...).  I've added [a step to the build for the `nzcensus` R package](https://github.com/ellisp/nzelect/blob/master/prep/create-cartograms.R) that saves a cartogram version of New Zealand's regional council boundaries, with size proportional to "usual resident population".  I hope to do the same for Territorial Authority, Area Unit and Mesh Block down the track, but for now only the regional council level is complete.  Because I've done the work in advance, there's no need to install ScapeToad to use the map in `nzcensus`.

Here's an example end result:

<img src='/img/0094-reg-cart.svg' width='100%'>

Auckland, and the north island in general, are distorted to appear much larger than their geographic region, whereas other regions (particularly the West Coast of the south island) are shrunk; yet the map remains recognisably New Zealand.  In contrast, in the traditional choropleth map on the right, the colour of Auckland is simply not prominent enough given its importance in socio-economic terms.

The R code to produce this from an existing cartogram set of boundaries (`reg_cart_simpl`, which now ships with `nzcensus`) is probably even easier than the rectangle version:

{% highlight R %}
#===============shape-preserving cartogram=============
comb_data <- reg_cart@data %>%
   left_join(REGC2013, by = c("Name" = "REGC2013_N")) 

par(font.main= 1, fg = "grey75", mfrow = c(1, 2))
plot(reg_cart,
	col = colour_scale(comb_data$PropUnemploymentBenefit2013))

title(main = "Unemployment by region; regions sized by usual resident population")

make_legend(comb_data$PropUnemploymentBenefit2013, 
		   title = "Proportion of all individuals\non unemployment benefit",
		   location = "left", cex = 0.8)

# compare with the standard regions map, from the mbiemaps package
data(region_simpl)
plot(region_simpl, col = colour_scale(comb_data$PropUnemploymentBenefit2013))
title(main = "Regions as they are shaped and sized geographically")
{% endhighlight %}

I was so pleased with how this looks a did a few variants:

<img src='/img/0094-reg-cart-2.svg' width='100%'>

<img src='/img/0094-reg-cart-3.svg' width='100%'>

Then I realised that better would be to let people choose a variable for colouring in the map themselves so I made this Shiny app:

<iframe width="950" height="700" src="https://ellisp.shinyapps.io/nzcensus-cartograms/" frameborder="0" scrolling="no"></iframe>

This also makes it easier to select a colour scheme (and its direction), and gives micro-interactivity like informative tooltips.  And thanks to the work of the Shiny and Leaflet developers isn't much harder than the first static graphics.  

The web app makes sense because most of the data in the `nzcensus` package has been transformed into percentages (of dwellings, of households, or of individuals), which minimises the effort needed in choice of scales and the like.  More polish (eg clearer what the denominator is for each variable) would be needed to turn this into something of professional standard, but what we've got here is ok for a Sunday afternoon project.

Also available:

- [Full screen version](https://ellisp.shinyapps.io/nzcensus-cartograms/)
- [Source code](https://github.com/ellisp/ellisp.github.io/tree/source/_working/0094-cartograms)