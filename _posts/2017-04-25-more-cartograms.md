---
layout: post
title: More cartograms of New Zealand census data (district and city level)!
date: 2017-04-25
tag: 
   - NewZealand
   - Spatial
   - Shiny
   - R
description: Shapefiles for cartogram by New Zealand Territorial Authority (ie District or City), with area proportional to population in 2013, have been added to the nzcensus package on GitHub.
image: /img/0094-ta1.svg
socialimage: http://ellisp.github.io/img/0094-ta1.png
category: R
---
Just a short note to say that I've finished creating an experimental map of New Zealand by the 66 Territorial Authorities (districts and cities), with area expanded or shrunk to be proportional to population at the 2013 census.  This is in addition to the 16 Regional Council divisions I blogged about a couple of days ago.  It's available in the [`nzcensus` R package](https://github.com/ellisp/nzelect).

So some static plots: 

<img src='/img/0094-ta1.svg' width = '100%'>

<img src='/img/0094-ta2.svg' width = '100%'>

And an enhanced Shiny web-app:

<iframe width="950" height="700" src="https://ellisp.shinyapps.io/nzcensus-cartograms/" frameborder="0" scrolling="no"></iframe>

Code for producing the static images:
{% highlight R %}
# Latest version of nzcensus package:
devtools::install_github("ellisp/nzelect/pkg2")

library(nzcensus)
library(tidyverse)
library(viridis)

# we need make_legend() and colour_scale() as defined in
# http://ellisp.github.io/blog/2017/04/23/cartograms

comb_data_ta <- ta_cart@data %>%
   left_join(TA2013, by = c("Name" = "TA2013_NAM")) %>%
   as_tibble()

par(font.main= 1, fg = "grey75")
plot(ta_cart,
     col = colour_scale(comb_data_ta$PropNoQualification2013))
title(main = "People with no qualification; areas sized by usual resident population")
make_legend(comb_data_ta$PropNoQualification2013, 
            title = "Percentage of all individuals\nwith no qualification",
            location = "left", cex = 0.8)

par(font.main= 1, fg = "grey75")
plot(ta_cart,
     col = colour_scale(comb_data_ta$PropLabourers2013))
title(main = "Labourers as a percentage of those with occupation;\nareas sized by usual resident population")
make_legend(comb_data_ta$PropLabourers2013, 
            title = "Percentage of all individuals\nwho are labourers",
            location = "left", cex = 0.8)
{% endhighlight %}

Also available for the Shiny app:

- [Full screen version](https://ellisp.shinyapps.io/nzcensus-cartograms/)
- [Source code](https://github.com/ellisp/ellisp.github.io/tree/source/_working/0094-cartograms)