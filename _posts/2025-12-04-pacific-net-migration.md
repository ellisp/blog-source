---
layout: post
title: Net migration in Pacific island countries
date: 2025-12-04
tag: 
   - Demography
   - WorkRelated
   - Pacific
   - Visualisation
description: Accessing data and drawing visualisations of net migration for Pacific island countries and territories.
image: /img/0309-six-picts-natural-immmigration-line.svg
socialimage: https:/freerangestats.info/img/0309-six-picts-natural-immmigration-line.png
category: R
---

This is the second in my series of posts about population, migration, diaspora and related issues in the Pacific. See the [first post in the series](/blog/2025/11/30/pacific-population) for more background. Today I'm looking at two charts relating to net migration and its role in Pacific islands' population growth. These were used early in the presentation that these blog posts refer to, as part of building the story of the importance of people movements in understanding the Pacific.

These two plots are pretty straightforward. The data comes from the UN's 2024 population prospects, a data source I've used in several blog posts before. The particular dataset we want is the "standard indicators"&mdash;these include net migration, natural change (ie births minus deaths), and total population for each year from 1950 to 2050; which is what we're looking for for the charts today.

First downloading and wrangling the data. Pretty simple:

{% highlight R lineanchors %}
# draws charts of migration and natural change impact on growth
# The source for this is UN population projections. Some details
# on net migration aren't in the PDH.stat so we get them from
# the UN website.
#
# Peter Ellis November 2025

library(tidyverse)
library(readxl)
library(scales)

#-------------Download and get ready the data---------------

df2 <- "pp24_standard.xlsx"
if(!file.exists(df2)){
  unlink(df2)
  download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
                destfile = df2, mode = "wb")
}

un_indicators <- read_excel(df2, skip = 16) 

picts <- c(
  "Fiji", "New Caledonia", "Papua New Guinea", "Solomon Islands",                                             
  "Guam", "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "Nauru",
  "Vanuatu", "Northern Mariana Islands","Palau", "American Samoa", "Cook Islands",
  "French Polynesia", "Niue", "Samoa", "Tokelau", "Tonga", "Tuvalu", "Wallis and Futuna Islands" 
)

# check all PICTs spelled correctly and in data
stopifnot(all(picts %in% un_indicators$`Region, subregion, country or area *`))

pict_indicators <- un_indicators |> 
  rename(country = `Region, subregion, country or area *`) |> 
  filter(country %in% picts) |> 
  mutate(nnm = as.numeric(`Net Number of Migrants (thousands)`),
         nmr = as.numeric(`Net Migration Rate (per 1,000 population)`) ,
         nc = as.numeric(`Natural Change, Births minus Deaths (thousands)`),
         pc = as.numeric(`Population Change (thousands)`),
         pop1july = as.numeric(`Total Population, as of 1 July (thousands)`)) |> 
  mutate(country = gsub("States of", "States", country), 
         country = fct_reorder(country, abs(nmr)),
         migration_direction = ifelse(nmr <0, -1, 1))
{% endhighlight %}

In fact, this data 'wrangling' isn't much more than filtering the data to the Pacific island countries and territories we want; relabelling some columns for ease of use; and a bit of fiddling with names for appearance's sake in the coming graphs.

Here's the first graph I wanted. This is just the net migration as a line, but to make it more visually striking I've filled in the space between the line and the net-zero horizontal axis, and colour-coded that fill with red for negative, blue for positive. 

<object type="image/svg+xml" data='/img/0309-all-picts-net-migration.svg' width='100%'><img src='/img/0309-all-picts-net-migration.png' width='100%'></object>

The key substantive point from this chart is the dominance of the red&mdash;negative net migration for the Pacific, most of the time, most countries. There are interesting stories in the blue episodes. Some are probably data problems that might be changed later, others are related to periods of immigration relating to construction projects. Going into each country or territory's particular story here is beyond the scope of my talk or the blog, but the plot is a great start for anyone wanting to embark on that.

Controlling the fill like this was a little fiddly, and as you may have spotted in the previous chunk of code, required me to make a `migration_direction` variable. In the chunk below, this variable gets mapped to the fill aesthetic of the ribbon geom. 

I experimented with alternative ways of filling the ribbon, including a continuous variable (so eg darker blue for more positive, darker red for more negative) but they weren't as visually effective as the single red-blue contrast.

Here's the code to draw that plot:

{% highlight R lineanchors %}
pict_indicators |> 
  ggplot(aes(x = Year, y = nmr)) +
  facet_wrap(~country, ncol = 7, scales = "fixed") + 
  geom_line(colour = "grey50", linetype = 1, linewidth = 0.4) +
  geom_ribbon(aes(ymin = 0, ymax = nmr, fill = migration_direction), alpha = 0.5) +
  scale_y_continuous(label = comma_format(scale = 1)) +
  scale_fill_gradient2(low = "red", high = "blue") +
  labs(title = "Net migration impact on Pacific island countries and territories",
       subtitle = "Countries shown in sequence of least proportionately impacted to most",
       x = "",
       y = "Net migration per thousand population in residence")  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7, colour = "grey50"),
        panel.grid.minor = element_blank())
{% endhighlight %}

The second plot uses the same data in a different presentation. Because it's a bit more cluttered, I picked just six countries or territories to show:

<object type="image/svg+xml" data='/img/0309-six-picts-natural-immmigration-line.svg' width='100%'><img src='/img/0309-six-picts-natural-immmigration-line.png' width='100%'></object>

The solid blue line is the total population growth. The dashed green line is the 'natural' growth ie births minus deaths. The dotted red line is the net migration we were just looking at. You can think of the red line as dragging down  (or very occasionally, up) the green line to get you the solid blue line as the sum of the two. Or you can imagine, if there were no red line at all, the green line would be the total population growth.  This was a nice talking point and I think complements the first chart nicely. 

It's a pretty dramatic story when you think of the green dashed line being dragged down to the blue line by migration! Look at Niue, Marshall Islands or Samoa (migration hugely important) compared to Kiribati or Papua New Guinea (not so much).

The important design choice is to use a solid line for the overall total, and dashed and dotted lines for the two components of it. I think this makes it easier to follow, ie making the total the eye-catchingly solid line. A few secondary things to note: another reason to use dashes and dots in addition to the red and green contrast is because of the relatively high frequency of red-green colourblind people; and I also carefully make sure the legend shows the natural increase, total population, net migration in the same vertical sequence as they appear in the plots.

Here's the code for this simple chart:

{% highlight R lineanchors %}
selected <- c("Niue", "Marshall Islands", "Samoa", "Kiribati", "Northern Mariana Islands", "Papua New Guinea")

pict_indicators |> 
  filter(country %in% selected) |> 
  select(country, Year, `Net migration gain/loss` = nnm, `Natural increase` = nc, `Total increase` = pc) |> 
  gather(variable, value, -Year, -country) |> 
  mutate(variable = fct_relevel(variable, "Net migration gain/loss", after = Inf)) |> 
  ggplot(aes(x = Year, y = value, colour = variable, linetype = variable)) +
  facet_wrap(~country, ncol = 3, scales = "free_y") + 
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line() +
  scale_colour_manual(values = c("darkgreen", "blue", "red")) +
  scale_linetype_manual(values = c(2,1,3)) +
  labs(y = "Annual change in population (thousands)",
       x = "",
       colour = "", linetype = "")
{% endhighlight %}

That's all for now! Coming up in subsequent posts&mdash;diaspora, the world's largest Pacific cities, and remittances.
