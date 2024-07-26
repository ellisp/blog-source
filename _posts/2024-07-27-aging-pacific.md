---
layout: post
title: Population age changes in the Pacific
date: 2024-07-27
tag: 
   - Pacific
   - WorkRelated
   - Demography
description: I polish up some visualisations of demographic trends in the Pacific.
image: /img/0271-aging-linechart.svg
socialimage: https:/freerangestats.info/img/0271-aging-linechart.png
category: R
---

The UN has released the [2024 Revision of World Population Prospects](https://population.un.org/wpp/) and I wanted to see the latest estimates of how Pacific Island Country and Territories (PICTs) populations will age in coming years.

## Aging populations

After quite a bit of experimentation and iteration through different variants, I came up with this chart of the proportion of elderly people in the different PICTs:

<object type="image/svg+xml" data='/img/0271-aging-linechart.svg' width='100%'><img src='/img/0271-aging-linechart.png' width='100%'></object>

The choice of "percentage of population that is aged 65 and over" seemed straightforward enough giving my aims to understand population aging. The harder questions were about what to map colour to. I tried an individual colour for each country - this was probably the easiest to read, and maybe what I'd use if the chart was for a more general readership. I also tried mapping colour to subregion (Melanesia, Polynesia and Micronesia), to population growth rates, and population sizes, before settling on GDP per capita in 2023 as a genuinely interesting additional variable that highlights some relationships between aging and income.

What I like about the chart I eventually chose is how it clearly shows the three clusters of countries, in terms of projected aging to 2050; and the top two (most aging) clusters of four countries each are clearly richer (in terms of GDP per capita in 2023) than the larger, bottom (least aging) cluster of poorer countries. Tiny Niue and Nauru being exceptions as countries with less predicted aging despite relatively high GDP; but Niue is a particularly difficult country to model because of its tiny size and ease of movement to New Zealand, and Nauru's economic status is complex for all sorts of interesting historical reasons.

I did feel the sub-regional aspect was an important one too, despite not choosing it in the end for colour. So I made a faceted version of the same chart to show that, which I think works quite nicely too:

<object type="image/svg+xml" data='/img/0271-aging-linechart-facet.svg' width='100%'><img src='/img/0271-aging-linechart-facet.png' width='100%'></object>

All the code for these charts is in one chunk at the bottom of the blog. It's all straightforward stuff, importing data from two data sources and then some moderately finicky plot polishing. The power of the grammar of graphics really shone through in making it easy to iterate through different versions of these before coming to the final production charts; this isn't so obvious in just reading the code except for when we see the simple one-liner conversion of the main chart into the faceted one.

## Absolute size of population by age

I wanted a sense of the absolute magnitudes too. Proportions of populations by PICT are all very well, but how many millions of people are we talking about in different age groups? It turns out this is an important thing to visualise because of how easy it is to forget that, in numbers of people terms, the Pacific is dominated by Melanesia, and Melanesia in turn is mostly Papua New Guinea. Again after a bit of iteration and experimentation I came up with this chart to show that:

<object type="image/svg+xml" data='/img/0271-aging-areachart.svg' width='100%'><img src='/img/0271-aging-areachart.png' width='100%'></object>

## R code

Well that's all for today very straightforward stuff but some nice charts. Big thanks to the UN for making the detailed data in the World Population Projections so nicely accessible. Here's the code to import that data, plus GDP per capita from the Pacific Data Hub at my own work, and turn it into those charts.

{% highlight R lineanchors %}
library(tidyverse)
library(scales)
library(readxl)
library(janitor)
library(ggrepel)
library(rsdmx)
library(extrafont)

#-------------------Download and prep data-----------------------

# GDP per capita of Pacific Island countries and terrritories--------------
# From the Pacific Data Hub
gdp <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POCKET,3.0/A..GDPCPCUSD?startPeriod=2023&endPeriod=2023&dimensionAtObservation=AllDimensions") |>
  as_tibble() |>
  clean_names() |>
  select(geo_pict, gdp_per_capita_2023 = obs_value)

# UN World Population Projections 2024
# a) population by 5-year age group and sex:
options(timeout=600)
df1 <- "pp24_agegrp.csv"
if(!file.exists(df1)){
  download.file("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryByAge5GroupSex_Medium.csv.gz",
                destfile = df1, mode = "wb")
}

# b) standard basic indicators (for some reason the CSV version is missing):
df2 <- "pp24_standard.xlsx"
if(!file.exists(df2)){
  unlink(df2)
  download.file("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
                destfile = df2, mode = "wb")
}


standard <- read_excel(df2, skip = 16) |>
  rename(pop = `Total Population, as of 1 July (thousands)`)

agegrp <- read_csv(df1) |>
  clean_names()

picts <- tibble(location = c(
  "Fiji",
  "New Caledonia",
  "Papua New Guinea",
  "Solomon Islands",
  "Vanuatu",
  
  "Guam",
  "Kiribati",
  "Marshall Islands",
  "Micronesia (Fed. States of)",
  "Nauru",
  "Northern Mariana Islands",
  "Palau",
  
  "American Samoa",
  "Cook Islands",
  "French Polynesia",
  "Niue",
  "Samoa",
  "Tokelau",
  "Tonga",
  "Tuvalu",
  "Wallis and Futuna Islands"),
  subregion = rep(c("Melanesia", "Micronesia", "Polynesia"), times = c(5, 7, 9)))

# check we have all 21 SPC members (other than Pitcairn)
stopifnot(nrow(picts) == 21)         
# check we have the same spelling of names as in the UN World Population Projections:
stopifnot(all(picts$location %in% agegrp$location))

# Some age categories for use later
old_grps <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") 
yng_grps <- c("0-4", "5-9", "10-14")
stopifnot(all(c(old_grps, yng_grps) %in% unique(agegrp$age_grp)))

#' Compound annual growth rate
#' 
#' @param x1 starting observation
#' @param x2 finishing observation
#' @param years number of years between x1 and x2
#' @examples
#' cagr(100, 110, 5)
#' 
cagr <- function(x1, x2, years){
  y <- (x2 / x1) ^ (1 / years) - 1
  return(y)
}

# make a summary of the PICTs values for use later
pict_sum <- agegrp |>
  inner_join(picts, by = "location") |>
  left_join(gdp, by = c("iso2_code" = "geo_pict")) |>
  mutate(old = age_grp %in% old_grps,
         young = age_grp %in% yng_grps) |>
  group_by(location, time, subregion, gdp_per_capita_2023) |>
  summarise(pop = sum(pop_total),
            pop_old = sum(pop_total[old]),
            pop_young = sum(pop_total[young]),
            prop_old =  pop_old / pop) |>
  ungroup() |>
  group_by(location) |>
  arrange(time) |>
  mutate(pop2024 = pop[time == 2024],
         yoy_growth = pop / dplyr::lag(pop) - 1,
         coming_growth = cagr(pop2024, pop[time == 2050], 26)) |>
  ungroup() |>
  mutate(pop2024_rank = as.numeric(as.factor(rank(-pop2024)))) |>
  filter(time %in% 1950:2050)
  # filter(time %in% seq(from = 1950, to = 2050, by = 5))

#----------------------compare aging to GDP-----------
myfont <- "Roboto"

p1 <- pict_sum |>
  ggplot(aes(x = time, y = prop_old, colour = gdp_per_capita_2023, group = location)) +
  annotate("rect", xmin = 2011, xmax = 2023.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  annotate("rect", xmin = 2023.5, xmax = 2050.5, ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.5) +
  geom_line() +
  geom_point(data = filter(pict_sum, time %in% c(1950, 2024, 2050)), 
             aes(size = pop), alpha = 0.5) +
  geom_text_repel(data = filter(pict_sum, time == 2050), direction = "y",
                  aes(label = location), hjust = 0, nudge_x = 3, seed = 123,
                  min.segment.length = 5, family = myfont, size = 2.9,
                  alpha = 1, force = 0.001, force_pull = 1) +
  scale_colour_viridis_c(option = "D", trans = log10_trans(), 
                         label = dollar_format(scale = 1, largest_with_cents = 100)) +
  scale_y_continuous(label = percent) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2050, by = 25), limits = c(1950, 2070)) +
  scale_size_area(label = comma_format(suffix = "m", scale = 1/1000), 
                  breaks = c(0.1, 1, 2, 5, 10, 20) * 1000, max_size = 12) +
  theme(legend.position = c(0.2, 0.62),
        legend.title = element_text(size = 10)) +
  labs(x = "", y = "Proportion of population that is 65 or older",
       colour = "GDP per capita in 2023 (USD)",
       size= " Population",
       title = "Aging populations in the Pacific",
       subtitle = "Shaded areas indicate increasingly dependent on projections",
       caption = "Source: UN World Population Prospects 2024 (population); SPC Pocket Summary (GDP)")

print(p1)                  
                  
# faceted version                  
p2 <- p1 +
  facet_wrap(~subregion, ncol = 2) +
  theme(legend.position = c(0.8, 0.2),
        legend.direction = "horizontal")


print(p2)

#------------------population by age group in absolute numbers-----------------
# colour palette for the sub-regions
sr_pal <- c("palegreen", brewer_pal(palette = "Accent")(4)[c(1,3,2)])

p3 <- pict_sum |>
  mutate(location2 = case_when(
    location == "Papua New Guinea" ~ location, 
    subregion == "Melanesia" ~ "Melanesia other than PNG",
    TRUE ~ subregion
    )) |>
  mutate(pop_working_age = pop - pop_old - pop_young) |>
  select(location, location2, subregion, time, pop_old, pop_young, pop_working_age) |>
  gather(variable, value, -location, -time, -subregion, -location2) |>
  mutate(variable = case_when(
    variable == "pop_old" ~ "65 and older",
    variable == "pop_young" ~ "Younger than 15",
    variable == "pop_working_age" ~ "15-64"
  )) |>
  mutate(location2 = fct_relevel(location2, c("Papua New Guinea",
                                              "Melanesia other than PNG"))) |>
  mutate(variable = fct_relevel(variable, "Younger than 15")) |>
  group_by(time, location2, variable) |>
  summarise(value = sum(value)) |>
  ggplot(aes(x = time, fill = location2, y = value)) +
  facet_wrap(~variable, ncol = 3) +
  annotate("rect", xmin = 2011, xmax = 2023.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  annotate("rect", xmin = 2023.5, xmax = 2050.5, ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.5) +
  geom_area() +
  scale_fill_manual(values = sr_pal) +
  scale_y_continuous(label = comma_format(scale = 1/1000, suffix = "m")) +
  labs(fill = "",
       x = "", y = "People (millions)",
       title = "Rapidly growing working age population in Melanesia",
       subtitle = "Shaded areas indicate increasingly dependent on projections",
       caption = "Source: UN World Population Prospects 2024")

print(p3)

{% endhighlight %}
