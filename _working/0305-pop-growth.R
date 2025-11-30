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
if(!file.exists("pop-mid-year-temp.rda")){
  d <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A.AS+CK+FJ+PF+GU+KI+MH+FM+NR+NC+NU+MP+PW+PG+PN+WS+SB+TK+TO+TV+VU+WF+_T+MEL+MIC+POL+_TXPNG+MELXPNG.MIDYEARPOPEST._T._T?startPeriod=1950&endPeriod=2050&dimensionAtObservation=AllDimensions") |> 
    as_tibble() |> 
    clean_names() |> 
    mutate(time_period = as.numeric(time_period))
  save(d, file = "pop-mid-year-temp.rda")
} else {
  load("pop-mid-year-temp.rda")
}

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


#----------------------time series line plot-------------

# This version just has 21 individual PICTs, no subregional totals. 21 fits
# ok on the screen in 3 rows of 7:
p1a <- d2 |> 
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

svg_png(p1a, "../img/0305-population-line", w = 13, h = 6)


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
  # labels for the axes, plot tile, legend:
  labs(x = "Population in 2025 (logarithmic scale)",
       y = "Compound annual population growth rate 2020 to 2025",
       colour = "",
       title = "Current population and recent growth in the Pacific",
       subtitle = "Populations of the Pacific Island country and territory members of the Pacific Community (SPC). 
",
       caption = the_caption)

# Save as a PNG, suitable for use in presentation or elsewhere
svg_png(p2b, "../img/0305-population-scatter", w = 9.5, h = 6)


easy_mobility <- c("Pitcairn", 
                   "Niue", "Tokelau", "Cook Islands", 
                   "Wallis and Futuna", "New Caledonia", "French Polynesia",
                   "Guam", "Northern Mariana Islands", "American Samoa",
                   "Marshall Islands", "Palau", "Micronesia, Fed. States")

# check all are in data apart from the two we deliberately dropped
stopifnot(sum(!easy_mobility %in% d4$pict) == 2)

p2c <- d4 |> 
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

svg_png(p2c, "../img/0305-population-scatter-highlighted", w = 9.5, h = 6)

