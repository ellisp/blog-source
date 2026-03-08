
# This script draws a simple bar chart of the latest year of remittances data
#
# Peter Ellis November 2025

library(WDI)
library(tidyverse)
library(glue)

picts <- c(
  "Fiji", "New Caledonia", "Papua New Guinea", "Solomon Islands",                                             
  "Guam", "Kiribati", "Marshall Islands", "Micronesia, Fed. Sts.", "Nauru",
  "Vanuatu", "Northern Mariana Islands","Palau", "American Samoa", "Cook Islands",
  "French Polynesia", "Niue", "Samoa", "Tokelau", "Tonga", "Tuvalu", "Wallis and Futuna Islands" 
)
length(picts)
sort(picts) # all 22 SPC PICT members except for Pitcairn

# Used this to see what series are available:
# WDIsearch("remittance") |>  View()
#
# Download data from World Bank's World Development Indicators.
# Apparently worker remittances is a subset of personal. But
# the worker remittances are all NA anyway:

remit <- WDI(indicator = c(personal = "BX.TRF.PWKR.DT.GD.ZS",
                           worker = "BX.TRF.PWKR.GD.ZS"), start = 2000) |> 
  as_tibble()

# which countries have we got?
sort(unique(remit$country))

# check who missing, just the 3 NZ Realm countries plus Wallis and futuna:
picts[!picts %in% unique(remit$country)]

# Data for bar chart:
pac_data <- remit |> 
  group_by(country) |> 
  filter(!is.na(personal)) |> 
  arrange(desc(year)) |> 
  slice(1) |> 
  ungroup() |> 
  filter(country %in% c(picts, "Middle income", "Low income", "Small states", "World", "Australia", "New Zealand")) |> 
  mutate(is_pict = ifelse(country %in% picts, "Pacific island", "Comparison")) |> 
  mutate(country_order = ifelse(country %in% picts, personal, 1000 - personal),
         country = fct_reorder(country, country_order)) 

# draw bar chart
bar_remit <- pac_data|> 
  ggplot(aes(x = country, y = personal, fill = is_pict)) +
  geom_col() +
  scale_y_continuous(label = percent_format(scale = 1)) +
  scale_fill_manual(values = c("brown", "steelblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position  = "none",
        plot.caption = element_text(colour = "grey50")) +
  labs(x = "", fill = "",
      subtitle = glue('{attr(remit$personal, "label")}, {min(pac_data$year)} to {max(pac_data$year)}'),
        y = "",
       title = "High dependency on remittances for many Pacific Island countries and territories",
       caption = "Source: World Bank World Development Indicators, series BX.TRF.PWKR.DT.GD.ZS")

# change colours to be consistent with others in series
svg_png(bar_remit, "../img/0315-remittances-bar", w = 8.5, h = 4.5)


