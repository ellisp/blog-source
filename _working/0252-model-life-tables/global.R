# Shiny app to explore the UN's model life tables
# Peter Ellis August 2023
#
# see `../0252-model life tables.R` for the prep that creates the .rda objects
# that are called here

library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(fresh)    # for use_googlefont with HTML
library(showtext) # for fonts in ggplot2 graphics
library(curl)     # to make showtext work

load("pal.rda")
load("families.rda")
load("e0s.rda")
load("mlt_raw.rda")

font_add_google("Roboto", "main_font")

showtext_auto()
res <- 150
showtext_opts(dpi = res)

my_theme <- theme_light(base_family = "main_font") + 
  theme(legend.position = "bottom") +
  theme(plot.caption = element_text(colour = "grey50"),
        strip.text = element_text(size = rel(1), face = "bold"),
        plot.title = element_text(family = "main_font"))

theme_set(my_theme)          
