source("_R/utilities.R")

setwd("_working")
proj_dir <- getwd()
library(showtext)
library(ggplot2)
library(scales)
library(grDevices)
library(stats)
library(extrafont)
library(beep)

# font_add_google("Poppins", "myfont")
font_add_google("Roboto", "main_font")
font_add_google("Roboto", "myfont")
font_add_google("Sarala", "heading_font")

showtext.auto()
showtext.opts(dpi = 600)

myfont <- "main_font"
main_font <- "main_font"
heading_font <- "heading_font"

theme_set(theme_light(base_family = main_font) + 
             theme(legend.position = "bottom") +
             theme(plot.caption = element_text(colour = "grey50"),
                   strip.text = element_text(size = rel(1), face = "bold"),
                   plot.title = element_text(family = heading_font))
          ) 
update_geom_defaults("text", list(family = main_font))

# update the "most popular" pages page
source("../_R/_analytics.R")
