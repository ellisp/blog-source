source("_R/utilities.R")

setwd("_working")
proj_dir <- getwd()
library(showtext)
library(ggplot2)
library(scales)
library(grDevices)
library(stats)
library(extrafont)
library(beepr)

# font_add_google("Poppins", "myfont")
# res <- try(font_add_google("Roboto", "Roboto"))
# if(class(res) != "try-error"){
#   font_add_google("Sarala", "Sarala")
#   showtext_auto()
#   showtext_opts(dpi = 600)
#   
# } 

myfont <- "Roboto"
main_font <- "Roboto"
heading_font <- "Sarala"


theme_set(theme_light(base_family = main_font) + 
             theme(legend.position = "bottom") +
             theme(plot.caption = element_text(colour = "grey50"),
                   strip.text = element_text(size = rel(1), face = "bold"),
                   plot.title = element_text(family = heading_font))
          ) 
update_geom_defaults("text", list(family = main_font))

# update the "most popular" pages page
# source("../_R/_analytics.R")
