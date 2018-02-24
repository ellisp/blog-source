library(micromap)
library(nzcensus)
library(extrafont)
library(mbiemaps) # for the TA map
library(tidyverse)
library(testthat)
library(scales)
library(ggrepel)

the_font <- "Calibri"
theme_set(theme_minimal(base_family = the_font))


# ok intro to concepts and examples (no code) at 
# http://www.thefunctionalart.com/2013/07/falling-in-love-with-micromaps.html

# package is from USEPA and source code is at
# https://github.com/USEPA/R-micromap-package-development


#============polish up the example from the ?lmplot helpfile===============
data("USstates")
data("edPov")

# convert from SpatialPolygonsDataFrame into a data frame (similar to fortify)
statePolys <- create_map_table(USstates, 'ST')
head(statePolys)
head(edPov)

# annoyingly, this fires up a new (!) graphics device and prints the result
# to it as a side effect, unless you specify print.file yourself.  This is
# a bit anti-pattern for R graphics.  Amongst other things it complicates
# font handling, resolutions, image sizes, and stops you saving to SVGs.


lmplot(stat.data = edPov,
       map.data = statePolys,
       panel.types = c('labels', 'dot', 'dot','map'),
       panel.data = list('state','pov','ed', NA),
       ord.by = 'pov',   
       grouping = 5,
       colors = brewer.pal(5, "Spectral"),
       median.row = TRUE,
       # how to merge the two data frames:
       map.link = c('StateAb', 'ID'),
       # stroke colour of the borders of previously-drawn areas:
       map.color2 = "white",
       # how to save the result:
       print.file = "../img/0095-eg1.png",
       print.res = 600,
       plot.width = 7,
       plot.height = 9,
       # attributes of the panels:
       panel.att = list(
          list(1, header = "States", 
               panel.width = 0.8, 
               text.size = 0.8, 
               align = 'right', 
               panel.header.font = the_font,
               panel.header.face = "bold",
               text.font = the_font,
               left.margin = 2,
               right.margin = 1),
          list(2, header = "Percent living below\npoverty level", 
               xaxis.title = 'Percent',
               xaxis.ticks = list(10, 15, 20),
               xaxis.labels = list(10, 15, 20),
               graph.bgcolor = 'lightblue',
               graph.grid.color = "grey99",
               graph.border.color = "white", 
               panel.header.font = the_font,
               panel.header.face = "bold"),
          list(3, header = "Percent adults with\n4+ years of college", 
               xaxis.title = 'Percent',
               xaxis.ticks = list(0, 20, 30, 40),
               xaxis.labels = list(0, 20, 30, 40),
               graph.bgcolor = 'lightblue',
               graph.grid.color = "grey99",
               graph.border.color = "white", 
               panel.header.font = the_font,
               panel.header.face = "bold"),
          list(4, header = 'Light gray means\nhighlighted above', 
               panel.width = 0.8,
               panel.header.font = the_font,
               panel.header.face = "italic")
       ))
# Use to get a list of attributes:
unlist(labels_att(TRUE) )
unlist(dot_att(TRUE) )
unlist(map_att(TRUE) )

# don't seem to work:
# median.text.label (no effect, always says "Median")
# median.text.size
# map.all (no effect)
# map.color2 (colours stroke but not fill)
# plot.header (no effect)

#================New Zealand example============
# pre-fortified data frame version of TA level map, from mbiemaps package:
data(ta_simpl_gg)

# change name in census data to match the name used in ta_simpl_gg
TA2013$short_name <- gsub(" District", "", TA2013$TA2013_NAM)


ta_data <- TA2013 %>%
   filter(!short_name %in% c("Chatham Islands Territory", "Area Outside Territorial Authority")) %>%
   mutate(PercNoReligion = PropNoReligion2013 * 100)

svg("../img/0095-choropleth.svg", 7, 8)
ta_simpl_gg %>%
   left_join(ta_data, by = c("NAME" = "short_name")) %>%
   arrange(order) %>% 
   ggplot(aes(x = long, y = lat, group = group, fill = MedianIncome2013)) +
   geom_polygon(colour = "grey50") +
   ggmap::theme_nothing(legend = TRUE) +
   theme(legend.position = c(0.2, 0.7)) +
   scale_fill_gradientn("Median individual\nincome", 
                        colours = brewer.pal(11, "Spectral"), label = dollar) +
   coord_map(projection = "sinusoidal") +
   ggtitle("Example choropleth map") +
   labs(caption = "Source: Statistics New Zealand, 2013 Census")
dev.off()

# change names of the map data frame to meet lmplot's expectations
ta_polys2 <- ta_simpl_gg %>%
   rename(coordsx = long, 
          coordsy = lat, 
          ID = NAME) %>%
   mutate(hole = as.numeric(hole),
          plug = 0,
          region = as.numeric(as.factor(ID)))

# check merge will be ok
a <- unique(ta_polys2$ID)       # names of map polygons
b <- unique(ta_data$short_name) # names of TAs with census data
expect_equal(a[!a %in% b], b[!b %in% a])



lmplot(stat.data = ta_data,
       map.data = ta_polys2,
       panel.types = c('labels', 'dot', 'dot','map'),
       panel.data = list('short_name','MedianIncome2013','PercNoReligion', NA),
       ord.by = 'MedianIncome2013',   
       grouping = 6,
       median.row = FALSE,
       # how to merge the two data frames:
       map.link = c('short_name', 'ID'),
       # stroke colour of the borders of previously-drawn areas:
       map.color2 = "white",
       # how to save the result:
       print.file = "../img/0095-eg2.png",
       print.res = 600,
       plot.width = 14,
       plot.height = 18,
       # attributes of the panels:
       panel.att = list(
          list(1, header = "Territorial Authorities", 
               panel.width = 1.2, 
               panel.header.size = 2,
               text.size = 1.6, 
               align = 'right', 
               panel.header.font = the_font,
               panel.header.face = "bold",
               text.font = the_font,
               left.margin = 2,
               right.margin = 1,
               xaxis.title.size = 2),
          list(2, header = "Median\nincome", 
               panel.header.size = 2,
               xaxis.title = 'Dollars',
               xaxis.title.size = 2,
               xaxis.labels.size = 2,
               graph.bgcolor = 'grey80',
               graph.grid.color = "grey99",
               graph.border.color = "white", 
               panel.header.font = the_font,
               panel.header.face = "bold",
               point.border = FALSE,
               point.size = 2),
          list(3, header = "Percent individuals\nwith no religion", 
               panel.header.size = 2,
               xaxis.title = 'Percent',
               xaxis.title.size = 2,
               xaxis.labels.size = 2,
               graph.bgcolor = 'grey80',
               graph.grid.color = "grey99",
               graph.border.color = "white", 
               panel.header.font = the_font,
               panel.header.face = "bold",
               point.border = FALSE,
               point.size = 2),
          list(4, header = '\n', 
               panel.header.size = 2,
               panel.width = 0.5,
               panel.header.font = the_font,
               panel.header.face = "italic",
               active.border.size = 0.2,
               withdata.border.size = 0.2,
               nodata.border.size = 0.2)
       ))


svg("../img/0095-scatter.svg", 10, 7)
ta_data %>%
   ggplot(aes(x = MedianIncome2013, y = PropNoReligion2013, label = short_name)) +
   scale_x_continuous("Median income", label = dollar) +
   scale_y_continuous("No religion", label = percent) +
   geom_smooth(method = "lm", colour = "white") +
   geom_point(aes(size = CensusNightPop2013), shape = 19, colour = "grey60") +
   geom_point(aes(size = CensusNightPop2013), shape = 1, colour = "black") +
   geom_text_repel(colour = "steelblue", size = 2.5) +
   scale_size_area("Census night population", max_size = 10, label = comma) +
   theme(legend.position = "bottom") +
   labs(caption = "Source: Statistics New Zealand, census 2013")
dev.off()   

#================regions example============
# convert from SpatialPolygonsDataFrame into a data frame (similar to fortify)
data(region_simpl)
reg_polys <- create_map_table(region_simpl, 'NAME')

reg_data <- REGC2013 %>%
   mutate(PercNoReligion = PropNoReligion2013 * 100) %>%
   filter(REGC2013_N != "Area Outside Region")

lmplot(stat.data = reg_data,
       map.data = reg_polys,
       panel.types = c('labels', 'dot', 'dot','map'),
       panel.data = list('REGC2013_N','MedianIncome2013','PercNoReligion', NA),
       ord.by = 'MedianIncome2013',   
       grouping = 4,
       median.row = FALSE,
       # how to merge the two data frames:
       map.link = c('REGC2013_N', 'ID'),
       # stroke colour of the borders of previously-drawn areas:
       map.color2 = "white",
       # how to save the result:
       print.file = "../img/0095-eg3.png",
       print.res = 600,
       plot.width = 8,
       plot.height = 9,
       # attributes of the panels:
       panel.att = list(
          list(1, header = "Territorial Authorities", 
               panel.width = 1.2, 
               text.size = 0.8, 
               align = 'right', 
               panel.header.font = the_font,
               panel.header.face = "bold",
               text.font = the_font,
               left.margin = 2,
               right.margin = 1),
          list(2, header = "Median\nincome", 
               xaxis.title = 'Dollars',
               panel.width = 0.8,
               left.margin = .5, 
               right.margin = .5, 
               graph.bgcolor = 'grey90',
               graph.grid.color = "grey99",
               graph.border.color = "white", 
               panel.header.font = the_font,
               panel.header.face = "bold",
               point.border = FALSE,
               point.size = 2.5),
          list(3, header = "Percent individuals\nwith no religion", 
               panel.width = 0.8,
               left.margin = .5, 
               right.margin = .5, 
               xaxis.title = 'Percent',
               graph.bgcolor = 'grey90',
               graph.grid.color = "grey99",
               graph.border.color = "white", 
               panel.header.font = the_font,
               panel.header.face = "bold",
               point.border = FALSE,
               point.size = 2.5),
          list(4, header = '\n', 
               panel.width = 1.2,
               panel.header.font = the_font,
               panel.header.face = "italic",
               active.border.size = 0.5,
               withdata.border.size = 0.5,
               nodata.border.size = 0.5)
       ))


convert_pngs("0095")
