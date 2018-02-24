
# original motivation came from
# https://www.nytimes.com/2017/05/26/world/europe/nato-trump-spending.html?_r=0
# note that the SIPRI figures don't match those of the NY Times

# Load in all the packages we need for the full post
library(cshapes)
library(tidyverse)
library(forcats)
library(scales)
library(openxlsx)
library(directlabels)
library(ggthemes)
library(RColorBrewer)
library(countrycode)
library(maps)
library(viridis)
library(leaflet)
library(rworldmap)
library(sf)
library(maptools)

# The WDI has fallen behind its source SIPRI https://www.sipri.org/databases/milex
# At time of writing WDI only goes up to 2015, but SIPR has 2016 data.

tf <- tempfile()

download.file("https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1949-2016.xlsx",
              destfile = tf, mode = "wb")

sipri <- read.xlsx(tf, sheet = "Share of GDP", startRow = 6) %>%
   gather(Year, Value, -Country, -Notes) %>%
   mutate(Value = as.numeric(Value),
          Year = as.numeric(Year)) %>%
   filter(!is.na(Value))

svg("../img/0099-all-countries.svg", 8, 5)
sipri %>%
   ggplot(aes(x = Year, y = Value, colour = Country)) +
   geom_line() +
   theme(legend.position = "none") +
   scale_y_continuous("Military expenditure as a percentage of GDP", label = percent) +
   ggtitle("Countries' and regions' expenditure on military as a percentage of GDP") +
   labs(caption = "Source: Stockholm International Peace Research Institute", x = "")
dev.off()

sipri %>%
   filter(Value > 0.25) %>%
   select(-Notes) %>%
   arrange(Country, Year)

#============Biggest spending==============
total_spend <- sipri %>%
   filter(!is.na(Value)) %>%
   group_by(Country) %>%
   summarise(Mean = mean(Value),
             TrMean = mean(Value, tr = 0.2),
             Max = max(Value)) %>%
   arrange(desc(Mean))

countries_big <- total_spend[1:10, ]$Country

p2 <- sipri %>%
   filter(Country %in% countries_big) %>%
   ggplot(aes(x = Year, y = Value, colour = Country)) +
   geom_line() +
   #theme(legend.position = "none") +
   scale_y_continuous("Military expenditure as a percentage of GDP", label = percent) +
   ggtitle("Selected countries' expenditure on military as a percentage of GDP", 
           "Ten biggest spending countries only") +
   labs(caption = "Source: Stockholm International Peace Research Institute",
        x= "", colour = "")

svg("../img/0099-biggest-spenders.svg", 8, 6)
print(p2)
dev.off()

   
#==========Five eyes countries===================
wid <- 0.01 # width of annotation bars
wars <- data_frame(
   name =          c("Malaysia",  "Korea",     "Aden",        "Vietnam",   "Northern Ireland", "Falklands",  "Gulf",      "Afghanistan",  "Iraq"),
   start = as.Date(c("15/6/1948", "25/6/1950", "10/12/1963",  "1/11/1955", "1/1/1968",         "2/4/1982" ,  "2/8/1990",  "7/10/2001",   "20/3/2003"), 
                   format = "%d/%m/%Y"),
   end =   as.Date(c("12/7/1960", "27/7/1953",  "30/11/1967", "30/4/1975", "30/6/1998",        "14/6/1982", "28/2/1991","28/12/2014",   "18/12/2011"),
                   format = "%d/%m/%Y"),
   lead =        c("UK",         "USA",      "UK",            "USA",        "UK",              "UK", "USA",         "USA",         "USA")
) %>%
   mutate(name_seq = n():1,
          ystart = wid * name_seq + 0.12, 
          yend = ystart +wid )


countries <- c("Australia", "New Zealand", "USA", "UK", "Canada")
palette <- brewer.pal(5, "Set1")
names(palette) <- countries
   
p <- sipri %>%
   filter(Country %in% countries) %>%
   mutate(YearDate = as.Date(paste0("30/6/", Year), format = "%d/%m/%Y")) %>%
   ggplot() +
   geom_rect(data = wars, aes(xmin = start, xmax = end, ymin = ystart, ymax = yend, fill = lead),
             alpha = 0.2) +
   geom_text(data = wars, aes(x = end, y = (yend + ystart) / 2, label = name),
             colour = "grey50", hjust = 1, nudge_x = -200, size = 3) +
   geom_line(aes(x = YearDate, y = Value, colour = Country)) +
   scale_y_continuous("Military expenditure as a percentage of GDP", label = percent,
                      breaks = c(0, 5, 10, 15) / 100) +
   ggtitle("Selected countries' expenditure on military as a percentage of GDP",
           "'Five eyes' countries only; periods also shown for conflicts in which they had material deployments") +
   labs(x = "",
        caption = "Source: Stockholm International Peace Research Institute") +
   scale_colour_manual(values = palette) +
   scale_fill_manual(values = palette, guide = "none") +
   theme_tufte(base_family = "myfont") +
   theme(plot.caption = element_text(colour = "grey50")) + 
   annotate("text", x = as.Date("1970/6/30"), y =0.145, hjust = 0.5, 
            colour = "grey50", size = 3,
            label = "Not shown - Grenada, Panama, Balkans,\nLibya, Lebanon, Haiti and numerous smaller...")

svg("../img/0099-fiveeyes.svg", 8, 6)
direct.label(p)
dev.off()


#===============map prep========================
sipri <- sipri %>%
   mutate(iso3c = countrycode(Country, "country.name", destination = "iso3c")) %>%
   # two manual concordances of country code:
   mutate(iso3c = ifelse(Country == "Kosovo", "KOS", iso3c))

# for some reason the tidyverse doesn't work for Central African Republic! 
# so we fix it old school:
sipri[sipri$Country == "Central African Rep.", "iso3c"] <- "CAF"

world <- map_data("world") %>%
   mutate(iso3c = countrycode(region, "country.name", destination = "iso3c"))

# data on military for just the latest year for each country
the_data <- sipri %>%
   group_by(Country) %>%
   filter(Year == max(Year))

# using the help at http://ggplot2.tidyverse.org/reference/coord_map.html

world2 <- world %>%
   left_join(the_data, by = "iso3c")

# define a ggplot mapping object
worldmap <- ggplot(world2, aes(x = long, y = lat, group = group, fill = Value)) +
   geom_polygon(colour = "grey75") +
   scale_y_continuous("", breaks = (-2:2) * 30) +
   scale_x_continuous("", breaks = (-4:4) * 45) +
   scale_fill_viridis("", label = percent, option = "inferno", direction = -1) +
   theme_minimal(base_family = "myfont") +
   theme(legend.position = "right",
         axis.text = element_blank()) +
   ggtitle("Military expenditure as a percentage of GDP",
           "Most recent data shown for each country; mostly this is 2016") +
   labs(caption = "Source: Stockholm International Peace Research Institute")


#-----------rotating globe with ggplot2---------------
setwd("C:/temp")
showtext.auto(enable = TRUE)
# set screen resolution
showtext.opts(dpi = 72)

# latitudes will go from 30 north to 30 south and back again:
lats <- rep(c(30:(-30), (-29):29), 3)
# longitudes will start at 50 and go around backwards:
lons <- c(50:(-309))

for(i in 1:length(lons)){
   lat <- lats[i]
   lon <- lons[i]
   png(paste0(1000 + i, ".png"), 600, 550, res = 100)
      print(worldmap + coord_map("ortho", orientation = c(lat, lon, 0)) )  
   dev.off()
}

system('magick -loop 0 -delay 7 *.png "military-gdp.gif"')


#-------------single map, nice projection-------------------
# Lots of the projections in coord_map have problems with drawing
# polygons.  ok are: globular, harrison 
setwd("D:/Peter/Documents/blog/ellisp.github.io/_working")
svg("../img/0099-globular.svg", 8, 5)
worldmap +   coord_map("globular")
dev.off()

svg("../img/0099-ortho.svg", 8, 4)
worldmap +   coord_map("orthographic", orientation = c(20, 20, 0))
dev.off()


#-------------------------leaflet------------------
shape <- countriesCoarse

pal <- colorNumeric(
   palette = inferno(10, direction = -1),
   domain = the_data$Value)

data2 <- shape@data %>%
   left_join(the_data, by = c("ISO_A3" = "iso3c")) %>%
   mutate(tooltip = paste0(ADMIN, " ", Year, ", ", round(Value * 100, 1), "%"))

# EPSG4326 means latitude and longitude
coarse_crs <- leafletCRS(crsClass = "L.CRS.EPSG4326", proj4def = proj4string(countriesCoarse))

shape %>%
   leaflet(options = leafletOptions(crs = coarse_crs))  %>%
   addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
               color = ~pal(data2$Value),
               label = data2$tooltip) 


#------------sf approach------------------
# this solution came from the demo in the sf package, tweeted about at
# https://twitter.com/edzerpebesma/status/835249468874321920
circ <- function(l = c(-180:180), lon0 = 0, lat0 = 30) {
   deg2rad = pi / 180
   lat = atan(-cos((l - lon0) * deg2rad)/tan(lat0 * deg2rad)) / deg2rad
   xy = if (lat0 == 0) {
      l1 = lon0 - 90
      l2 = lon0 + 90
      rbind(c(l1,-90), c(l2,-90), c(l2,0), c(l2,90), c(l1,90), c(l1,0), c(l1,-90))
   } else if (lat0 > 0) {
      xy = cbind(lon = l, lat = lat)
      rbind(c(-180,90),xy,c(180,90),c(-180,90))
   } else {
      xy = cbind(lon = l, lat = lat)[length(l):1,]
      rbind(c(180,-90), xy, c(-180,-90),c(180,-90))
   }
   st_sfc(st_polygon(list(xy)), crs = st_crs(4326))
}

m <- st_make_grid()

m <- st_segmentize(m, 4e5)


data(wrld_simpl)

w1 <- st_as_sf(countriesLow) %>%
   left_join(the_data, by = c("ISO3" = "iso3c")) %>%
   mutate(fill = pal(Value))

w2 <- st_as_sf(wrld_simpl) %>%
   left_join(the_data, by = c("ISO3" = "iso3c")) %>%
   mutate(fill = pal(Value))
# misses western sahara


# latitudes will go from 30 north to 30 south and back again:
lats <- rep(c(30:(-30), (-29):29), 3)
# longitudes will start at 50 and go around backwards:
lons <- c(180:(-179))

dir.create("C:/temp2")
setwd("C:/temp2")


# Frame 234 goes missing if using wrld_simpl
# Frame 269 goes missing if using countriesLow or wrld_simpl
for(i in 1:length(lons)){
   png(paste0(1000 + i, ".png"), 600, 550, res = 100)
   par(mar <- rep(0, 4), bg = "black")
   par(family = "myfont")
   
   lat <- lats[i]
   lon <- lons[i]
   
   p4s <- paste0("+proj=ortho +lat_0=", lat, " +lon_0=", lon)
   
   # draw the pale blue globe in space
   blank_globe <- st_transform(m, st_crs(p4s), check = TRUE)
   plot(blank_globe, col = '#e6f2ff', border = 'grey99')
   
   # create a clipped version of the great circle??
   # I don't really understand what is happening, but it seems to work.
   crc <- circ(lat0 = lat, lon0 = lon)
   w10 <- suppressWarnings(st_intersection(w1, crc))
   w20 <- suppressWarnings(st_intersection(w2, crc))
   
   # cast and re-project the map itself
   w10 <- st_cast(w10, "MULTIPOLYGON")
   w10 <- st_transform(w10["fill"], st_crs(p4s), check = TRUE) 
   w20 <- st_cast(w20, "MULTIPOLYGON")
   w20 <- st_transform(w20["fill"], st_crs(p4s), check = TRUE) 
   
   # draw the map
   plot(w10, col = w10$fill, add = TRUE, border = NA)
   plot(w20, col = w20$fill, add = TRUE, border = "grey75")
   
   # title and legend
   title("Military expenditure as a percentage of GDP\nNearest year to 2016", 
         col.main = "white", font.main = 1, adj = 0)
   title(sub = "Source: Stockholm International Peace Research Institute", 
         col.sub = "grey50", adj = 1)
   
   leg_nums <- seq(from = 4, to = 20, length.out = 6) / 100
   legend("bottomright", legend = paste0(round(leg_nums * 100), "%"),
          pch = 15, adj = 0.1,
          col = pal(leg_nums), text.col = pal(leg_nums),
          bg = "grey80")

   dev.off()
}
   
system('magick -loop 0 -delay 7 *.png "0099-military-gdp.gif"')


#================animated map over time===============
showtext.opts(dpi = 72)
setwd("C:/temp1")
years <- unique(sipri$Year)

for(i in years){

   this_year_data <- sipri %>%
      group_by(Country) %>%
      filter(Year == i)
   
   world2 <- world %>%
      left_join(this_year_data, by = "iso3c")
   
   worldmap <- ggplot(world2, aes(x = long, y = lat, group = group, fill = Value)) +
      geom_polygon(colour = "grey75") +
      scale_y_continuous("", breaks = (-2:2) * 30) +
      scale_x_continuous("", breaks = (-4:4) * 45) +
      scale_fill_viridis("", label = percent, option = "inferno", direction = -1, 
                         limits = range(sipri$Value), trans = "sqrt",
                         breaks = c(1, 10, 40, 100) / 100) +
      theme_minimal(base_family = "myfont") +
      theme(legend.position = "right",
            axis.text = element_blank()) +
      ggtitle(paste("Military expenditure as a percentage of GDP -", i)) +
      labs(caption = "Source: Stockholm International Peace Research Institute")
   
   png(paste0(i, ".png"), 12 * 72, 6 * 72)
      print(worldmap +   coord_map("harrison", dist = 1, angle = 30, ylim = c(-75, 85)))
   dev.off()
}

system('magick -loop 0 -delay 40 *.png "0099-military-gdp-time.gif"')



convert_pngs("0099")
