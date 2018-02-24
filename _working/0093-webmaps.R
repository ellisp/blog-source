library(tidyverse)
library(plotGoogleMaps)
library(viridis)
library(leaflet)



circles_data <- expand.grid(lat  = seq(from = -80, to = 80, by = 10),
                            lon  = seq(from = -180, to = 180, by = 20)) %>%
   mutate(rad = rnorm(n(), 100, 1))

circles_sp <- with(circles_data, SpatialPointsDataFrame(coords = cbind(lon, lat),
                                                        data = data.frame(rad),
                                                        proj4string = CRS("+proj=longlat +datum=WGS84")))

bubbleGoogleMaps(circles_sp, zcol = "rad", filename = "_output/0093-goog1.html",
                 max.radius =  2 * 10 ^ 5, do.sqrt = TRUE,
                 openMap = FALSE, control = FALSE, legend = FALSE,
                 map.width = "1000px", map.height = "520px",
                 fitBounds = FALSE, zoom = 2,
                 fillOpacity = 1, strokeOpacity = 1,
                 colPalette = "white")

# leaflet with circles - radius mapped to metres.  This is what you want if eg
# you are showing a bomb's impact area.  The Mercator projection makes them look
# bigger nearer the poles; and when you zoom in or out the circles get bigger or
# smaller on screen so they show the same number of metres.
leaflet() %>%
   addTiles() %>%
   addCircles(radius = ~rad * 2000, data = circles_data)

# leaflet with circleMarkers - radius mapped to pixels.  So they aren't warped
# by the Mercator projection; and they redraw when you zoom in
leaflet() %>%
   addTiles() %>%
   addCircleMarkers(radius = ~rad / 15, data = circles_data)


# useful here https://www.revolvy.com/main/index.php?s=Web%20Mercator
# EPSG is the european petroleum survey group

# https://gis.stackexchange.com/questions/194295/getting-borders-as-svg-using-peters-projection

peters_proj4 <- "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
peters_crs = leafletCRS(crsClass = "L.CRS.EPSG4326",
                        proj4def = peters_proj4)

leaflet(options = leafletOptions(crs = peters_crs)) %>%
   addTiles() %>%
   addCircles(radius = ~rad * 15, data = circles_data)


epsg2163 <- leafletCRS(
   crsClass = "L.Proj.CRS",
   code = "EPSG:2163",
   proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
   resolutions = 2^(16:7))

leaflet(options = leafletOptions(crs = epsg2163)) %>%
#   addTiles() %>%
   addCircleMarkers(radius = ~rad / 15, data = circles_data)


epsg3006 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:3006",
                       proj4def = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                       resolutions = 2^(13:-1), # 8192 down to 0.5
                       origin = c(0, 0)
)

tile_url <- "http://api.geosition.com/tile/osm-bright-3006/{z}/{x}/{y}.png"
tile_attrib <- "Map data &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap contributors</a>, Imagery &copy; 2013 <a href='http://www.kartena.se/'>Kartena</a>"

leaflet(options = leafletOptions(crs = epsg3006)) %>%
   addTiles(urlTemplate = tile_url,
            attribution = tile_attrib,
            options = tileOptions(continuousWorld = T)) %>%
   addCircleMarkers(radius = ~rad / 15, data = circles_data)



# good overview of coordinate reference systems (CRS) in R at 
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

turnout_geocoded <- read_csv("../Data/CrowdEstimates_Geocoded.csv") %>%
   mutate(Average = as.numeric(Average))

summary(turnout_geocoded)

# The projection used by google maps makes high latitudes objects too large
turnout_sp <-  with(filter(turnout_geocoded, !is.na(lon) & !is.na(Average) & lat > -70), 
                    SpatialPointsDataFrame(coords = cbind(lon, lat),
                                           data = data.frame(City, Country, Turnout = Average),
                                           proj4string = CRS("+proj=longlat +datum=WGS84")))



m1 <- bubbleGoogleMaps(turnout_sp, zcol='Turnout', filename = "_output/turnout_map_1.html", 
                       openMap = FALSE,
                       max.radius = 2 * 10 ^5, do.sqrt = TRUE,
                       key.entries = c(50, 500, 5000, 50000),
                       strokeOpacity = 0, fillOpacity = 0.6, shape = "q",
                       colPalette = magma(4, direction = -1),
                       mapTypeId = "ROADMAP",
                       control = FALSE, legend = FALSE,
                       map.width = "1000px", map.height = "520px",
                       fitBounds = FALSE, zoom = 2)



