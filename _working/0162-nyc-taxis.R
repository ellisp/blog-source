
library(tidyverse)
library(scales)
library(odbc)
library(DBI)
library(clipr)
library(knitr)
library(kableExtra)
library(ggmap)
library(Cairo)
library(mgcv)
library(metR)

# assumes you have an ODBC data source named "nyc_taxi" to the database created by https://github.com/ellisp/nyc-taxis:
nyc_taxi <- dbConnect(odbc(), "localhost", database = "nyc_taxi")

#' Theme for dark background maps to be used later
#'
#' @author minimally  adapted from a Todd Schneider original
theme_dark_map <- function(base_size = 12, font_family = main_font){
  theme_bw(base_size) +
    theme(text = element_text(family = font_family, color = "#ffffff"),
          rect = element_rect(fill = "#000000", color = "#000000"),
          plot.background = element_rect(fill = "#000000", color = "#000000"),
          panel.background = element_rect(fill = "#000000", color = "#000000"),
          plot.title = element_text(family = font_family),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

theme_white_map <- function(base_size = 12, font_family = main_font){
  theme_bw(base_size) +
    theme(text = element_text(family = font_family, color = "#000000"),
          rect = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.title = element_text(family = font_family),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.caption = element_text(color = "grey50"))
}

the_caption <- "Analysis by http://freerangestats.info with data from NYC Taxi & Limousine Commission"

#-----------------------passenger count frequencies------
sql <- "
SELECT 
  COUNT(1) AS freq,
  passenger_count
FROM yellow.tripdata
GROUP BY passenger_count
"
# takes a couple of seconds:
pc_freq <- dbGetQuery(nyc_taxi, sql)

p1 <- pc_freq %>%
  ggplot(aes(x = str_pad(passenger_count, 3, pad = "0"), y = freq)) +
  geom_col() +
  scale_y_log10(label = comma) +
  scale_x_discrete(labels = sort(unique(pc_freq$passenger_count))) +
  labs(x = "Number of passengers (discrete scale - only values with at least one trip shown)",
       y = "Number of trips (log scale)",
       title = "Number of passengers per trip",
       subtitle = "New York City yellow cabs January 2009 to mid 2016",
       caption = the_caption)

svg_png(p1, "../img/0162-passenger-counts",w = 15, h = 6)



#-----------------------distance in miles--------------

sql <- "
SELECT 
  COUNT(1) AS freq,
  ROUND(trip_distance, 1) AS trip_distance
FROM yellow.tripdata
GROUP BY ROUND(trip_distance, 1)
"

# takes about 20 seconds
td_freq <- dbGetQuery(nyc_taxi, sql)

p2 <- td_freq  %>%
  filter(trip_distance <= 250) %>%
  ggplot(aes(x = trip_distance, y = freq)) +
  geom_line() +
  scale_y_log10(label = comma) +
  labs(x = "Trip distance in miles - rounded to 0.1 of a mile",
       y = "Number of trips (log scale)",
       title = "Trip distances",
       subtitle = "New York City yellow cabs January 2009 to mid 2016",
       caption = the_caption)

svg_png(p2, "../img/0162-trip-dist1", 10, 4)

sql <- "
SELECT 
  COUNT(1) AS freq,
  ROUND(trip_distance, 0) AS trip_distance
FROM yellow.tripdata
GROUP BY ROUND(trip_distance, 0)
"

# takes about 20 seconds
td_freq0 <- dbGetQuery(nyc_taxi, sql)

p3 <- td_freq0  %>%
  filter(trip_distance <= 250) %>%
  ggplot(aes(x = trip_distance, y = freq)) +
  geom_line() +
  scale_y_log10(label = comma) +
  labs(x = "Trip distance in miles - rounded to nearest mile",
       y = "Number of trips (log scale)",
       title = "Trip distances",
       subtitle = "New York City yellow cabs January 2009 to mid 2016",
       caption = the_caption)

svg_png(p3, "../img/0162-trip-dist0", 10, 4)


#----------------------Single map--------------
sql <- "
WITH a AS
  (SELECT
        fare_amt                       AS fare_amt,
        ROUND(start_lon, 4)            AS round_lon,
        ROUND(start_lat, 4)            AS round_lat
  FROM yellow.tripdata
  WHERE start_lon > -74.05 AND start_lon < -73.75 AND
        start_lat > 40.58 AND start_lat < 40.90 AND
        end_lon > -74.05 AND end_lon < -73.75 AND
        end_lat > 40.58 AND end_lat < 40.90 AND
        passenger_count > 0 AND passenger_count < 7 AND
        trip_distance > 0 AND trip_distance < 100)
SELECT
  AVG(fare_amt) AS mean_fare_amt,
  COUNT(1) AS count,
  round_lon,
  round_lat
FROM a
GROUP BY  
  round_lon,
  round_lat"

# Takes about 3 minutes on my laptop:
pickups <- dbGetQuery(nyc_taxi, sql) %>% as_tibble()

# draw map:
alpha_range <- c(0.14, 0.75)
size_range <- c(0.134, 0.173)
m1 <- pickups %>%
  #complete(round_lon, round_lat, fill = list(count = 0)) %>%
  ggplot(aes(x = round_lon, y = round_lat, alpha = count, size = count)) +
  geom_point(colour = "yellow") +
  theme_dark_map() +
  scale_size_continuous(range = size_range, trans = "log", limits = range(pickups$count)) +
  scale_alpha_continuous(range = alpha_range, trans = "log", limits = range(pickups$count)) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_map() +
  theme(legend.position = "none")

# Give around one pixel width per 1/10000th a degree of longitude, and set height in proportion:
pixels_x <- diff(range(pickups$round_lon)) * 10000 + 1
pixels_y <- round((diff(range(pickups$round_lat)) * 10000 + 1) / cos(41 / 180 * pi))

CairoPNG("../img/0162-nyc-map1-hires.png", pixels_x, pixels_y, bg = "black")
print(m1)
dev.off()


#-----------------Map of credit v cash----------

sql <- "WITH a AS
  (SELECT
        fare_amt                       AS fare_amt,
        ROUND(start_lon, 4)            AS round_lon,
        ROUND(start_lat, 4)            AS round_lat,
		payment_type_code
  FROM yellow.tripdata
  WHERE start_lon > -74.05 AND start_lon < -73.75 AND
        start_lat > 40.58 AND start_lat < 40.90 AND
        end_lon > -74.05 AND end_lon < -73.75 AND
        end_lat > 40.58 AND end_lat < 40.90 AND
        passenger_count > 0 AND passenger_count < 7 AND
        trip_distance > 0 AND trip_distance < 100 AND
		payment_type_code IN (1, 2)
		)
SELECT
  AVG(fare_amt) AS mean_fare_amt,
  COUNT(1) AS count,
  round_lon,
  round_lat,
  payment_type
FROM a
INNER JOIN yellow.d_payment_type_codes AS b
	ON a.payment_type_code = b.payment_type_code
GROUP BY  
  round_lon,
  round_lat,
  payment_type"

system.time({pickups_pt <- dbGetQuery(nyc_taxi, sql) %>% as_tibble()}) # about 9 minutes

m2 <- pickups_pt %>%
  # filter out places with only 1 pickup - probably data errors and make a nasty cloud on the image:
  filter(count > 1) %>%
  ggplot(aes(x = round_lon, y = round_lat, alpha = count, size = count)) +
  geom_point(colour = "yellow") +
  theme_dark_map(72) +
  scale_size_continuous(range = size_range, trans = "log", limits = range(pickups$count)) +
  scale_alpha_continuous(range = alpha_range, trans = "log", limits = range(pickups$count)) +
  scale_fill_gradient(low = "black", high = "white") +
  coord_map() +
  facet_wrap(~payment_type) +
  theme(legend.position = "none") +
  labs(caption = the_caption,
       title = "Concentration of pickups by payment type",
       subtitle = "New York City yellow cabs January 2009 to mid 2016")

pixels_x2 <- (diff(range(pickups$round_lon)) * 10000 + 1) * 2
pixels_y2 <- round((diff(range(pickups$round_lat)) * 10000 + 1) / cos(41 / 180 * pi))

CairoPNG("../img/0162-nyc-map2-hires.png", pixels_x2, pixels_y2, bg = "black")
print(m2)
dev.off()

#-------------------modelling price of fare-----------------




#--------------hour, week, year------------
sql <- "
WITH a AS
	(SELECT
      trip_distance,
      fare_amt,
      YEAR(trip_pickup_datetime) AS yy,
      DATEPART(hh, trip_pickup_datetime) AS hd,
      DATEPART(dy, trip_pickup_datetime) AS dy,
      DATEPART(dw, trip_pickup_datetime) AS dw,
      DATEPART(m, trip_pickup_datetime) AS my
   FROM yellow.tripdata
   WHERE trip_distance > 0 AND total_amt > 0)
SELECT
  COUNT(1) AS freq,
  AVG(fare_amt / trip_distance) AS fare_per_mile,
  yy, hd, dy, dw, my
FROM a
GROUP BY yy, hd, dy, dw, my
ORDER BY yy, dy, hd"

pickup_ts <- dbGetQuery(nyc_taxi, sql) %>% as_tibble()

# Note that weekday = 1 means sunday


p4 <- pickup_ts %>%
  mutate(meta_hour = 1:n()) %>%
  ggplot(aes(x = meta_hour, y = freq, colour = as.ordered(my))) +
  facet_wrap(~yy, scales = "free_x") +
  geom_line() +
  scale_colour_viridis_d(breaks = 1:12, labels = month.abb,
                         guide = guide_legend(ncol = 2)) +
  scale_y_continuous(label = comma) +
  labs(x = "Day of the year",
       y = "Number of taxi pickups",
       colour = "",
       caption = the_caption,
       title = "Taxi pickups time series",
       subtitle = "Hourly observations 2009 to mid 2016") +
  theme(legend.position = c(0.85, 0.15))

svg_png(p4, "../img/0162-hourly", 8, 6)

p5 <- pickup_ts %>%
  group_by(yy, my) %>%
  summarise(freq = mean(freq)) %>%
  ggplot(aes(x = my, y = freq)) +
  facet_wrap(~yy) +
  geom_line() +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "",
       y = "Average hourly taxi pickups\n",
       caption = the_caption,
       title = "Taxi pickups time series",
       subtitle = "Monthly averages based on hourly observations 2009 to mid 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

svg_png(p5, "../img/0162-monthly", 8, 6)

# add a labelled day of the week variable, still keeping the ordering
pickup_ts2 <- pickup_ts %>%
  mutate(dwl = factor(dw, labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat")))

p6 <- pickup_ts2 %>%
  group_by(dwl, hd) %>%
  summarise(freq = mean(freq)) %>%
  ggplot(aes(x = as.factor(hd), y = dwl, fill = freq)) +
  geom_tile() +
  scale_fill_viridis_c(label = comma) +
  labs(caption = the_caption,
       x = "Hour of the day",
       y = "Day of the week",
       title = "Taxi frequency by time of day and day of week",
       subtitle = "Hourly observations from 2009 to mid 2016",
       fill = "Pickups\nper hour")+
  theme(legend.position = "right")

svg_png(p6, "../img/0162-heatmap1", 9, 5)

p7 <- pickup_ts2 %>%
  group_by(dwl, hd) %>%
  summarise(fare_per_mile = sum(fare_per_mile * freq) / sum(freq)) %>%
  ggplot(aes(x = as.factor(hd), y = dwl, fill = fare_per_mile)) +
  geom_tile() +
  scale_fill_viridis_c(label = dollar) +
  labs(caption = the_caption,
       x = "Hour of the day",
       y = "Day of the week",
       title = "Taxi fare per distance by time of day and day of week",
       subtitle = "Hourly observations from 2009 to mid 2016",
       fill = "fare\nper mile") +
  theme(legend.position = "right")

svg_png(p7, "../img/0162-heatmap2", 9, 5)

#-------------Modelling average fare by space-------------------

# extract data
sql <- "
SELECT 
  AVG(fare_amt) AS fare_amt,
  COUNT(1) AS freq,
  ROUND(start_lon, 3) AS start_lon,
  ROUND(start_lat, 3) AS start_lat
FROM yellow.tripdata
WHERE fare_amt IS NOT NULL AND 
      start_lon > -74.05 AND start_lon < -73.75 AND
      start_lat > 40.58 AND start_lat < 40.90 AND
      end_lon > -74.05 AND end_lon < -73.75 AND
      end_lat > 40.58 AND end_lat < 40.90 AND
      passenger_count > 0 AND passenger_count < 7 AND
      trip_distance > 0 AND trip_distance < 100
GROUP BY 
  ROUND(start_lon, 3),
  ROUND(start_lat, 3)"

fare_rounded <- dbGetQuery(nyc_taxi, sql)  # takes a while - 10 minutes or so
# about 95,000 observations

# Fit model
model <- bam(fare_amt ~ s(start_lon, start_lat), weights = freq, data = fare_rounded)

# predict values over a regular grid
all_lons <-seq(from = -74.05, to = -73.75, length.out = 200)
all_lats <- seq(from = 40.58, to = 40.90, length.out = 200)

the_grid <- expand.grid(
  start_lon = all_lons,
  start_lat = all_lats
)

predicted <- predict.bam(model, newdata = the_grid)

d <- the_grid %>%
  as_tibble() %>%
  mutate(predicted_fare = predicted) %>%
  filter(start_lat < 40.8) 


# get a background map
marg <- 0.00
nyc_map <- ggmap::get_stamenmap(bbox = c(-74.05 - marg, 40.58 - marg, 
                                         -73.75 +marg, 40.80 + marg), 
                                maptype = "toner-background")


# draw a contour map over that background:
m3 <- ggmap(nyc_map) +
  geom_raster(data = d,aes(x = start_lon,
                           y = start_lat,
                           fill = predicted_fare), alpha = 0.6) +
  geom_contour(data = d, aes(x = start_lon,
                              y = start_lat,
                              z = predicted_fare)) +
  geom_text_contour(data = d, aes(x = start_lon,
                                  y = start_lat,
                                  z = predicted_fare),
                    colour = "white") +
  theme_white_map() +
  scale_fill_viridis_c(option = "A", label = dollar) +
  # we need cartesian coordinates because of raster. Probably better ways to fix this:
  coord_cartesian() +
  labs(title = "Expected taxi fare by pick-up point in New York",
       subtitle = "New York City yellow cabs January 2009 to mid 2016",
       caption = paste(the_caption, 
                       "Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.",
                       sep = "\n"),
       fill = "Predicted\naverage fare")

svg_png(m3, "../img/0162-fare-map", 9, 8.1)

