library(openxlsx)
library(tidyverse)
library(foreach)
library(ggpmisc) # for selecting objects to label based on 2d density
library(ggrepel)
library(scales)
library(viridis)
library(MASS)

# Caution - this URL: "https://dhhs.vic.gov.au/moving-annual-rents-suburb" - looks like a permanent
# one but it's not, it's for a particular year.

# See https://dhhs.vic.gov.au/publications/rental-report
url <- "https://www.dhhs.vic.gov.au/sites/default/files/documents/201805/Moving%20Annual%20rents%20by%20suburb%20-%20March%20quarter%202018.xlsx"
download.file(url, destfile = "melb-rents.xlsx", mode = "wb")


sns <- getSheetNames("melb-rents.xlsx")
sns <- sns[!sns %in% "Suburb groupings"]

rents <- foreach(i = 1:length(sns), .combine = rbind) %do% {
  sheet_name <- sns[i]
  rawdata <- read.xlsx("melb-rents.xlsx", sheet = sheet_name)
  
  # Correct an annoying bug where column Z, which should be Dec 2002, is actually Dec 2003:
  the_dates <- as.character(unique(rawdata[1, ]))
  the_dates <- the_dates[!is.na(the_dates)]
  sep02 <- which(the_dates == "Sep 2002")
  the_dates[sep02 + 1] <- "Dec 2002"
  
  # read in data:
  data <- rawdata[ , -3]
  data[1, ] <- c("", "", rep(the_dates, each = 2))
  names(data) <- paste(data[1, ], data[2, ])
  names(data)[1:2] <- c("district", "area")
  data <- data [-(1:2), ]
  
  # tidy up:
  data %>%
    gather(variable, value, -district, -area) %>% 
    as_tibble %>%
    separate(variable, c("mon", "yr", "variable"), sep = "\\s") %>%
    mutate(
      yr = as.numeric(yr),
      value = as.numeric(gsub("$", "", value, fixed = TRUE)),
      property = sheet_name,
      mon_num = case_when(
        mon == "Mar" ~ 3,
        mon == "Jun" ~ 6,
        mon == "Sep" ~ 9,
        mon == "Dec" ~ 12),
      yr_mon =  yr + (mon_num - 1.5) / 12,
      ye_mar = ifelse(mon == "Mar", yr, yr + 1)) %>%
    fill(district)
}


summary(rents)         

# Rentals with 2 to 3 bedrooms
rents_melb_23 <- rents %>%
  filter(variable != "Count") %>%
  filter(grepl("Melbourne", district) & grepl("[2-3]", property))

svg("../img/0123-all-medians-melbourne-2-3.svg", 16, 11)
rents_melb_23 %>%
  mutate(district = fct_reorder(district, value, .fun = max, na.rm = TRUE)) %>%
  ggplot(aes(x = yr_mon, y = value, colour = area)) +
  facet_grid(property~district) +
  geom_line()  +
  theme(legend.position = "none") +
  scale_y_continuous("Median rent", label = dollar) +
  labs(x = "Quarter in which the move happened") +
  ggtitle("Rent at time of moving in Victoria",
          "Each line represents a suburb (not labelled)")
dev.off()


rents_melb_23_2000 <-  rents_melb_23 %>%
  filter(yr == 2000) %>%
  group_by(area, property) %>%
  summarise(mean_median_2000 = mean(value, na.rm = TRUE)) %>%
  arrange(desc(mean_median_2000))

svg('../img/0123-spaghetti-index.svg', 10, 7)
rents_melb_23 %>%
  group_by(area, property, yr_mon) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  group_by(area, property) %>%
  mutate(value = value / value[1] * 100) %>%
  left_join(rents_melb_23_2000, by = c("area", "property")) %>%
  ggplot(aes(x = yr_mon, y = value, colour = mean_median_2000, group = area)) +
  facet_wrap(~property) +
  geom_line() +
  scale_colour_viridis(option = "C", direction = -1, label = dollar, breaks = c(150, 350, 550)) +
  labs(colour = "Mean of four quarters' median rent in 2000:",
       y = "Index\n(defined to be 100 in first quarter of 2000)") +
  ggtitle("Rent at time of moving in Victoria, as an index to show growth",
          "Each line represents a suburb (not labelled)")

dev.off()

growth_melb <- rents_melb_23 %>%
  filter(ye_mar %in% c(2001, 2018)) %>%
  group_by(area, property, ye_mar) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  spread(ye_mar, value) %>%
  mutate(growth = (`2018` / `2001`) ^ (1/17) - 1)

svg("../img/0123-melbourne-scatter.svg", 8, 6)
growth_melb %>%
  ggplot(aes(x = `2001`, y = `2018`, label = area)) +
  facet_wrap(~property) +
  theme(panel.spacing = unit(1.5, "lines")) +
  geom_point() +
  geom_smooth(colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  scale_x_continuous("Average rental cost in year ending March 2001", label = dollar) +
  scale_y_continuous("Average rental cost in year ending March 2018", label = dollar) +
  ggtitle("Rental costs in Melbourne, comparing 2001 and 2018",
          "Each point represents a single area.")
dev.off()


svg("../img/0123-melbourne-growth-scatter.svg", 8, 6)
growth_melb %>%
  ggplot(aes(x = `2001`, y = growth, label = area)) +
  geom_point() +
  geom_smooth(method = "rlm", colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  facet_wrap(~property) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  scale_x_continuous("Average rent in year ending March 2001", label = dollar) +
  scale_y_continuous("Average annual growth in rent, 2001 - 2018\n", label = percent) +
  ggtitle("Growth in rental costs in Melbourne compared to price of rent in the year to March 2001",
          "Each point represents a single area.
Higher cost flats have seen materially faster growth in rental prices, but this is not evident for houses.")
dev.off()


# modelling - completely unnecessary as the effect is so strong, but for the sake of it
growth_melb$start_price <- growth_melb$"2001"
model <- lm(growth ~ start_price * property, data = growth_melb)
summary(model)
anova(model)
svg("../img/0123-diagnostics.svg", 8, 6)
par(mfrow = c(2,2), bty = "l", family = "Roboto")
plot(model)
dev.off()

#----------compare price and volume------

growth_melb_counts <-  rents %>%
  filter(variable == "Count") %>%
  filter(grepl("Melbourne", district) & grepl("[2-3]", property)) %>%
  filter(ye_mar %in% c(2001, 2018)) %>%
  group_by(area, property, ye_mar) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  spread(ye_mar, value) %>%
  mutate(growth_volume = (`2018` / `2001`) ^ (1/17) - 1) %>%
  dplyr::select(area, property, growth_volume) %>%
  left_join(growth_melb, by = c("area", "property")) %>%
  rename(growth_price = growth)

svg("../img/0123-growth-volume.svg", 8, 4)
growth_melb_counts %>%
  ggplot(aes(x = growth_volume, y = growth_price, label = area)) +
  geom_smooth(method = "rlm", colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  geom_point(size = 0.8) +
  facet_wrap(~property) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  scale_x_continuous("Average annual growth in number of recorded tenancy moves/starts, 2001-2018", label = percent) +
  scale_y_continuous("Average annual growth in median rent\n2001 - 2018\n", label = percent) +
  ggtitle("Growth in rental costs in Melbourne compared to growth in number of tenancies",
          "Each point represents a single area. Scales are equal for both axes.
Growth in volume of moves has much higher variance than does growth in price.")   +
  coord_equal()
dev.off()

convert_pngs("0123")
