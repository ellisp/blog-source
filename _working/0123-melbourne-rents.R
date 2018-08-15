library(openxlsx)
library(tidyverse)
library(foreach)
library(ggpmisc) # for selecting objects to label based on 2d density
library(ggrepel)
library(scales)
library(viridis)
library(MASS)

# https://dhhs.vic.gov.au/moving-annual-rents-suburb

url <- "https://dhhs.vic.gov.au/sites/default/files/documents/201708/Moving-annual-rents-by-suburb-March-quarter-2017.xlsx"
download.file(url, destfile = "melb-rents.xlsx", mode = "wb")


sns <- getSheetNames("melb-rents.xlsx")
sns <- sns[!sns %in% "Suburb groupings"]

rents <- foreach(i = 1:length(sns), .combine = rbind) %do% {
  sheet_name <- sns[i]
  rawdata <- read.xlsx("melb-rents.xlsx", sheet = sheet_name)
  
  the_dates <- as.character(unique(rawdata[1, ]))
  the_dates <- the_dates[!is.na(the_dates)]
  sep02 <- which(the_dates == "Sep 2002")
  the_dates[sep02 + 1] <- "Dec 2002"
  
  data <- rawdata[ , -3]
  data[1, ] <- c("", "", rep(the_dates, each = 2))
  names(data) <- paste(data[1, ], data[2, ])
  names(data)[1:2] <- c("district", "area")
  data <- data [-(1:2), ]
  
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
      yr_mon =  yr + (mon_num - 1.5) / 12) %>%
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
          "Each line represents a collection of suburbs (not labelled)")
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
  scale_colour_viridis(option = "C", direction = -1)
dev.off()

growth_melb <- rents_melb_23 %>%
  filter(yr %in% c(2000, 2016)) %>%
  group_by(area, property, yr) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  spread(yr, value) %>%
  mutate(growth = (`2016` / `2000`) ^ (1/16) - 1)

svg("../img/0123-melbourne-scatter.svg", 8, 6)
growth_melb %>%
  ggplot(aes(x = `2000`, y = `2016`, label = area)) +
  facet_wrap(~property) +
  theme(panel.spacing = unit(1.5, "lines")) +
  geom_point() +
  geom_smooth(colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  scale_x_continuous("Average rental cost in 2000", label = dollar) +
  scale_y_continuous("Average rental cost in 2016", label = dollar) +
  ggtitle("Rental costs in Melbourne, comparing 2000 and 2016",
          "Each point represents a single area.")
dev.off()


svg("../img/0123-melbourne-growth-scatter.svg", 8, 6)
growth_melb %>%
  ggplot(aes(x = `2000`, y = growth, label = area)) +
  geom_point() +
  geom_smooth(method = "rlm", colour = "orange") +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.05, 
                     size = 2, colour = "steelblue", min.segment.length = 0.2) +
  facet_wrap(~property) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  scale_x_continuous("Average rent in year 2000", label = dollar) +
  scale_y_continuous("Average annual growth in rent, 2000 - 2016\n", label = percent) +
  ggtitle("Growth in rental costs in Melbourne compared to price of rent in 2000",
          "Each point represents a single area.
Higher cost flats have seen materially faster growth in rental prices, but this is not evident for houses.")
dev.off()


# modelling - completely unnecessary as the effect is so strong, but for the sake of it
growth_melb$start_price <- growth_melb$"2000"
model <- lm(growth ~ start_price * property, data = growth_melb)
summary(model)
anova(model)
par(mfrow = c(2,2), bty = "l", family = "Roboto")
plot(model)
