library(openxlsx)
library(tidyverse)
library(foreach)
library(ggseas)
library(scales)

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

svg("../img/0123-all-medians-melbourne-2-3.svg", 16, 11)
rents %>%
  filter(variable != "Count") %>%
  filter(grepl("Melbourne", district) & grepl("[2-3]", property)) %>%
  mutate(district = fct_reorder(district, value, .fun = max, na.rm = TRUE)) %>%
  ggplot(aes(x = yr_mon, y = value, colour = area)) +
  facet_grid(property~district) +
  geom_line() +
  theme(legend.position = "none") +
  scale_y_continuous("Median rent", label = dollar) +
  labs(x = "Quarter in which the move happened") +
  ggtitle("Rent at time of moving in Victoria",
          "Each line represents a collection of suburbs (not labelled)")
dev.off()

