library(tidyverse)
library(openxlsx)
library(lubridate)
library(forecast)

#==============download data=================

dir.create("sl-tourism")

# From the actual website for the detailed data by country, just for 2018:
download.file("http://www.sltda.gov.lk/sites/default/files/monthly-international-tourist-arrivals-2018.xlsx",
              destfile = "sl-tourism/monthly-international-tourist-arrivals-2018.xlsx",
              mode = "wb")

# From my home-made hack for the past years, totals only:
download.file("https://github.com/ellisp/blog-source/raw/master/_working/sl-tourism/monthly-totals.xlsx",
              destfile = "sl-tourism/monthly-totals.xlsx",
              mode = "wb")

# There are PDFs of detail by country by month (one PDF for each year-month combination) but I wasn't able to 
# systematically scrape them all in in the time available.

#==============total arrivals by month========================

# file name:
fn <- "sl-tourism/monthly-totals.xlsx"
# sheet names:
sns <- 2018:2015
# handy vector of month titles in full for later use:
mons <- month(1:12, label = TRUE, abbr = FALSE)

# Import all sheets
all_sheets <- lapply(sns, function(x){
  tmp <- read.xlsx(fn, sheet = as.character(x))[, 1:3]
  tmp <- tmp %>%
    gather(year_orig, value, -Month) %>%
    mutate(year = as.numeric(str_sub(year_orig, 1, 4)),
           status = ifelse(grepl("\\*$", year_orig), "Provisional", "Final"),
           Month = factor(Month, levels = levels(mons)),
           value = as.numeric(value)) 
  return(tmp)
  })

# Combine into a single data frame and tidy up a bit eg remove duplicates:
monthly <- do.call("rbind", all_sheets) %>%
  mutate(yrmon = year + (as.numeric(Month) - 0.5) / 12) %>%
  filter(status == "Final" | year == 2018) %>%
  arrange(yrmon)

#-------------forecast-------------------
# put into a time series object
monthly_ts <- ts(monthly$value, frequency = 12, start = c(2014, 1))
# check what sort of transformation makes sense. Recommends < 0, so will use 0 (log) for simplicity:
BoxCox.lambda(monthly_ts)

# fit model:
mod <- auto.arima(monthly_ts, lambda = 0)

# draw graph with forecast:
svg("../img/0134-forecast.svg", 8, 5)
autoplot(forecast(mod, h = 18)) +
  scale_y_continuous("Visitor arrivals to Sri Lanka per month\n(Shaded blue area shows forecasts and uncertainty)", 
                     label = comma) +
  coord_cartesian(ylim = c(80000, 300000)) +
  labs(x = "Growth has slowed slightly since 2015")
dev.off()  
  
#--------------more detailed seasonality plot----------------
svg("../img/0134-seasonality.svg", 8, 5)
monthly %>%
  ggplot(aes(x = as.numeric(Month), y = value, colour = as.ordered(year))) +
  geom_line(size = 2) +
  scale_x_continuous("", breaks = 1:12, labels = month(1:12, label = TRUE, abbr = TRUE), minor_breaks = NULL) +
  scale_color_brewer("Year", palette = "Oranges", guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous("Arrivals per month", label = comma) +
  theme(legend.position = "right") +
  ggtitle("Visitor arrivals to Sri Lanka by month",
          "Two peak seasons (Dec-Mar and Jul-Aug) are clearly visible, as is the strong growth in recent years.")
  
dev.off()


       


#================country detail==================
# Import and tidy the detailed country data for first months of 2018:
detail <- read.xlsx("sl-tourism/monthly-international-tourist-arrivals-2018.xlsx",
                    startRow = 4) %>%
  # get rid of the total:
  filter(X1 != "TOTAL") %>%
  # only keep the COUNTRY and the columns that are months:
  select(COUNTRY, one_of(toupper(mons))) %>%
  # tidy:
  rename(country = COUNTRY) %>% 
  gather(month, value, -country) %>%
  filter(!is.na(value)) %>%
  mutate(country2 = str_to_title(fct_lump(country, n = 15, w = value)),
         country2 = gsub(" Of", " of", country2)) %>%
  group_by(country2, month) %>%
  summarise(value = sum(value)) %>%
  ungroup(month) %>%
  mutate(month = factor(str_to_title(month), levels = levels(mons)),
         country2 = fct_reorder(country2, -value)) 

# Straight barchart of numbers:
svg("../img/0134-country-barchart.svg", 9, 6)
detail %>%
  ggplot(aes(x = as.numeric(month), weight = value)) +
  geom_bar() +
  facet_wrap(~country2) +
  scale_x_continuous("", breaks = 1:12, labels = month(1:12, label = TRUE, abbr = TRUE), minor_breaks = NULL) +
  scale_y_continuous("Visitor arrivals per month", label = comma) +
  ggtitle("Visitor arrivals to Sri Lanka by country and month", 
          "Top 15 countries, first eight months of 2018.  Differing seasonal patterns for different market countries are clear.")
dev.off()

# Calculate proportions so can see different countries on the same scale:
detail2 <- detail %>%
  group_by(country2) %>%
  mutate(prop = value / sum(value))

svg("../img/0134-country-seasonality.svg", 9, 9)
detail2 %>%
  ggplot(aes(x = as.numeric(month), y = prop)) +
  scale_x_continuous("", breaks = 1:12, labels = month(1:12, label = TRUE, abbr = TRUE), minor_breaks = NULL) +
  scale_y_continuous("Percentage of first eight months' visitor arrivals", label = percent) +
  facet_wrap(~country2) +
  geom_line() +
  ggtitle("Different peak seasons for different countries")
dev.off()

#------------------Principal components analysis-------

# make wide version of the data (one column per month):
detail3 <- detail2  %>%
  select(country2, month, prop) %>%
  spread(month, prop)

# convert to a matrix and fit principal components to it:
detail_m <- as.matrix(detail3[ , -1])
row.names(detail_m) <- detail3$country2
detail_pc <- prcomp(detail_m)

# visualize:
svg("../img/0134-country-seasonality-pc.svg", 8,7)
par(family = "Roboto", font.main = 1)
par(bg = "grey99")
biplot(detail_pc, choices = 1:2, col = c("darkblue",  "grey75"), pc.biplot = TRUE,
       xlim = c(-3, 3), xlab = "Countries with peaks in the same months are grouped close together", ylab = "", 
       main = "Comparison of seasonality patterns by country",
       sub = "(Data only from the first eight months of 2018)", col.axis = "transparent")
dev.off()


convert_pngs("0134")
