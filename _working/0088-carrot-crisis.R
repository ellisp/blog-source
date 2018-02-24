library(tidyverse)
library(ggseas)
library(scales)
library(ggrepel)

fpi_orig <- read.csv("../data/CPI324701_20170313_040440_6.csv", skip = 1, stringsAsFactors = FALSE, check.names = FALSE) 

names(fpi_orig)[1] <- "Period"

fpi <- fpi_orig %>%
   filter(!is.na(`Oranges, 1kg`)) %>%
   gather(Item, Price, -Period) %>%
   mutate(Price = as.numeric(Price),
          Year = as.numeric(substring(Period, 1, 4)),
          Month = as.numeric(substring(Period, 6, 7)),
          TimePeriod = Year + (Month - 0.5) / 12) %>%
   select(-Year, -Month, -Period)

all_items <- unique(fpi$Item)

set.seed(123)
fpi_sub <- fpi %>%
   filter(Item %in% sample(all_items, 15))

fpi_sub %>%
   ggplot(aes(x = TimePeriod, y = Price, colour = Item)) +
   geom_line() +
   theme(legend.position= "none") +
   geom_text(data = filter(fpi_sub, TimePeriod == max(TimePeriod)),
             aes(label = Item), x = 2017.5, hjust = 0, size = 2) +
   xlim(c(2006, 2020)) +
   labs(x = "", caption = "Source: Statistics New Zealand Food Price Index") +
   scale_y_continuous(label = dollar) +
   ggtitle("Price of 15 randomly selected food items in New Zealand")

library(directlabels)

set.seed(123)
p <- fpi %>% 
   filter(Item %in% sample(all_items, 20)) %>%
   filter(TimePeriod > 2011.5) %>%
   ggplot(aes(x = TimePeriod, y = Price, colour = Item)) +
   stat_stl(index.ref = 1:12, s.window = 7) +
   xlim(c(2011, 2020)) +
   labs(x = "", y = "Index (mid 2011 to mid 2012 = 100)",
        caption = "Source: Statistics New Zealand Food Price Index") +
   ggtitle("Price index of 20 randomly selected food items in New Zealand")
direct.label(p)

fpi %>%
   filter(grepl("Carrot", Item) | grepl("Apple", Item) | grepl("Potatoes", Item)) %>%
   ggsdc(aes(x = TimePeriod, y = Price, colour = Item), method = "stl", s.window = 7) +
   geom_line() +
   scale_y_continuous(label = dollar) +
   labs(x = "")

