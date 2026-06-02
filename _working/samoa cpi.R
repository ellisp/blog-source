library(tidyverse)
library(rsdmx)
library(janitor)
library(lubridate)

items <- tibble(item_label = c(
  "All items",
  "Food and non-alcoholic beverages",
  "Alcoholic beverages, tobacco and narcotics",
  "Clothing and footwear",
  "Housing, water, electricity, gas and other fuels",
  "Furnishings, household equipment and routine household maintenance",
  "Health",
  "Transport",
  "Communication",
  "Recreation and culture",
  "Education",
  "Restaurants and hotels",
  "Miscellaneous goods and services"),
  item = c("_T", paste0("ITEM_", sprintf("%02d", 1:12)))) |> 
  mutate(item_label = factor(item_label, levels = item_label))



samoa_cpi <- readSDMX("https://data-sdmx-disseminate.sbs.gov.ws/rest/data/SBS,DF_CPI,1.0/M..ALLCPI.IDX._T+ITEM_01+ITEM_02+ITEM_03+ITEM_04+ITEM_05+ITEM_12+ITEM_11+ITEM_10+ITEM_09+ITEM_08+ITEM_07+ITEM_06.N?startPeriod=2019-01&dimensionAtObservation=AllDimensions") |> 
  as_tibble() |> 
  clean_names() |> 
  left_join(items, by = "item") |> 
  select(time_period, item, item_label, obs_value) |> 
  arrange(time_period) |> 
  mutate(month_ending = ceiling_date(ym(time_period), "month") - days(1))


# prices Feb 2016 = 100

samoa_cpi |> 
  ggplot(aes(x = month_ending, y = obs_value, colour = item_label)) +
  geom_line()

tail(samoa_cpi)
max(samoa_cpi$month_ending)
