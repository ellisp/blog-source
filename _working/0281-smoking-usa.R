library(jsonlite)
library(tidyverse)


fn <- "smoking-usa.json"

if(!file.exists(fn)){
  download.file("https://data.cdc.gov/resource/7nwe-3aj9.json?submeasureid=453CGS&$where=year<2001&$limit=5000",
                destfile = fn)
}

smoking <- read_json(fn)

# number obs:
length(smoking)

# check we got the right submeasre - 
stopifnot(unique(sapply(smoking, function(x){x$submeasuredesc})) == 
            "Cigarette Consumption (Pack Sales Per Capita)")



smoking_df <- lapply(smoking, as_tibble) |>
  bind_rows() |>
  # geolocation has 3 elements so creates three rows for
  # each observation, so we just drop it column and take
  # distinct values to hack around this
  select(-geolocation) |>
  distinct() |>
  mutate(year = as.numeric(year),
        data_value = as.numeric(data_value),
        is_ca = ifelse(locationdesc == "California", "California", "Other"))

# to do - which are the states that had similar things

smoking_df |>
  ggplot(aes(x = year, y = data_value, group = locationdesc, colour = is_ca)) +
  geom_line() +
  # redraw just hte California line so it is at the forefront
  geom_line(data = filter(smoking_df, is_ca == "California"), size = 2)


smoking_df |>
  group_by(year, is_ca) |>
  summarise(data_value = mean(data_value)) |>
  ggplot(aes(x = year, y = data_value, colour = is_ca)) +
  geom_line()

