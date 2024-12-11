# data were downloaded by hand from
# https://unstats.un.org/sdgs/dataportal/analytics/DataAvailability


# embed their good chart with this code
# <iframe width="800" height="600"  src="https://unstats.un.org/sdgs/dataportal/analytics/CountryWiseIndicator?&dataPointType=2&countryId=0&natureOfData=All&countryName=All countries&datapointTextPart=Data for at least two years since 2015" frameborder="0" marginheight="0" marginwidth="0"></iframe>

library(tidyverse)
d <- read_csv("at least two years since 2015.csv", skip = 7) 

rn <- function(x){
  names(x) <- c("variable", "value")
  return(x)
}

d1 <- tibble()
for (i in seq(from = 1, to = 27, by = 2)){
  d1 <- rbind(d1, rn(d[ , i:(i+1)]))
}

d1 <- d1 |>
  filter(!is.na(variable) | !is.na(value)) |>
  mutate(value = as.numeric(str_squish(gsub("%", "", value)))) |>
  separate(variable, into = c("sdg", "indicator", "subindicator"), sep = "\\.", remove = FALSE) |>
  mutate(sdg = as.numeric(sdg))


d1 |>
  group_by(sdg) |>
  summarise(number_indicators = n(),
            avg = mean(value),
            med = median(value)) |>
  arrange(desc(avg))

filter(d1, is.na(variable))
