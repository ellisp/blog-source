library(nzelect)
library(ggmap)
library(tidyverse)
library(scales)

# try out three methods of geocoding
# ggmap::geocode (Google maps geocoding - check)
# https://github.com/ropensci/opencage
# https://github.com/ropensci/geonames 

set.seed(321)

vp <-  voting_places %>%
  filter(election_year == 2014) %>%
  sample_n(1500) %>%
  mutate(full_name = paste0(voting_place, ", ", voting_place_suburb, ", New Zealand"))
head(vp)

#------------------ggmap::geocode------------------
# This is the simplest to use as there is no authentication necessary
# oops, maybe should have added "New Zealand" to the end of the addresses...

gg_google_codes <- geocode(vp$full_name, source = "google")

gg_dsk_codes <- geocode(vp$full_name, source = "dsk")

#---------------geonames---------------------
options(geonamesUsername="myusername") 



#-----------------opencage----------------------------
# 6fdc21a8468b4b349bf3148f600ded02