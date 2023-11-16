

library(R.utils)
library(tidyverse)
library(jsonlite)


download.file("https://downloads.spansh.co.uk/galaxy_populated.json.gz",
              destfile = "galaxy_populated.json.gz", mode = "wb")

gunzip("galaxy_populated.json.gz", remove = FALSE)
# short <- readLines("galaxy_populated.json", n = 100) # and hand edit to add missing ]
# writeLines(short, "short_galaxy.json")
# d_raw <- jsonlite::fromJSON("short_galaxy.json")

# prob is this is 9 gigs, can it be read?:
d_raw <- jsonlite::fromJSON("galaxy_populated.json")

# str(d_raw)
# length(d_raw)
# lapply(d_raw, length)
# names(d_raw)

systems <- with(d_raw,
  tibble(
    id64 = as.character(id64),
    name = as.character(name),
    population = as.character(population),
    powers = as.character(powers),
    powerState = as.character(powerState),
    primaryEconomy = as.character(primaryEconomy),
    secondaryEconomy = as.character(secondaryEconomy),
    security = as.character(security),
    date = as.character(date)
  )
)


factions <- bind_rows(d_raw$factions) |>
  mutate(id64 = rep(systems$id64, sapply(d_raw$factions, nrow)))


