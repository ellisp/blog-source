
library(tidyverse)
library(WDI)


WDIsearch("disposable")


d <- WDI(indicator = "NY.GNY.TOTL.CN") |> 
  as_tibble()


unique(d$country)


165 * 6 * 4
