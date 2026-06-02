
library(tidyverse)
library(tidyquant)

# doesn't work:
tq_get("GO=F", from = "2025-01-01")  # ICE Gasoil futures

remotes::install_github("quandl/quandl-r")
library(Quandl)

Quandl.api_key("YOUR_FREE_KEY")  # Register at data.nasdaq.com

gasoil <- Quandl("CHRIS/ICE_GO1", 
                 start_date = "2025-01-01",
                 type = "xts")

head(gasoil)


data <- Quandl('NSE/OIL', type = "xts")
