library(WDI)
library(tidyverse)

View(WDIsearch("GDP per capita"))


# GDP per capita PPP current international prices
# GDP per capita current US$
  
d <- WDI::WDI(country = c("AU", "NZ"),
    indicator = c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.CD"))

d |>
  rename(ppp = NY.GDP.PCAP.PP.CD,
         usd = NY.GDP.PCAP.CD) |>
  # ratio to convert USD to PPP. Higher means your currency is better than
  # exchange rates make it appear (because you can buy more). A really high
  # PPP GDP compared to forex-based GDP means your prices are cheap (usually
  # seen in poor developing countries) compared to the official exchange rate:
  mutate(ppp_ratio = ppp / usd) |>
  select(country, year, ppp_ratio) |>
  spread(country, ppp_ratio) |>
  drop_na() |>
  mutate(nz_oz = `New Zealand` / Australia) |>
  tail(6)
  
