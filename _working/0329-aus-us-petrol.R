library(tidyverse)
library(janitor)
library(readxl)
library(patchwork)

download.file(
    "https://www.mbie.govt.nz/assets/Data-Files/Energy/Weekly-fuel-price-monitoring/weekly-table.csv",
    destfile = "nz-petrol-prices.csv"
)

# For some reason this crashes R
# nz <- read_csv("nz-petrol-prices.csv")

#---------------New Zealand----------------------
stale_fx <- TRUE

if(file.exists("nzd_usd.csv")){
  nzd_usd <- read_csv("nzd_usd.csv")
  if(as.numeric(Sys.Date() - max(nzd_usd$observation_date)) < 10){
    stale_fx <- FALSE
  }
  }
}
if(stale_fx){
  download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DEXUSNZ",
                         destfile = "nzd_usd.csv")
  nzd_usd <- read_csv("nzd_usd.csv")
  }

  

nz <- read.csv("nz-petrol-prices.csv") |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(date = as.Date(date)) |> 
  filter(variable == "Board price") |> 
  left_join(nzd_usd, by = c("date" = "observation_date")) |> 
  fill(DEXUSNZ, .direction = "down") |> 
  mutate(value_usd_gallon = value * DEXUSNZ * 3.78541 / 100) |> 
  mutate(fuel = ifelse(fuel == "Premium Petrol 95R", "Premium Petrol", fuel)) |> 
  select(date, fuel, value_usd_gallon) |> 
  mutate(country = "New Zealand")

# Adjusted retail price is
# "The national average price paid by consumers for a given fuel for the week. "
# note, different from "Board price" which is the advertised rate

nz |> 
  ggplot(aes(x = date, y = value_usd_gallon, colour = fuel)) +
  geom_line()



#------------------USA---------------------
# See https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_w.htm

download.file("https://www.eia.gov/dnav/pet/xls/PET_PRI_GND_DCUS_NUS_W.xls",
              destfile = "usa-petrol-prices.xls", mode = "wb")

usa <- read_excel("usa-petrol-prices.xls", sheet = "Data 1", skip = 2) |> 
  mutate(Date = as.Date(Date)) |> 
  select(date = Date,
         `Regular Petrol` = `Weekly U.S. Regular All Formulations Retail Gasoline Prices  (Dollars per Gallon)`,
         `Premium Petrol` = `Weekly U.S. Premium All Formulations Retail Gasoline Prices  (Dollars per Gallon)`,
         Diesel = `Weekly U.S. No 2 Diesel Ultra Low Sulfur (0-15 ppm) Retail Prices  (Dollars per Gallon)`) |> 
  gather(fuel, value_usd_gallon, -date) |> 
  mutate(country = "USA")

combined_petrol <- usa |> 
  rbind(nz) |> 
  filter(date >= min(nz$date)) |> 
  filter(fuel != "Premium Petrol")

p1 <- combined_petrol |> 
  ggplot(aes(x = date, y = value_usd_gallon, colour = country)) +
  facet_wrap(~fuel, ncol = 1) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(label = dollar) +
  scale_colour_manual(values = c("New Zealand" = "blue", "USA" = "red")) +
  labs(x  = "", colour = "", y = "Price (USD per gallon)",
       title = "Retail petrol and diesel prices 2004-2026, New Zealand vs USA, (USD/gallon).",
       subtitle = "New Zealand prices include petrol excise, GST and other taxes but exclude diesel fuel excise.",
       caption= "Source: New Zealand MBIE, USA EIA") +
  theme(legend.position = c(0.18, 0.9))

p2 <- p1%+% filter(combined_petrol, date >= "2026-01-01") +
  geom_point() +
  labs(title = "Retail petrol and diesel prices 2026, New Zealand vs USA, (USD/gallon).")
       

p1
p2
