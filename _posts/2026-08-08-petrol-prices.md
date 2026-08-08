---
layout: post
title: NZ and US petrol ('gasoline') and diesel prices
date: 2026-08-08
tag: 
   - Timeseries
   - Energy
   - Visualisation
   - WorkRelated
description: A chart of petrol ('gasoline') prices in the USA and New Zealand since 2004 to now. Petrol is more expensive in New Zealand than the USA&mdash;around US$7 per gallon in fact.
image: /img/0329-nz-us-petrol-from-2004.svg
socialimage: https:/freerangestats.info/img/0329-nz-us-petrol-from-2004.png
category: R
---

This is another short, simple blog post building a chart I am going to be updating regularly as part of monitoring the impact of the slow-burn fuel crisis caused by the USA war with Iran.

I have a page on this website with [a few charts on the fuel crisis](/fuel-crisis/index.html).

## Petrol (or gasoline) and diesel prices

Here's today's new chart. 

<object type="image/svg+xml" data='/img/0329-nz-us-petrol-from-2004.svg' width='100%'><img src='/img/0329-nz-us-petrol-from-2004.png' width='100%'></object>

For those interested, here are some of the key features I thought through and are deliberately part of the polish here

* Both New Zealand and US prices on the same basis for direct comparability (and to shock USians with how much the rest of the world pays for gasoline).
* Mapping colour to country and using the facets for fuel type, rather than vice versa; doing it this way makes it easier to compare country-to-country (rather than fuel-to-fuel, which I found less interesting).
* Using colour in the title (thanks to `ggtext` by Claus O. Wilke and Brenton M. Wiernik) rather than a legend or direct labelling of the lines&mdash;very strong clutter-reduction technique in this context, I think.
* Annotations, in carefully chosen grey italics, to indicate the key events/periods of interest and answer the obvious questions that anyone looking at the chart (including me, this morning) would have eg "what happened in XXX?"
* Colours for countries chosen to suggest their flags.
* Make sure the audience understands the peculiar tax situation for diesel in New Zealand (diesel and electric vehicles pay a road user charge per distance travelled, not fuel consumed, which isn't included in these prices).
* I considered, and eventually decided against, forcing the vertical axis scale to go down to zero. Still a bit unsure about this one, I see pros and cons of both options.

I've also got a zoomed in version of the chart looking at just 2026. This is actually quite a bit less interesting. I like the big sweep of the past twenty years shown in the main chart, and the way we can link price changes to major events.

<object type="image/svg+xml" data='/img/0329-nz-us-petrol-from-2026.svg' width='100%'><img src='/img/0329-nz-us-petrol-from-2026.png' width='100%'></object>

## Data sources and code

So there's nothing particularly complex in the code. I needed three data sources:

* New Zealand fuel prices, provided by the Ministry of Business, Innovation and Employment
* USA fuel prices provided by the Energy Information Administration
* NZD / USD exchange rate, which I have taken from FRED, the St Louis Federal Reserve data service.

There were choices to make about exactly which series to match up and show, but not too difficult and I think I chose right.

There's also a bit of fiddling around to not download the data files every time the script is run, but only when the data is to some degree stale. The prices series are weekly so there's no point hitting the provider's server for yet another copy of the file when the last published observation was six days or less ago.

{% highlight R lineanchors %}
library(tidyverse)
library(janitor)
library(readxl)
library(patchwork)
library(ggtext)

#---------------New Zealand----------------------

# Download petrol prices from MBIE. Not sure how to determine if it is 'stale'
# or not, seems to get 10 days out of date at least.
download.file(
  "https://www.mbie.govt.nz/assets/Data-Files/Energy/Weekly-fuel-price-monitoring/weekly-table.csv",
  destfile = "nz-petrol-prices.csv"
)

# For some reason this crashes R
# nz <- read_csv("nz-petrol-prices.csv")
# so need to use read.csv instead

# The exchange rate file is a bit slow to donwload so only want to download it
# if necessary ie latest value is more than 10 days old
stale_fx <- TRUE

if (file.exists("nzd_usd.csv")) {
  nzd_usd <- read_csv("nzd_usd.csv")
  if (as.numeric(Sys.Date() - max(nzd_usd$observation_date)) < 10) {
    stale_fx <- FALSE
  }
}
if (stale_fx) {
  download.file(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DEXUSNZ",
    destfile = "nzd_usd.csv"
  )
  nzd_usd <- read_csv("nzd_usd.csv")
}


nz <- read.csv("nz-petrol-prices.csv") |>
  as_tibble() |>
  clean_names() |>
  mutate(date = as.Date(date)) |>
  filter(variable == "Adjusted retail price") |>
  left_join(nzd_usd, by = c("date" = "observation_date")) |>
  arrange(date) |>
  fill(DEXUSNZ, .direction = "down") |>
  mutate(value_usd_gallon = value * DEXUSNZ * 3.78541 / 100) |>
  mutate(fuel = ifelse(fuel == "Premium Petrol 95R", "Premium Petrol", fuel)) |>
  select(date, fuel, value_usd_gallon) |>
  mutate(country = "New Zealand")

# Adjusted retail price is
# "The national average price  paid by consumers for a given fuel for the week. "
# note, different from "Board price" which is the advertised rate Decided the
# Adjusted retail price (i.e. what actually paid) was most comparable to the USA
# series in the next section.

#------------------USA---------------------
# See https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_w.htm

stale_usa <- TRUE
if (file.exists("usa-petrol-prices.xls")) {
  tmp <- read_excel("usa-petrol-prices.xls", sheet = "Data 1", skip = 2)
  if (as.numeric(Sys.Date() - max(as.Date(tmp$Date))) < 7) {
    stale_usa <- FALSE
  }
}


if (stale_usa) {
  download.file(
    "https://www.eia.gov/dnav/pet/xls/PET_PRI_GND_DCUS_NUS_W.xls",
    destfile = "usa-petrol-prices.xls",
    mode = "wb"
  )
}

usa <- read_excel("usa-petrol-prices.xls", sheet = "Data 1", skip = 2) |>
  mutate(Date = as.Date(Date)) |>
  select(
    date = Date,
    `Regular Petrol` = `Weekly U.S. Regular All Formulations Retail Gasoline Prices  (Dollars per Gallon)`,
    `Premium Petrol` = `Weekly U.S. Premium All Formulations Retail Gasoline Prices  (Dollars per Gallon)`,
    Diesel = `Weekly U.S. No 2 Diesel Ultra Low Sulfur (0-15 ppm) Retail Prices  (Dollars per Gallon)`
  ) |>
  gather(fuel, value_usd_gallon, -date) |>
  mutate(country = "USA")

#------------combine the two----------------
combined_petrol <- usa |>
  rbind(nz) |>
  filter(date >= min(nz$date)) |>
  filter(fuel != "Premium Petrol") |>
  mutate(fuel = fct_relevel(fuel, "Regular Petrol"))

#-----------------plot drawing---------------
annotations <- tibble(
  date = as.Date(c(
    "2008-01-01",
    "2013-01-01",
    "2016-10-01",
    "2022-06-01",
    "2026-02-01"
  )),
  value_usd_gallon = 8.5,
  fuel = "Regular Petrol",
  country = "USA",
  label = c(
    "Buildup to Global\nFinancial Crisis",
    "'$100 oil plateau'",
    "US shale comes online",
    "Russia invades Ukraine",
    "USA attacks Iran"
  )
) |>
  mutate(fuel = factor(fuel, levels = levels(combined_petrol$fuel)))

# Base definition of chart, used in both versions:
p0 <- combined_petrol |>
  ggplot(aes(x = date, y = value_usd_gallon, colour = country)) +
  facet_wrap(~fuel, ncol = 1) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(label = dollar) +
  scale_colour_manual(values = c("New Zealand" = "blue", "USA" = "red")) +
  labs(
    x = "",
    colour = "",
    y = "Price (USD per gallon)",
    title = "Retail petrol and diesel prices 2004-2026, <span style='color:#0000FF;'>**New Zealand**</span> vs <span style='color:red;'>**USA**</span>, (USD/gallon).",
    subtitle = "New Zealand prices include petrol excise, GST and other taxes but exclude diesel fuel excise.",
    caption = "Source: New Zealand MBIE, USA EIA"
  ) +
  theme(legend.position = "none", plot.title = element_markdown())

# Main chart:
p1 <- p0 +
  geom_text(
    data = annotations,
    aes(label = label),
    colour = "grey40",
    vjust = 1,
    size = 2.9,
    fontface = "italic"
  )

# Zoomed in on 2026:
p2 <- p0 +
  filter(combined_petrol, date >= "2026-01-01") +
  geom_point() +
  labs(
    title = "Retail petrol and diesel prices 2026, <span style='color:#0000FF;'>**New Zealand**</span> vs <span style='color:red;'>**USA**</span>, (USD/gallon)."
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B"
  )

print(p1)
print(p2)
{% endhighlight %}

That's all for today.