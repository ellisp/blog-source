# Visualisation of the monthly CPI data now available on PDH.Stat
library(tidyverse)
library(janitor)
library(rsdmx)
library(countrycode)

cpi_all <- readSDMX(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_CPI,3.2/M..IDX.?startPeriod=2016-01&dimensionAtObservation=AllDimensions"
) |>
  as_tibble() |>
  clean_names() |>
  mutate(month = ym(time_period)) |>
  mutate(year = year(month)) |>
  mutate(
    country = countrycode(
      geo_pict,
      origin = "iso2c",
      destination = "country.name.en"
    )
  )

# date for which we aren't interested if countries don't
# have data at this point or later:
cut_off_date <- as.Date("2026-06-01")


cpi_total <- cpi_all |>
  # filter to just total CPI:
  filter(commodity == "_T") |>
  # filter to just countries that have inflation up to the last point in the data
  group_by(country) |>
  mutate(last_date = max(month)) |>
  filter(last_date >= cut_off_date) |>
  # create a reindexed version with the reference month being just before the war:
  group_by(geo_pict, commodity) |>
  mutate(
    ref = obs_value[time_period == "2026-02"],
    reindexed = obs_value / ref * 100
  ) |>
  ungroup()

# This next step is a bit redundant if the index reference year is when
# the war started, but if we want the chart to have another reference point
# we would still need to calculate the growth rate like this
cpi_growth <- cpi_total |>
  group_by(country, last_date) |>
  summarise(
    last_value = reindexed[month == last_date],
    growth_since_war = last_value /
      reindexed[month == as.Date("2026-02-01")] -
      1
  ) |>
  ungroup() |>
  rename(month = last_date)

#----------------charts-----------
common_labels <-
  labs(
    x = "",
    y = "Index (February 2026 = 100)",
    title = "Overall inflation",
    caption = glue("Data compiled by SPC on the Pacific Data Hub; accessed {format(Sys.Date(), '%d %B %Y')}.")
  )


# Facet plot
p0 <- cpi_total |>
  filter(commodity == "_T") |>
  ggplot(aes(x = month, y = reindexed)) +
  facet_wrap(~country) +
  war_rect +
  geom_line() +
  common_labels +
  labs(
    subtitle = "'All items' Consumer Price Index in Pacific Island countries with up-to-date monthly data"
  )


svg_png(p0, "../img/0328-cpi-picts-facet-latest", w = 10, h = 5)

# All on one plotting area, but separate out years for ease of focusing on 2026:
p1 <- cpi_total |>
  filter(year >= 2024) |>
  mutate(country = fct_reorder(country, -reindexed, .fun = last)) |>
  ggplot(aes(x = month, y = reindexed, colour = country)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  war_rect +
  geom_line() +
  geom_point(
    data = filter(cpi_total, month == max(month)),
    shape = 19,
    size = 3
  ) +
  geom_label_repel(
    data = cpi_growth,
    direction = "y",
    nudge_x = +40,
    aes(
      label = percent(growth_since_war, accuracy = 0.1),
      y = last_value
    ),
    show.legend = FALSE
  ) +
  common_labels +
  labs(
    colour = "",
    subtitle = "'All items' Consumer Price Index in Pacific Island countries with recent, monthly CPI data. Total growth since February 2026 labelled."
  ) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

svg_png(p1, "../fuel-crisis/cpi-picts-latest", w = 10, h = 4.2)
