library(tidyverse)
library(glue)
library(janitor)
library(mice)
library(countrycode)

update_2026 <- FALSE

# TODO - do proper imputation of the missing values rather than just
# using fill = downup.

#-----------------Downloads----------------

if (update_2026) {
  # Download current year. This will get more complete month by month.
  df <- here("fuel-crisis/jodi-2026.csv")
  url <- "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/primaryyear2026.csv"
  download.file(url, destfile = df)
}


# Download historical data; only needed to be done once
if (!file.exists(here("fuel-crisis/jodi-2025.csv"))) {
  for (y in 2002:2025) {
    df <- here(glue("fuel-crisis/jodi-{y}.csv"))
    url <- glue(
      "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/{y}.csv"
    )
    download.file(url, destfile = df)
  }
}

#----------------Import data------------------

# Import all the data to R:
jodi_hist_l <- list()
for (i in 1:25) {
  df <- here(glue("fuel-crisis/jodi-{i+2001}.csv"))
  jodi_hist_l[[i]] <- read.csv(df) |> as_tibble()
}

jodi_hist <- bind_rows(jodi_hist_l) |>
  clean_names()

# see https://www.jodidata.org/_resources/files/downloads/oil-data/jodi-oil-wdb-item-names-ver2017.pdf
# for guide on what everything is
# CLOSTLV is closing stocks
# CRUDEOIL is just crude
# TOTCRUDE also includes NGL and refinery feedstocks, additives and other hydrocarbons

# Warning - china is not in the data. See:
jodi_hist |>
  filter(
    energy_product == "CRUDEOIL" &
      flow_breakdown == "CLOSTLV" &
      unit_measure == "KBBL"
  ) |>
  filter(ref_area %in% c("CN", "RU") & !is.na(as.numeric(obs_value))) |>
  arrange(time_period) |>
  select(time_period, obs_value, ref_area)

#-----------explore which countries missing---------------------
jodi_wide <- jodi_hist |>
  #  filter(energy_product == "CRUDEOIL") |>
  mutate(
    dimensions = paste(energy_product, flow_breakdown, unit_measure, sep = "|")
  ) |>
  mutate(obs_value = as.numeric(obs_value)) |>
  select(ref_area, time_period, dimensions, obs_value) |>
  drop_na() |>
  spread(dimensions, obs_value, fill = NA) |>
  rename(crude = `CRUDEOIL|CLOSTLV|KBBL`) |>
  complete(ref_area, time_period, fill = list(crude = NA)) |>
  mutate(
    country = countrycode(
      ref_area,
      origin = "iso2c",
      destination = "country.name.en"
    )
  ) |>
  mutate(
    ref_area = fct_relevel(ref_area, "US"),
    date = ym(time_period),
    date_n = as.numeric(date)
  )

jodi_wide |>
  ggplot(aes(x = date, y = crude / 1000, colour = ref_area)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "Millions of barrels", title = "Stocks of crude oil")

possible_obs <- length(unique(jodi_wide$time_period))

country_sum <- jodi_wide |>
  group_by(ref_area, country) |>
  summarise(
    crude_total = mean(crude, na.rm = TRUE),
    missing_obs = sum(is.na(crude))
  ) |>
  # particular problem if a country has any missing observations and averages
  # 10 million barrels or more of stock:
  mutate(
    some_missing = as.logical(missing_obs > 0 & missing_obs < possible_obs),
    problem = as.logical(crude_total > 10000 & missing_obs > 0)
  ) |>
  arrange(desc(crude_total))

# 20 countries that never have any observations for Crude:
never_crude <- filter(country_sum, is.na(problem))
print(never_crude)
# Particular obvious problems, likely to be significant: China, Hong Kong,
# Bangladesh, Singapore, Vietnam, Yemen, Malaysia

country_sum |>
  ggplot(aes(
    x = crude_total,
    y = missing_obs,
    label = ref_area,
    colour = problem
  )) +
  geom_text() +
  scale_x_log10(label = comma) +
  labs(
    x = "Average crude stocks (thousands of barrels)",
    y = "Number of months missing an observation"
  )

# The nine biggest problem countries in terms of partly missing data
jodi_wide |>
  #  filter(ref_area %in% filter(country_sum, some_missing)$ref_area) |>
  filter(ref_area %in% filter(country_sum, problem)$ref_area) |>
  mutate(country = fct_reorder(country, crude)) |>
  ggplot(aes(x = date, y = crude / 1000)) +
  facet_wrap(~country, scales = "free_y") +
  geom_line(colour = "steelblue") +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  labs(
    x = "",
    y = "Millions of barrels",
    title = "Crude oil stocks of countries missing at least one data point"
  )


#----------------Summarise and draw chart------------

# Crude oil stocks (excludes NGL etc because often missing data):
crude_stocks <- jodi_wide |>
  left_join(select(country_sum, ref_area, some_missing), by = "ref_area") |>
  group_by(date, some_missing) |>
  summarise(
    total_crude_mbbl = sum(crude, na.rm = TRUE) / 1000,
    reporting_countries = length(unique(ref_area))
  ) |>
  ungroup()

crude_stocks |>
  ggplot(aes(x = date, y = total_crude_mbbl, fill = some_missing)) +
  geom_area()


# note the commonly used figure of 105 is for TOTCRUDE, not just CRUDEOIl (which is more ike 85)
world_use_per_day <- 85

# Summary for calculation of how long until only 10 days of cover:
war_summary <- crude_stocks |>
  # restrict it to countries that have observations for all months
  filter(!some_missing) |>
  summarise(
    prewar = total_crude_mbbl[date == "2026-03-01"],
    latest = total_crude_mbbl[date == max(date)],
    days = max(date) - as.Date("2026-03-01")
  ) |>
  mutate(
    rate = (prewar - latest) / as.numeric(days),
    ten_days_left = (latest - world_use_per_day * 10) / rate
  )

# draw chart:
p <- crude_stocks |>
  filter(!some_missing) |>
  ggplot(aes(x = date, y = total_crude_mbbl)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = world_use_per_day * 10, colour = "darkred") + #expand_limits(y = world_use_per_day * 10) +
  expand_limits(y = 0) +
  scale_y_continuous(
    label = comma_format(suffix = "m"),
    sec.axis = sec_axis(
      ~ . / world_use_per_day,
      name = glue(
        "Days of crude inventory cover,\nat {world_use_per_day} million barrels per day"
      )
    )
  ) +
  labs(
    x = "",
    title = "World stocks of crude oil",
    y = "Millions of barrels",
    subtitle = glue(
      "Excluding countries with any missing data (eg China, Russia, India, Venezuela).
If the rate of decline since March 2026 continued, crude inventories would fall to 10 days of cover in {round(war_summary$ten_days_left / 7)} weeks."
    ),
    caption = "Source: JODI-OIL World Database"
  )

svg_png(p, here("fuel-crisis/world-crude-stocks"), w = 8.3, h = 4)
