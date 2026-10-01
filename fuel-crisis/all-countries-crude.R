library(tidyverse)
library(glue)
library(janitor)

# TODO - do proper imputation of the missing values rather than just
# using fill = downup.

#-----------------Downloads----------------

# Download current year. This will get more complete month by month.
df <- here("fuel-crisis/jodi-2026.csv")
url <- "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/primaryyear2026.csv"
download.file(url, destfile = df)



# Download historical data; only needed to be done once
if(!file.exists(here("fuel-crisis/jodi-2025.csv"))){
  for(y in 2002:2025){
    df <- here(glue("fuel-crisis/jodi-{y}.csv"))
    url <- glue("https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/{y}.csv")
    download.file(url, destfile = df)
  }
}

#----------------Import data------------------

# Import all the data to R:
jodi_hist_l <- list()
for(i in 1:25){
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
  filter(energy_product == "CRUDEOIL" &
           flow_breakdown == "CLOSTLV" &
           unit_measure == "KBBL") |> 
  filter(ref_area %in% c("CN", "RU") & !is.na(as.numeric(obs_value))) |> 
  arrange(time_period) |> 
  select(time_period, obs_value, ref_area)

#----------------Summarise and draw chart------------
crude_stocks_country <- jodi_hist |> 
  # just to be sure China is systematically excluded:
  filter(ref_area != "CN") |> 
  mutate(obs_value = as.numeric(obs_value),
         date = ym(time_period)) |>
  filter(energy_product == "CRUDEOIL" &
           flow_breakdown == "CLOSTLV" &
           unit_measure == "KBBL") |> 
  filter(!is.na(obs_value)) 

countries_2026 <- crude_stocks_country |> 
  filter(year(date) == max(year(date))) |> 
  distinct(ref_area)

# Crude oil stocks (excludes NGL etc because often missing data):
crude_stocks <- crude_stocks_country |>
  # restrict to only currently reporting countries:
  inner_join(countries_2026, by = "ref_area") |>
  # fill in countries missing value with their most recent one:
  complete(ref_area, date) |> 
  group_by(ref_area) |> 
  arrange(date) |> 
  fill(obs_value, .direction = "downup") |> 
  group_by(date) |> 
  summarise(total_crude_mbbl = sum(obs_value) / 1000,
            reporting_countries = length(unique(ref_area)))

range(crude_stocks$reporting_countries)

# note the commonly used figure of 105 is for TOTCRUDE, not just CRUDEOIl (which is more ike 85)
world_use_per_day <- 85

# Summary for calculation of how long until only 10 days of cover:
war_summary <- crude_stocks |> 
  summarise(prewar = total_crude_mbbl[date == "2026-03-01"],
            latest = total_crude_mbbl[date == max(date)],
            days = max(date) - as.Date("2026-03-01")) |> 
  mutate(rate = (prewar - latest) / as.numeric(days),
         ten_days_left = (latest - world_use_per_day * 10) / rate)

# draw chart:
p <- crude_stocks |>  
  ggplot(aes(x = date, y = total_crude_mbbl)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = world_use_per_day * 10, colour = "darkred") +
  expand_limits(y = world_use_per_day * 10) +
  scale_y_continuous(
    label = comma_format(suffix = "m") ,
    sec.axis = sec_axis(
      ~ . / world_use_per_day,
      name = glue("Days of crude inventory cover,\nat {world_use_per_day} million barrels per day")
    )
  ) +
  labs(x = "",
       title = "World stocks of crude oil",
       y = "Millions of barrels",
       subtitle = glue("Millions of barrels reported to JODI-OIL, excluding countries with missing data in 2026 (eg China).
If the rate of decline since March 2026 continued, crude inventories would fall to 10 days of cover in {round(war_summary$ten_days_left / 7)} weeks."),
       caption = "Source: JODI-OIL World Database")

svg_png(p, here("fuel-crisis/world-crude-stocks"), w = 8.3, h = 4)
