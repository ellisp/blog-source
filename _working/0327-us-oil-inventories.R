library(tidyverse)
library(readxl)
library(scales)
library(glue)

fn <- "us-petrol-status-weekly.xls"

download.file(
  "https://ir.eia.gov/wpsr/psw01.xls",
  fn,
  mode = "wb"
)

us_stocks <- read_excel(fn, sheet = "Data 1", skip = 2) |>
  rename(
    crude = `Weekly U.S. Ending Stocks of Crude Oil  (Thousand Barrels)`,
    crude_spr = `Weekly U.S. Ending Stocks of Crude Oil in SPR  (Thousand Barrels)`,
    gasoline = `Weekly U.S. Ending Stocks of Total Gasoline  (Thousand Barrels)`,
    diesel = `Weekly U.S. Ending Stocks of Distillate Fuel Oil  (Thousand Barrels)`,
    date = Date
  )

#------------------plotting------------------

eia_caption <- glue("Source: Energy Information Administration (EIA). Accessed {format(Sys.Date(), '%d %B %Y')}")

# Facet plot of total crude, SPR crude, diesel and gasoline stocks
p1 <- us_stocks |>
  mutate(com_crude = crude - crude_spr) |>
  select(
    date,
    `Non-SPR crude oil` = com_crude,
    `Crude oil in SPR` = crude_spr,
    `Gasoline` = gasoline,
    `Distillate (mostly diesel)` = diesel
  ) |>
  gather(variable, value, -date) |>
  mutate(variable = fct_reorder(variable, value)) |>
  ggplot(aes(x = date, y = value / 1000)) +
  war_rect +
  facet_wrap(~variable, scales = "free_y") +
  geom_line(colour = "blue") +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  labs(
    x = "",
    y = "Thousands of barrels",
    title = "US stocks of crude oil, gasoline, and distillate fuel oil (effectively diesel)",
    subtitle = "Showing both total crude oil stocks (crude) and those in the Strategic Petroleum Reserve (crude_spr)",
    caption = eia_caption
  )

svg_png(p1, "../fuel-crisis/facet-us-stocks-latest", w = 10, h = 7)

refinery_throughput <- 17.3 # as at 24 July, operating at 97% of US capacity. Daily refinery use.
plausible_stress <- 30 * refinery_throughput
plausible_high_stress <- 24 * refinery_throughput

crude_growth_summary <- us_stocks |>
  summarise(
    latest_crude = crude[date == max(date)],
    weeks = as.numeric(as.Date(max(date)) - as.Date("2026-04-03")) / 7,
    difference = (latest_crude -
      crude[date == as.Date("2026-04-03")]) /
      1000,
    ratio = latest_crude / crude[date == as.Date("2026-04-03")],
    # growth/decline per week in million barrels e.g. 10m barrels per week:
    difference_rate = difference / weeks,
    # growth/declien rate per week:
    growth_rate = 1 - exp(log(ratio) / weeks)
  ) |>
  mutate(
    weeks_at_this_rate = (plausible_stress - latest_crude / 1000) /
      difference_rate
  )

plot_us_stocks <- function(min_date = "2020-01-01", lab_x_diff = NULL) {
  if (!"Date" %in% class(min_date)) {
    min_date <- as.Date(min_date)
  }

  if (is.null(lab_x_diff)) {
    days_shown <- as.numeric(as.Date(max(us_stocks$date))) -
      as.numeric(min_date)
    lab_x_diff <- days_shown / 50
  }

  lv <- tail(us_stocks, 1)$crude / 1000

  p2 <- us_stocks |>
    filter(date >= min_date) |>
    ggplot(aes(x = date, y = crude / 1000)) +
    war_rect +
    geom_hline(yintercept = plausible_stress, colour = "darkred") +
    geom_hline(yintercept = plausible_high_stress, colour = "red") +
    annotate(
      "text",
      x = as.Date(min_date + lab_x_diff),
      y = plausible_stress + 50,
      label = "Illustrative stress threshold:\n30 days of refinery cover",
      colour = "darkred",
      hjust = 0
    ) +
    annotate(
      "text",
      x = as.Date(min_date + lab_x_diff),
      y = plausible_high_stress - 30,
      label = "Illustrative high stress threshold:\n24 days of refinery cover",
      colour = "red",
      hjust = 0,
      vjust = 1
    ) +
    annotate(
      "text",
      x = as.Date(max(us_stocks$date) - 5e6),
      y = lv,
      label = glue(
        "{round(lv)} million barrels;\n{round(lv / refinery_throughput)} days of cover"
      ),
      size = 2.9,
      hjust = 1,
      vjust = 1,
      colour = "blue"
    ) +
    geom_line(colour = "blue") +
    expand_limits(y = 0) +
    scale_y_continuous(
      label = comma,
      sec.axis = sec_axis(
        ~ . / refinery_throughput,
        name = "Days of refinery throughput"
      )
    ) +
    labs(
      x = "",
      y = "Millions of barrels",
      title = "U.S. Total Crude Oil Stocks, including Strategic Petroleum Reserve",
      subtitle = glue(
        "Comparison of existing inventories with refinery throughput as at July 2026.
Decline since peak on 3 April 2026 is at {abs(round(crude_growth_summary$difference_rate, 1))} million barrels per week; {round(crude_growth_summary$weeks_at_this_rate)} weeks from stress threshold at this (hypothetical and linear) rate."
      ),
      caption = eia_caption
    ) +
    theme(axis.line.y = element_line(colour = "grey50"))

  frs::svg_png(p2, glue("../fuel-crisis/us-crude-from{min_date}-latest"), w = 10, h = 7)
}

plot_us_stocks("2000-01-01")
plot_us_stocks("2020-01-01")
plot_us_stocks("2025-01-01")
