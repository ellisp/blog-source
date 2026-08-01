---
layout: post
title: US oil stocks
date: 2026-08-01
tag: 
   - Timeseries
   - Energy
   - Visualisation
   - WorkRelated
description: Code for an updateable chart of crude oil stocks for the USA as a whole; and a pointer to a new page I'm maintaining of up-to-date monitoring of the fuel crisis.
image: /img/0327-us-crude-from2000-01-01.svg
socialimage: https:/freerangestats.info/img/0327-us-crude-from2000-01-01.png
category: R
---

Like many people I have been keeping track of the slow-burn fuel crisis arising from the war in Iran, its possible expansion to the Red Sea, and exacerbation from the ongoing war between Russia and Ukraine. The latter seems to have reached a stage where serious damage is being done to Russia's refining capacity and petro products' exports.

I've now started a page on this website with [a few charts on the fuel crisis](/fuel-crisis/index.html) that I'm going to be keeping up to date.

## Stocks of crude are going down, but "how much is left"?

The motivation for today's post was that I'd been seeing many news and opinion articles about how the US crude oil stocks&mdash;in total, and in the Strategic Petroleum Reserve in particular&mdash;have declined to modern record lows. But expert commentators are reluctant to show this decline in relation to the absolute amount left, as opposed to comparisons to seasonal or historic lows. This is understandable because no-one really knows (or perhaps tells) what the real "tank bottoms" are in a big complex system like the USA. A certain amount of oil, of uncertain volume but certainly in the tens (and possibly hundreds) of millions of barrels, is needed just to keep various bits of plumbing or storage from seizing up or collapsing.

So it's hard to get a chart like this one, and I had to make it myself:

<object type="image/svg+xml" data='/img/0327-us-crude-from2000-01-01.svg' width='100%'><img src='/img/0327-us-crude-from2000-01-01.png' width='100%'></object>

I've got no reputation as an oil expert to worry about, so I can safely draw those lines at a fairly arbitrary 24 (or 30) days of refinery throughput, and calculate the time it will take to get to those low levels "at the current rate". It's simplistic, but it's an important reality check to see that there might be about 19 weeks left. 

Fast as the stocks are being used up, the USA has a *lot* of oil available to it. But 19 weeks isn't that many. Markets are betting that good things will happen in the weeks or months before the blue line reaches the red line. Because of that, oil is currently selling for around $90 per barrel instead of the $110+ that would be justified if people thought we had lost the traffic through the Strait of Hormuz indefinitely.

Not that I think 19 weeks is a realistic forecast. Long before US stocks reached that red line, there would be economic and political chaos. Efforts to keep retail prices artificially low would probably be overwhelmed and there would be material demand destruction. There would also be huge incentives for the US to give away anything to restore Persian Gulf and Red Sea transit routes. 19 weeks gives us a time frame within which *something* will have to change.

The value-add for me from this chart is showing the total oil available for the US in the event this becomes a real emergency for them; the comparison of that to a meaningful if arbitrary threshold in terms of days of refinery cover; and the calculation of weeks left at current draw-down rates to reach that high stress threshold.

## Break out the SPR and also look at gasoline, distillates

The same data source has a lot of other detailed information, most of it far too detailed for me, but I'm interested enough in four key headline variables that are more detailed than "total stocks of crude oil". In this chart I break down the total stocks into those in the Strategic Petroleum Reserve, and those in more normally accessed stocks. I also show the amounts held of two key refined products:

<object type="image/svg+xml" data='/img/0327-facet-us-stocks.svg' width='100%'><img src='/img/0327-facet-us-stocks.png' width='100%'></object>

## Code

Here's the code that grabs the data from the US Energy Information Administration and draws that second plot: 

{% highlight R lineanchors %}
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

# an annotation rectangle we can use in multiple charts
war_rect <- annotate(
  "rect",
  xmin = as.Date("2026-02-28"),
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  alpha = 0.5,
  fill = "grey80"
)

# Facet plot of total crude, SPR crude, diesel and gasoline stocks
us_stocks |>
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
{% endhighlight %}

As I side point, I've started using [`air`](https://tidyverse.org/blog/2025/02/air/) to format my code. Nearly always it comes out looking better and easier to read than my manual formatting, and it's certainly more consistent. Thanks to D. Vaughan and L. Henry and the tidyverse.org project. 

And here's the code to draw my highly polished main plot, the one of total crude oil stocks with the annotations. I've tried to do this in a way that it will keep working in the months ahead with minimal maintenance, we'll see how that goes.

{% highlight R lineanchors %}
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
      x = min_date + lab_x_diff,
      y = plausible_stress + 50,
      label = "Illustrative stress threshold:\n30 days of refinery cover",
      colour = "darkred",
      hjust = 0
    ) +
    annotate(
      "text",
      x = min_date + lab_x_diff,
      y = plausible_high_stress - 30,
      label = "Illustrative high stress threshold:\n24 days of refinery cover",
      colour = "red",
      hjust = 0,
      vjust = 1
    ) +
    annotate(
      "text",
      x = max(us_stocks$date) - 5e6,
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

    svglite(
      glue("all-us-crude-from{min_date}.svg"),
      width = 10,
      height = 7
    )
    print(p2)
    dev.off()
}

plot_us_stocks("2000-01-01")
plot_us_stocks("2020-01-01")
plot_us_stocks("2025-01-01")

{% endhighlight %}

Because I did this plot in the form of a function, I have a couple of variants of the plot. One starting in 2020:

<object type="image/svg+xml" data='/img/0327-us-crude-from2020-01-01.svg' width='100%'><img src='/img/0327-us-crude-from2020-01-01.png' width='100%'></object>

And the other zoomed right in to just January 2025 and onwards:
<object type="image/svg+xml" data='/img/0327-us-crude-from2025-01-01.svg' width='100%'><img src='/img/0327-us-crude-from2025-01-01.png' width='100%'></object>

On the whole I prefer the plot I started with, showing the situation from 2000 and onwards. I'm trying to get some big picture perspective after all.

That's all for today. The images on this page will stay the same as at the time of writing; but those on [the fuel crisis monitoring page](/fuel-crisis/index.html) will be updated each week.


