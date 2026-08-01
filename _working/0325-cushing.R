


library(tidyverse)
library(scales)
library(readxl)
library(ggrepel)
library(glue)

# Download data
download.file("https://www.eia.gov/dnav/pet/hist_xls/W_EPC0_SAX_YCUOK_MBBLw.xls", 
              mode = "wb", destfile = "cushing.xls")

# Import and process data
cushing <- read_excel("cushing.xls", sheet = "Data 1", skip = 2) |> 
      rename(value = `Weekly Cushing, OK Ending Stocks excluding SPR of Crude Oil  (Thousand Barrels)`,
             end_date = Date) |> 
      # we want a label for a point only if it is at least 500 different from
      # the subsequent point (this is to avoid clutter when the line is
      # basically horizontal)
      mutate(label = ifelse(is.na(lead(value)) | 
                            abs(value - lead(value)) > 500, 
                            comma(value / 1000, accuracy = 0.1, suffix = "m"), ""),
      # We also want the label to disappear if the value is really close to
      # 20,000, which is going to be a clearly labelled line anyway so would
      # just be unnecessary clutter.
             label = ifelse(abs(value - 20000) < 200, "", label))

# Draw chart
p <- cushing |> 
    filter(end_date > as.Date("2025-12-31")) |>
    # original data was in thousands but it's better to have it in millioms
    # visually:
    ggplot(aes(x = end_date, y = value / 1000)) +
    geom_hline(yintercept = 20, colour = "darkred") +
    geom_line(colour = "steelblue") +
    geom_point(colour = "steelblue") +
    geom_text(data = filter(cushing, end_date > as.Date("2026-05-01")), 
              aes(label = label, x = end_date + 150000), 
              size = 2.8, hjust = 0) +
    annotate("text", x = as.Date("2026-03-02"), y = 20.500, 
             label = "Widely cited minimum working level - 20 million barrels", 
             colour = "darkred") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    scale_y_continuous(label = comma) +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Month (2026)",
         y = "Million barrels",
         title = "Stocks of crude oil at Cushing, Oklahoma",
         subtitle = "Cushing is the main US crude oil storage and pipeline hub, and the delivery point for the West Texas Intermediary (WTI) oil benchmark.",
         caption = glue("Source: US Energy Information Administration (EIA). Accessed {format(Sys.Date(), '%d %B %Y')}."))

svg_png(p, "../img/0325-cushing-latest", w = 10, h = 5)
