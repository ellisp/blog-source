


library(tidyverse)
library(scales)
library(readxl)
library(ggrepel)

download.file("https://www.eia.gov/dnav/pet/hist_xls/W_EPC0_SAX_YCUOK_MBBLw.xls", mode = "wb",
               destfile = "cushing.xls")

cushing <- read_excel("cushing.xls", sheet = "Data 1", skip = 2) |> 
    rename(value = `Weekly Cushing, OK Ending Stocks excluding SPR of Crude Oil  (Thousand Barrels)`,
            end_date = Date)


p <- cushing |> 
    filter(end_date > as.Date("2025-12-31")) |> 
    ggplot(aes(x = end_date, y = value)) +
    geom_hline(yintercept = 20000, colour = "darkred") +
    geom_line(colour = "steelblue") +
    geom_point(colour = "steelblue") +
    geom_text(data = filter(cushing, end_date > as.Date("2026-05-01")), 
              aes(label = comma(value), x = end_date + 150000), 
              size = 2.5, hjust = 0) +
    annotate("text", x = as.Date("2026-03-02"), y = 20500, label = "Widely cited minimum working level - 20 million barrels", colour = "darkred") +
    scale_y_continuous(label = comma) +
    theme_minimal() +
    labs(x = "Month in 2026",
        y = "Thousand barrels",
        title = "Weekly Cushing, Oklahoma Stocks excluding SPR of Crude Oil",
        subtitle = "Cushing is the main US crude oil storage and pipeline hub, and the physical delivery point for the WTI oil benchmark.",
        caption = "Source: EIA https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=W_EPC0_SAX_YCUOK_MBBL&f=W")

svg_png(p, "../img/0325-cushing", w = 10, h = 5)
