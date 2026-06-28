
# download latest version of Maddison data from:
# https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023

library(tidyverse)
library(readxl)
library(scales)
library(glue)
library(countrycode)

gdppc <- read_excel("mpd2023_web.xlsx", sheet = "GDPpc", skip = 2)

#' Draw time series chart from 1900 to 2023 for a single country
#' 
#' @param ccode 3 digit ISO country code
#' @param points whether to add points for each observation(default is to just draw line)
draw_chart <- function(ccode, points = FALSE){
    # country name for this country code  
    cname <- countrycode(ccode, origin = "iso3c", destination = "country.name.en")

    # Data for just this country"
    data_tc <- gdppc |> 
        select(year, all_of(ccode)) |> 
        filter(year >= 1900)

    names(data_tc)[2] <- "value"

    constant_growth <- data_tc |>
        arrange(year) |> 
        drop_na() |> 
        summarise(n = max(year) - min(year),
                    start = value[1],
                    end = value[n()],
                    start_year = min(year),
                    # for drawing labels, not actually a 'mid' point:
                    mid_point = (end + start) / 4) |> 
        mutate(growth_rate = (end / start) ^ (1 / n) - 1)

    # colours for constant growth and for data:
    cgcol <- "red"
    dcol <- "blue"

    # define plot
    p1 <- data_tc |> 
        ggplot(aes(x = year, y = value)) +
        # Draw constant growth line:
        annotate("segment", 
                x = constant_growth$start_year, 
                xend = max(data_tc$year),
                    y = constant_growth$start, yend = constant_growth$end, 
                colour = cgcol, linetype = 2) +
        # draw data line:
        geom_line(colour = dcol) +
        annotate("text", x = 1900, y = constant_growth$mid_point, 
                label = glue("Constant growth of {percent(constant_growth$growth_rate, accuracy = 0.1)}"), 
                colour = cgcol, hjust = 0) +
        annotate("text", x = 2010, y = constant_growth$mid_point, 
                    label = "Actual GDP per capita", 
                    colour = dcol, hjust = 1) +
        scale_y_log10(label = dollar_format(accuracy = 1), 
                    breaks = c(0, 0.25, 0.5, 1:6) * 10000) +
        labs(x = "",
            y= "",
            title = glue("Long term historical growth in GDP per person in {cname}"),
            subtitle = "GDP per capita, purchasing power parity, 2011 prices.",
            caption = "Source: Maddison Project Database 2023")
  
  # for some countries with broken series we might want to draw points, not just
  # lines:
  if(points){
    p1 <- p1 + geom_point(colour = dcol)
  }

    frs::svg_png(p1, glue("../img/0326-{cname}"), w = 9, h = 5)
}

draw_chart("AUS")
draw_chart("NZL")
draw_chart("USA")
draw_chart("DNK")
draw_chart("CHN", points = TRUE)
draw_chart("IND")
draw_chart("GBR")
draw_chart("IDN")
draw_chart("JPN")
