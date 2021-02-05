
#-----------------Functionality-------------------
library(quantmod)
library(tidyverse)
library(scales)
library(Cairo)
library(glue)
library(lubridate)

#-------------Download stock data--------
symb <- "GME"
symb_name = "The GameStop stock bubble of 2021 ($GME)"
  
this_stock <- getSymbols(symb, src = 'yahoo', auto.assign = FALSE)

# will use just the last 100 values for illustration
d <- tail(this_stock, 100)
names(d) <- gsub(glue("{symb}\\."), "", names(d))

annotation_lab <- "Highest plausible actual value based on firm's revenue, etc.: $20"
annotation_col <- "steelblue"


#---------OHLC Chart plus volume bars-----------
ohlc <- function(){

par(family = main_font)
chartSeries(d, theme = chartTheme("white", 
                                                      bg.col = 'grey96', 
                                                      fg.col = "grey70",
                                                      grid.col = "white", 
                                                      border = "transparent"), 
            up.col = 'darkblue', dn.col = 'tomato', 
            log.scale = TRUE,
            name = symb_name)

# 32 is the number below chosen to appear as 20 on the y axis of top panel, and
# it totally depends on device size, scale, etc. Caution - set by hand.
# Basically like drawing a line on by hand.
abline(32, 0, col = annotation_col)
text(100, 40, annotation_lab, col = annotation_col, cex = 0.8)

}

svg_png(ohlc, "../img/0200-ohlc")


#----------------Line series with bubble points-------------

p1 <- d %>%
  as_tibble() %>%
  mutate(date = time(d)) %>%
  ggplot(aes(x = date, y = Close)) +
  geom_hline(yintercept = 20, colour = "steelblue") +
  geom_line() +
  geom_point(aes(size = Volume / 1e6), alpha = 0.5) +
  annotate("text", x = Sys.Date() - 50, y = 25,  size = 3, hjust = 1, 
           colour = annotation_col,
           label = annotation_lab) +
  scale_y_log10(label = dollar_format(accuracy = 1)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_size_area(label = comma) +
  labs(x = "",
       y = "Closing price (log scale)",
       size = "Volume (m)",
       caption = "Source: Yahoo Finance, via quantmod R package",
       title = symb_name,
       subtitle = "Line plot for price with volume shown as point size. Intuitive, but not great for showing relationship.")

svg_png(p1, "../img/0200-line-bubbles")


#---------------Connected Scatter Plot-------
p2 <- d %>%
  as_tibble() %>%
  mutate(date = time(d)) %>%
  group_by(month(date), year(date)) %>%
  mutate(direction = ifelse(Close > Open, "Bullish", "Bearish"),
         lab = ifelse(day(date) == min(day(date)) | Volume > 100e6 | Close > 300, 
                      format(date, "%e %b %y"), "")) %>%
  ungroup() %>%
  ggplot(aes(x = Volume / 1e6, y = Close, label = lab)) +
  geom_path(colour = "grey") +
  geom_point(aes(colour = direction), alpha = 0.5) +
  geom_text(aes(colour = direction), hjust = 0, size = 3, nudge_x = 0.015) +
  scale_x_log10(label = comma_format(suffix = "m", accuracy = 1),
                limits = c(1, 300)) +
  scale_y_log10(label = dollar_format(accuracy = 1)) +
  scale_colour_manual(values = c(Bullish = "darkblue", Bearish = "tomato")) +
  labs(x = "Volume", 
       y = "Closing price (log scale)",
       title = symb_name,
       colour = "Direction of sentiment over the day:",
       subtitle = "Connected scatter plot. Not intuitive, but shows relationship well.")

svg_png(p2, "../img/0200-csp",w = 8, h = 8)
