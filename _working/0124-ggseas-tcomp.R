library(tidyverse)
library(scales)
library(ggseas)
library(Tcomp)


?Tcomp

which(sapply(tourism, function(x){x$period}) == "MONTHLY")

svg("../img/0124-plot.svg", 8, 4)
# default plot method for forecasting competition datasets of class Mdata
par(family = "main_font", bty = "l", font.main = 1)
plot(tourism[["M4"]], main = "Series M4 from the tourism forecasting competition")
dev.off()

# convert a time series to a data frame
the_data <- ggseas::tsdf(tourism[["M4"]]$x)

svg("../img/0124-stat_seas.svg", 8, 4)
# draw a graphic
ggplot(the_data, aes(x = x, y = y, colour = 1)) +
  geom_line(aes(colour = "Original")) +
  stat_seas(aes(colour = "Seasonally adjusted")) +
  stat_rollapplyr(aes(colour = "12 month rolling average"), width = 12) +
  scale_colour_manual(values = c("Original" = "grey50", "Seasonally adjusted" = "red", 
                                 "12 month rolling average" = "blue")) +
  theme(legend.position = c(0.2, 0.8)) +
  scale_y_continuous("Unknown units\n", label = comma) +
  labs(x = "", colour = "") +
  ggtitle("Comparison of statistical summary methods in plotting a time series",
          "Monthly series 4 from the Tourism forecasting competition")
dev.off()


svg("../img/0124-ggsdc.svg", 8, 6)
# draw a graphic
ggsdc(the_data, aes(x = x, y = y), method = "seas") +
  geom_line() +
  ggtitle("Seasonal decomposition with `X13-SEATS-ARIMA`, `seasonal` and `ggseas`",
          "Monthly series 4 from the Tourism forecasting competition") +
  scale_y_continuous("Unknown units\n", label = comma) +
  labs(x = "")
dev.off()

convert_pngs("0124")
