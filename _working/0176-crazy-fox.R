
# https://twitter.com/DrHughHarvey/status/1246419945220640773
library(tidyverse)
library(scales)

fox_data <- tibble(
  cases = c(33, 61, 86, 112, 116, 129, 192, 174, 
            344, 304, 327, 246, 320, 339, 376),
  day = as.Date("2020-03-18") + 0:14)

foxcol <- "#0c244a"
foxfont <- "Montserrat"

p <- ggplot(fox_data, aes(x = day, y = cases, label = cases)) +
  geom_line(size = 3, colour = "white") +
  geom_point(size = 10, colour = "white", shape = 19) +
  geom_point(size = 10, colour = "black", shape = 1) +
  geom_text(size = 3.5, fontface = 'bold', family = foxfont) +
  theme_dark(base_family = foxfont, base_size = 10) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = foxcol),
        plot.background = element_rect(fill = foxcol),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(colour = "grey50")) +
  scale_x_date(breaks = fox_data$day, labels = format(fox_data$day, "%B\n%d")) +
  labs(x = "",
       y = "",
       title = "New Cases Per Day")


p1 <- p + labs(caption = "Standard scaling (or non-scaling) of vertical axis")

crazyfox_trans <- function(){
  scales::trans_new("crazyfox",
                    transform = function(y){
                      yt <- case_when(
                        y <= 90 ~ y / 30,
                        y <= 100 ~ 3 + (y - 90) / 10,
                        y <= 190 ~ 4 + (y - 100) / 30,
                        y <= 240 ~ 7 + (y - 190) / 50,
                        y <= 250 ~ 8 + (y - 240) / 10,
                        TRUE ~ 9 + (y - 250) / 50
                      )
                      return(yt)
                    },
                    inverse = function(yt){
                      y <- case_when(
                        yt <= 3 ~  yt * 30,
                        yt <= 4 ~ 90 + (yt - 3) * 10,
                        yt <= 7 ~ 100 + (yt - 4) * 30,
                        yt <= 8 ~ 190 + (yt - 7) * 50,
                        yt <= 9 ~ 240 + (yt - 8) * 10,
                        TRUE ~ 250 + (yt - 9) * 50
                      )
                      return(y)
                    }
  )
}

p2 <- p + 
  scale_y_continuous(trans = crazyfox_trans()) + 
  labs(caption = "Matching the eccentric scale transformation in a Fox News graphic of 4 April, with standard axis breaks")

p3 <- p + 
  scale_y_continuous(trans = crazyfox_trans(), breaks = c(30,60,90,100,130,160,190,240,250,300, 350,400,450))  + 
  labs(caption = "Matching the eccentric scale transformation in a Fox News graphic of 4 April, with Fox's axis breaks")

svg_png(p1, "../img/0176-normal", w = 8, h = 5, googlefonts = "Montserrat")
svg_png(p2, "../img/0176-weird1", w = 8, h = 5, googlefonts = "Montserrat")
svg_png(p3, "../img/0176-weird2", w = 8, h = 5, googlefonts = "Montserrat")