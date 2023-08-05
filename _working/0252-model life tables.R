
#-------------functionality and import data----------------
library(tidyverse)
library(readxl)
library(janitor)
library(glue)

# download.file("https://www.un.org/en/development/desa/population/publications/pdf/mortality/EMLT/MLT_UN2011_130_1y_complete.xlsx",
#               destfile = "model-life-tables.xlsx", mode = "wb")

mlt_raw <- read_excel("model-life-tables.xlsx", sheet = "Sheet1") |>
  clean_names()

#---------------exploration-----------------
mlt_raw  |>
  count(type, family)


pal <- c("brown", "darkblue")
names(pal) <- c("Female", "Male")

p1 <- mlt_raw |>
  # filter to age == 0 so we can see life expectancy at birth
  filter(age == 0) |>
  arrange(desc(mx1)) |>
  ggplot(aes(x = mx1, y = e0, colour = sex)) +
  geom_line() +
  facet_wrap(~family) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  scale_colour_manual(values = pal) +
  labs(x = "mx1 for age zero i.e. raw infant mortality",
       y = "life expectancy at birth")

svg_png(p1, "../img/0252-facets", w = 7, h = 5)

#------------animation---------
types <- unique(mlt_raw$type)  
e0s <- unique(mlt_raw$e0)


dir.create("tmp_mlt")
for(the_type in types){
  for(e in e0s){
    d <- mlt_raw |>
      filter(type == the_type & e0 == e)
    p <- d |>
      ggplot(aes(x = age, y = mx1, colour = sex)) +
      geom_line() +
      scale_colour_manual(values = pal) +
      labs(x = "Age",
           y = "Death rate",
           colour = "",
           title = glue("Model life table type = {the_type}"),
           subtitle = glue("Life expectancy = {e}")) +
      theme(legend.position = c(0.2, 0.8))
    
    png(glue("tmp_mlt/model-{the_type}_le-{e}.png"), width = 2500, height = 1500, res = 300, type = "cairo")
    print(p)
    dev.off()
  }
}

# Convert all the single frames into a GIF.
# Requires ImageMagick to be installed. Can uncomment and run it here or do 
# it directly in a system / shell window
projdir <- setwd("tmp_mlt")
system('magick -loop 0 -delay 30 *.png "model-life-tables.gif"')
setwd(projdir)


#---------prep for shiny app-----------
families <- list(
  "CD" = c("East", "North", "South", "West"),
  "UN" = c("Chilean", "Far_East_asian", "General", "Latin", "South_Asian")
)

save(mlt_raw, file = "0252-model-life-tables/mlt_raw.rda")
save(e0s, file = "0252-model-life-tables/e0s.rda")
save(families, file = "0252-model-life-tables/families.rda")
save(pal, file = "0252-model-life-tables/pal.rda")
#--------choosing one-----------
# see http://demographicestimation.iussp.org/content/fitting-model-life-tables-pair-estimates-child-and-adult-mortality
# 2 paramater method - we match an under 5 mortality rate, and some adult rate (eg prob surviving to 
# 60 if you reach the age of 15) to choose the MLT that closest matches to those two.

obs_imr <- 0.100


mlt_raw |>
  filter(age == 0) |>
  filter(sex == "Female") |>
  mutate(diff = obs_imr - mx1) |>
  group_by(family) |>
  arrange(abs(diff)) |>
  slice(1)

