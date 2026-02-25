# this chart draws population pyramids for 1980 and 2025, firstly
# for Marshall Islands and Kiribati together for comparison 
# purposes, and then for each of the 21 PICTs (exlcuding Pitcairn)
# so we can pick and choose which ones
#
# Peter Ellis November 2025

library(tidyverse)
library(janitor)
library(rsdmx)
library(ISOcodes)
library(glue)

# Download all population data needed
if(!exists("pop2picts")){
  pop2picts <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A.AS+CK+FJ+PF+GU+KI+MH+FM+NR+NC+NU+MP+PW+PG+WS+SB+TK+TO+TV+VU+WF.MIDYEARPOPEST.F+M.Y00T04+Y05T09+Y10T14+Y15T19+Y20T24+Y25T29+Y30T34+Y35T39+Y40T44+Y45T49+Y50T54+Y55T59+Y60T64+Y65T69+Y70T999?startPeriod=1980&endPeriod=2025&dimensionAtObservation=AllDimensions") |> 
    as_tibble() |> 
    clean_names()
}

# sort out the from and to ages, rename sex, and add country labels
d <- pop2picts |> 
  mutate(age = gsub("^Y", "", age)) |>
  separate(age, into = c("from", "to"), sep = "T", remove = FALSE) |>
  mutate(age = gsub("T", "-", age),
         age = gsub("-999", "+", age, fixed = TRUE),
         sex = case_when(
           sex == "M" ~ "Male",
           sex == "F" ~ "Female"
         )) |>
  mutate(age = factor(age)) |>
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  rename(pict = Name) |> 
  filter(time_period %in% c(1980, 2025))

#----------Marshalls and Kiribati-------------
# subset data to these two countries:
d1 <- d |> 
  filter(pict %in% c("Kiribati", "Marshall Islands"))

# breaks in axis for Marshall and Kiribati chart:
x_breaks <- c(-5000, -2500, 0, 2500, 5000)

# draw chart:
pyramid_km <- d1 |> 
  # according to Wikipedia males are usually on the left and females on the right
  filter(sex == "Female") |> 
  ggplot(aes(y = age)) +
  facet_grid(pict ~ time_period) +
  geom_col(aes(x = obs_value), fill = "brown") +
  geom_col(data = filter(d1, sex == "Male"), aes(x = -obs_value), fill = "steelblue") +
  labs(x = "", y = "Age group") +
  scale_x_continuous(breaks = x_breaks, labels = c("5,000\n(male)", "2,500", 0 , "2,500", "5,000\n(female)")) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(size = 14, face = "bold"))

print(pyramid_km)


#--------------population pyramid individual image for each pict-----------
# This section draws one chart and saves as an image for each PICT
dir.create("pic-pyramids")

all_picts <- unique(d$pict)

for(this_pict in all_picts){
  this_d <- d |> 
    filter(pict == this_pict)

  this_pyramid <- this_d  |> 
    filter(sex == "Female") |> 
    ggplot(aes(y = age)) +
    facet_grid(pict ~ time_period) +
    geom_col(aes(x = obs_value), fill = "brown") +
    geom_col(data = filter(this_d, sex == "Male"), aes(x = -obs_value), fill = "steelblue") +
    labs(x = "", y = "Age group") +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(size = 14, face = "bold"))

  png(glue("pic-pyramids/pop-mobility-wellington-2025-11-03/pyramid-{this_pict}.png"), width = 5000, height = 2800, 
      res = 600, type = "cairo-png")
  print(this_pyramid)
  dev.off()

}
