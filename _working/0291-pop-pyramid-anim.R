
library(tidyverse)
library(rsdmx)
library(scales)
library(janitor)
library(ISOcodes)
library(glue)
require(spcstyle)

#============Animated population pyramids=====================

if(!exists("proj_raw")){
  # This is quite slow - several minutes - but the slow part is apparently parsing
  # the XML in the as_tibble
  proj_raw <- readSDMX(providerId = "PDH", 
                       resource = "data", 
                       flowRef = "DF_POP_PROJ")  |>
    as_tibble() |>
    clean_names()
}

pops <- proj_raw |>
  filter(sex != "_T" & age != "_T") |>
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC")) |>
  filter(indicator == "MIDYEARPOPEST") |>
  mutate(age = gsub("^Y", "", age)) |>
  separate(age, into = c("from", "to"), sep = "T", remove = FALSE) |>
  mutate(age = gsub("T", "-", age),
         age = gsub("-999", "+", age, fixed = TRUE),
         sex = case_when(
           sex == "M" ~ "Male",
           sex == "F" ~ "Female"
         )) |>
  mutate(age = factor(age),
         sex = fct_relevel(sex, "Male"))|>
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  rename(pict = Name)

#-----------------------Draw plot--------------------
# see https://blog.datawrapper.de/gendercolor/
pal <- c("#D4855A", "#C5CB81")
names(pal) <- c("Female", "Male")

# Reverse order so Male appears on left in legend:
pal <- pal[2:1]

ff <- "Calibri"

dir.create("tmp_pyramids")

for(y in 1950:2050){

  p1 <- ggplot(pops, aes(y = age, fill = sex)) +
    facet_wrap(~pict, scales = "free_x", ncol = 7) +
    geom_col(data = filter(pops, sex == "Male" & obs_time == y), 
             aes(x = -obs_value)) +
    geom_col(data = filter(pops, sex == "Female" & obs_time == y), 
             aes(x = obs_value)) +
    scale_fill_manual(values = pal) +
    scale_x_continuous(label = comma) +
    theme_void(base_family = ff) +
    theme(axis.text.y = element_text(hjust = 1, size = 6),
          axis.title.x = element_text(),
          legend.position = "top",
          plot.caption = element_text(hjust = 0.5, colour = "grey20"),
          panel.background = element_rect(fill = "grey95", colour = NA),
          plot.margin = unit(c(3,3,3,3), "mm")) +
    labs(title = glue("Population estimates and projections in {y}"),
         subtitle = "Pacific Island Country and Territory members of the Pacific Community",
         x = "Number of people",
         fill = "",
         caption = "Source: UN Population Projections in the Pacific Data Hub")
  
  sc <- 5
  png(glue("tmp_pyramids/{y}.png"), 
      width = 7000 / 5, height = 4000 / 5, res = 600 / 5, type = "cairo-png")
  print(p1)
  dev.off()

}

# next step requires imagemagick to be installed
wd <- setwd("tmp_pyramids")
system('magick -loop -50 -delay 10 *.png "pac_pyramids2.gif"')
setwd(wd)


#=============changes in population projections============

# these have been downloaded in previous blog posts
indicators22  <- read_csv("data-pop-proj-2022/WPP2022_Demographic_Indicators_Medium.csv")|> 
  filter(Variant == "Medium") |> 
  select(Location, Time, TPopulation1July) |> 
  mutate(wpp_year = "2022")
         
indicators24 <- read_csv("wpp2024.csv") |> 
  filter(Variant == "Medium") |> 
  select(Location, Time, TPopulation1July) |> 
  mutate(wpp_year = "2024")

indicators_both <- bind_rows(indicators22, indicators24)

picts <- c("Papua New Guinea", "Solomon Islands", "Vanuatu", "Fiji",
               "Kiribati", "Samoa", "Tonga", "Marshall Islands",
           "Micronesia (Fed. States of)")

p2 <- indicators_both |> 
  filter(Location %in% picts) |> 
  mutate(Location = fct_reorder(Location, -TPopulation1July, .na_rm = TRUE)) |> 
  ggplot(aes(x = Time, y = TPopulation1July, colour = wpp_year)) +
  facet_wrap(~Location, scales = "free_y") +
  annotate("rect", xmin = 2024, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.2) +
  geom_line() +
  scale_y_continuous(label = comma) +
  scale_colour_manual(values = spc_cols(1:2)) +
  theme(legend.position = "right") +
  labs(x = "",
       colour = "Year of\nprojection",
       y = "Population on 1 July ('000s)",
       title = "UN World Population Prospects", 
       subtitle = "Projection made in 2024 compared to that made in 2022")

png("../img/0291-selected-picts-forecasts.png", width = 4500, height = 2300, 
    res = 600, type = "cairo-png")
print(p2)
dev.off()
