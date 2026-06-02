
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

#' Format a number as millions or thousands
format_num <- function(x){
  y <- dplyr::case_when(
    # had to use round explicitly because of something funny happening later with pops' creation:
    x > 1e6 ~ paste0(format(round(x / 1e6, digits = 1), nsmall = 1, scientific = FALSE), "m"),
    x > 1e3 ~ paste0(format(round(x / 1e3, digits = 1), nsmall = 1, scientific = FALSE), "k"),
    TRUE    ~ as.character(round(x))
  )
  return(str_squish(y))
}

# test that function works as expected
stopifnot(format_num(1234567.1234) == "1.2m")
stopifnot(format_num(1234.1234) == "1.2k")
stopifnot(format_num(12) == "12")

# create a pops data frame, complete with country names that include
# the population in that year
pops <- proj_raw |>
  # eliminate the totals we only want male, female, and particular age groups:
  filter(sex != "_T" & age != "_T") |>
  # eliminate the subregional groupings - we only want countries / territories
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC")) |>
  # choose the right indicator
  filter(indicator == "MIDYEARPOPEST") |>
  # fiddle a bit with the age categories to make sure we have them correctly
  # in order:
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
  # join the geo_pict country codes to the full country names:
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  # calculate total population in any given year, for displaying in facet label:
  group_by(geo_pict, obs_time) |> 
  mutate(total_pop = sum(obs_value)) |> 
  # population in 2024 which we'll use for ordering the facets in the chart
  # (we want the ordering to be stable over time):
  group_by(geo_pict) |> 
  mutate(total_pop_2024 = sum(obs_value[obs_time == 2024])) |> 
  ungroup() |> 
  # Fiddle a bit with the country names so they fit nicely
  mutate(short_name = gsub("Federated States of", "Fed Sts", Name),
         short_name = gsub("Mariana Islands", "Marianas", short_name)) |> 
  mutate(pict = glue("{short_name}: {format_num(total_pop)}")) |> 
  mutate(pict = fct_reorder(pict, total_pop_2024))
  

#-----------------------Draw plot--------------------
# see https://blog.datawrapper.de/gendercolor/
pal <- c("#D4855A", "#C5CB81")
names(pal) <- c("Female", "Male")

# Reverse order so Male appears on left in legend:
pal <- pal[2:1]

ff <- "Calibri"

dir.create("tmp_pyramids", showWarnings = FALSE)

for(y in 1950:2050){

  p1 <- ggplot(filter(pops, obs_time == y), aes(y = age, fill = sex)) +
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
      width = 7000 / sc, height = 4000 / sc, res = 600 / sc, type = "cairo-png")
  print(p1)
  dev.off()

}

  # next step requires imagemagick to be installed. Takes about 30 seconds.
wd <- setwd("tmp_pyramids")
system('magick -loop -50 -delay 10 *.png "0291-pac_pyramids.gif"')
setwd(wd)

# move to where the blog expects it
# file.rename("tmp_pyramids/0291-pac_pyramids.gif", 
#             "../img/0291-pac_pyramids.gif")

