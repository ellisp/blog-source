# Draws an animation of choropleth maps of the pacific with SDGs. Used to illustrate
# the power of the Pacific Data Hub PDH.Stat.

#-----------------functionality-----------------

library(tidyverse)
# remotes::install_github("ellisp/frs-r-package/pkg") # needed for draw_pac_map() 
library(frs)

#---------------minimal demo--------------

# grey
p <- draw_pac_map()
svg_png(p, "../img/0249-blank-map",w = 8, h = 5.5)

# Human Development Index
library(readxl)
library(ISOcodes)
download.file("https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Statistical_Annex_HDI_Table.xlsx",
              destfile = "hdi.xlsx", mode = "wb")

hdi <- read_excel("hdi.xlsx", skip = 7, sheet = "Table 1") |>
  rename(Name = `VERY HIGH HUMAN DEVELOPMENT`,
         hdi2021 = ...3) |>
  select(Name, hdi2021) |>
  mutate(hdi2021 = as.numeric(hdi2021)) |>
  filter(!is.na(hdi2021)) |>
  left_join(select(ISO_3166_1, iso3 = Alpha_3, Name), by = "Name")

# check to see if any failed to match names
#View(filter(d, is.na(iso3)))

p2 <- draw_pac_map(fill_df = hdi, join_col = "iso3", fill_col = "hdi2021",
                   fill_col_label = "Human\nDevelopment\nIndex 2021",
                   family = "Calibri",
                   country_label_col = "white") +
  scale_fill_viridis_c(option = "A")
svg_png(p2, "../img/0249-hdi-map",w = 8, h = 5.5)


#==================animated map===================
#-----------------------Get the SDG data and metadata---------------
library(rsdmx)
library(janitor)
library(glue)

dataflow <- "DF_SDG"

# metadata ie code lists. Warning this next function, in frs, is a bit of a hack
# and is subject to change!
d_code_list <- get_pdh_codelists(dataflow = dataflow, version = "3.0")

# data
d_raw <- readSDMX(providerId = "PDH", resource = "data", flowRef = dataflow) |> 
  as_tibble()

# join the data to the code list so we have labels
d <- d_raw |>
  left_join(filter(d_code_list, category_en == "Codelist for SDG indicators"),
            by = c("INDICATOR" = "id")) |>
  select(-category_en, -category_fr) |>
  rename(indicator_full = name) |>
  clean_names() 

# for simplicity, limit ourselves just to indicators that are totals eg all
# age groups, not individual age groups, not disaagregated by sex, etc.
# this loses a lot of data of course but is ok because it is just for illustration
# to coose the indicators most suitable for choropleth matps
d_totals <- d |>
  filter(
    sex == "_T" &
      age == "_T" &
      urbanization == "_T" &
      income == "_T" &
      education == "_T" &
      occupation == "_T" &
      composite_breakdown == "_Z"  &
      disability == "_T"
  ) |>
  group_by(indicator_full, geo_pict) |>
  arrange(desc(obs_time)) |>
  slice(1) |>
  ungroup()



indicator_summary <- d_totals |>
  group_by(indicator, indicator_full) |>
  summarise(countries = length(unique(geo_pict)),
            years = length(unique(obs_time)),
            n = n()) |>
  arrange(desc(countries))


# what indicators are we going to use, in a simple vector
inds <- pull(indicator_summary, indicator_full)

#--------------------draw the maps------------------

dir.create("pac-maps", showWarnings = FALSE)

for(i in 1:length(inds)){
  the_ind <- inds[i]
  the_data <- filter(d_totals, indicator_full == the_ind) 
  
  
  units <- the_data |>
    count(unit_measure, sort = TRUE) |>
    pull(unit_measure)
  
  if(length(units) > 1){
    warning("More than 1 unit measure, choosing the most common")
    the_data <- filter(the_data, unit_measure == units[1])
  }
  
  
  unit_title <- stringr::str_to_title(gsub("_", " ", units[1])) |>
    str_replace("Usd", "USD") |>
    str_replace("Ha ", "Hectares ") |>
    str_replace("Ha$", "Hectares") |>
    str_replace("Bool$", "Yes (1)/No (0)") |>
    str_replace("Km", "Kilometres") 
  
  
  
  m <- draw_pac_map(the_data, 
                    family = "Calibri",
                    base_size = 14,
                    country_labels = FALSE,
                    fill_col = "obs_value",
                    fill_col_label = unit_title,
                    ylim = c(-40, 30)
  )  +
    scale_fill_viridis_c(label = comma) +
    labs(title = the_ind,
         subtitle = paste(sort(unique(the_data$obs_time)), collapse = ", ")) +
    theme(axis.text = element_blank())
  
  png(glue("pac-maps/{i + 1000}-{indicator_summary[i, ]$indicator}.png"), 1500, 900, res = 150, type = "cairo-png")
  print(m)
  dev.off()
  
}

# Convert all the single frames into a GIF.
# Requires ImageMagick to be installed. Can uncomment and run it here or do 
# it directly in a system / shell window
projdir <- setwd("pac-maps")
system('magick -loop 0 -delay 150 *.png "pac-maps-sdgs.gif"')
setwd(projdir)
