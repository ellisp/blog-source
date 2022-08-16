library(tidyverse)
library(sf)
library(Cairo)
library(janitor)
library(ggrepel)
library(maps)
library(extrafont)
library(rnaturalearth) # for downloading dateline
library(rgdal)         # support for rnatural earth
library(rsdmx)
library(ISOcodes)
library(scales)
library(RColorBrewer)
library(glue)

#-----------------population-------------

pops <- readSDMX(providerId = "PDH", 
                     resource = "data", 
                     flowRef = "DF_KEYFACTS")  |>
  as_tibble() |>
  clean_names()

#-----------------------land borders-------------------

# make two worlds together for drawing pacific-centred, adapted from
# https://stackoverflow.com/questions/34011100/plot-pacific-ocean-and-continents-with-ggplot2borders
mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
mp <- rbind(mp1, mp2) |>
  filter(long > 90  & long <360 & lat <50 & lat > -60) 

ggplot(mp) +
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_map()

#------------------international date line-----------------------------

if(!exists("glines")){
  glines <- ne_download(type = "geographic_lines", 
                        category = "physical", 
                        returnclass = "sf") |>
    st_shift_longitude() |>
    filter(name == "International Date Line")
}

#------------------------exclusive economic zones------------------------


fn1 <- "global_ffa_spc_sla_pol_june2022_kml.zip"
fn2 <- gsub("_kml\\.zip", ".kml", fn1)

if(!file.exists(fn1)){
  url <- "https://pacificdata.org/data/dataset/a89d83bc-378d-4679-b1fb-7096e76f2e30/resource/4dad0629-4cf3-498e-9f56-75f5c49c2763/download/global_ffa_spc_sla_pol_june2022_kml.zip"
  download.file(url, destfile = fn1, mode = "wb")
}

if(!file.exists(fn2)){
  unzip(fn1)
}

if(!exists("eez")){
  eez <- st_read(fn2)
}

sf_use_s2(FALSE)
pac <- eez |>
  slice(c(67, 245:282)) |>
  clean_names() |>
  filter(!grepl("Joint", name)) |>
  filter(!grepl("Triangle between", name)) |>
  filter(!grepl("Australia", name)) |>
  filter(!grepl("New Zealand", name)) |>
  filter(!grepl("Howland and Baker", name)) |>
  filter(!grepl("Palmyra", name)) |>
  filter(!grepl("Wake Island", name)) |>
  filter(!grepl("Matthew and Hunter", name)) |>
  filter(!grepl("Jarvis", name)) |>
  filter(!grepl("Hawaii", name)) |>
  st_shift_longitude() |>
  mutate(name2 = gsub(" Exclusive Economic Zon.*", "", name)) |>
  mutate(name2 = gsub("n$", "", name2)) |>
  mutate(name2 = ifelse(name2 %in% c("Niuea", "Fijia", "Naurua", "Kiribatia", "Tuvalua"),
                 str_sub(name2, end = -2),
                 name2)) |>
  mutate(name2 = case_when(
    name2 == "Micronesia" ~ "Micronesia, Federated States of",
    name2 == "Northern Mariana" ~ "Northern Mariana Islands",
    name2 == "Pitcairn Islands" ~ "Pitcairn",
    TRUE ~ name2
  )) |>
  mutate(id = 1:n()) |>
  # next 3 lines are for combining the countries that were split by 180 degrees into one
  # eg Tuvalu
  group_by(id, name2) |>
  dplyr::summarise(across(geometry, ~ sf::st_union(., by_feature = TRUE))) |>
  ungroup() 

stopifnot(
  pac |>
    anti_join(ISO_3166_1, by = c("name2" = "Name")) |>
    nrow() == 0)

pac_c <- st_centroid(pac)

if(!exists("pop_pdh")){
  pocket <- readSDMX(providerId = "PDH", 
          resource = "data", 
          flowRef = "DF_POCKET")  |>
    as_tibble() |>
    clean_names() 
  
  pop_pdh <- pocket |>
    filter(indicator %in% c("EEZ", "MIDYEARPOPEST", "POPDENS")) |>
    group_by(indicator, geo_pict) |>
    arrange(desc(obs_time)) |>
    slice(1) |>
    select(geo_pict, indicator, obs_value) |>
    spread(indicator, obs_value) |>
    # people per thousand km2 of EEZ (different to land)
    mutate(pop_dens_eez = MIDYEARPOPEST / EEZ * 1000) |>
    ungroup()
}



pac <- cbind(pac, st_coordinates(pac_c)) |>
  left_join(select(ISO_3166_1, Name, geo_pict = Alpha_2, iso3 = Alpha_3), by = c("name2" = "Name")) |>
  # note this is EEZ of whole country, so Kiribati has all in one number not split in 3,
  # and will have 3 reps of the same number
  left_join(pop_pdh, by = "geo_pict") 
  
quantiles <- quantile(pac$pop_dens_eez, prob = seq(0, 1, length = 8), type = 5)
quantiles[1] <- 0
quantiles[length(quantiles)] <- quantiles[length(quantiles)] + 1


#-----------how much of the world is there in the Pacific?-------------

# what proportion fo the world's surface (which is 510 million km2):
prop_surface <- percent(sum(pop_pdh$EEZ) / 510e6, accuracy = 0.1)

# proportion of world's population (which is 7.9 billion)
prop_pop <- percent(sum(pop_pdh$MIDYEARPOPEST) / 7.9e9, accuracy = 0.01)


total_land_area <- pop_pdh |>
  mutate(land_area = MIDYEARPOPEST / POPDENS) |>
  ungroup() |>
  summarise(land_area = sum(land_area)) |>
  pull(land_area)
# reality check - PNG is about 463k km2 so this (531k) looks right for all PICTs

# proportion of the world's total land area (which is 149 million km2)
prop_land <- percent(total_land_area / 149e6, accuracy = 0.01)

#-------------------------combined map------------------------
ff <- "Roboto"

sf_use_s2(FALSE) # so reticules still drawn on right half of map
m1 <- pac |>
  mutate(dens_cat = cut(pop_dens_eez, breaks = round(quantiles), dig.lab = 5),
         dens_cat = fct_reorder(gsub(",", "-", dens_cat), pop_dens_eez)) |> 
  ggplot() +
  geom_sf(aes(fill = dens_cat), colour = "grey70", alpha = 0.9) +
  geom_polygon(data = mp,
               aes(x = long, y = lat, group = group),
               fill = "white",
               alpha = 0.8) +
  geom_sf(data = glines, colour = "steelblue", linetype = 1, alpha = 0.5) +
  annotate("text", x = 182, y = 38, label = "International date line", 
           colour = "steelblue", hjust = 0, family = ff, size = 3) +
  geom_text(aes(label = name2, x = X, y = Y),
            colour = "black", family = ff, size = 3, angle = 15) +
  theme_minimal(base_family = ff) +
  scale_fill_manual(values = brewer.pal(9, "Oranges")) +
  theme(legend.position = c(0.8, 0.7),
        panel.background = element_rect(fill = "lightsteelblue", colour = NA),
        panel.grid = element_blank(),
        plot.caption = element_text(colour = "grey50")) +
  coord_sf(xlim = c(120, 290),  ylim = c(-50, 50)) +
  labs(title = "Exclusive economic zones (EEZs) of Pacific Community island countries and territories",
       subtitle = glue("Together, the EEZs make up around {prop_surface} of the world's surface, {prop_pop} of the world's population and the land is {prop_land} of the world's total land area."),
       x = "",
       y = "",
       fill = "People per 1,000\nsquare km of EEZ",
       caption = "Source: http://freerangestats.info with data from the Pacific Data Hub")

svg_png(m1, "../img/0241-map1", w = 12, h = 7)


#---------------------some more on population density--------------------


library(WDI)

if(!exists("densities")){
  densities <- WDI(indicator = "EN.POP.DNST") |>
    group_by(country) |>
    arrange(desc(year)) |>
    slice(1) |>
    ungroup()
}

densities |>
  mutate(spc = ifelse(iso2c %in% pocket$geo_pict, "Pacific", "Other"),
         country = fct_reorder(country, EN.POP.DNST)) |>
  ggplot(aes(y = EN.POP.DNST, x = country, fill = spc)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))
  

world_density <- densities |>
  filter(country == "World")
  # World density is 60 people per square km

pop_pdh |>
  select(geo_pict, pop_dens = POPDENS) |>
  rbind(select(world_density, geo_pict = iso2c, pop_dens = EN.POP.DNST)) |>
  left_join(select(ISO_3166_1, Name, geo_pict = Alpha_2), 
            by = "geo_pict") |>
  mutate(Name = ifelse(geo_pict == "1W", "World average", Name)) |>
  mutate(country = fct_reorder(Name, pop_dens)) |>
  ggplot(aes(x = pop_dens,  y = country, fill = (country == "World average"))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "", 
       x = "Population density (people per square kilometre of land")
  
