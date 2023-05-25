library(tidyverse)
library(rnaturalearth)
library(sf)
library(WDI)
library(extrafont)
library(RColorBrewer)

#-----------------data prep--------------------

# Data on proportion of women in Parliament from World Development Indicators:
wom_raw <- WDI(indicator = "SG.GEN.PARL.ZS", start = 2010)

# Get the latest year for each country:
wom <- wom_raw |>
  as_tibble() |>
  rename(wom_parl = SG.GEN.PARL.ZS) |>
  filter(!is.na(wom_parl)) |>
  group_by(iso3c) |>
  arrange(desc(year)) |>
  slice(1) |>
  ungroup() 

# maps for Asia and Pacific from Natural earth
sf_use_s2(FALSE)
m1 <- ne_countries(scale = "medium", returnclass = "sf", continent = "asia") 
m2 <- ne_countries(scale = "medium", returnclass = "sf", continent = "oceania") 
m <- rbind(m1, m2)

  # extract the centres:
centers <- st_centroid(m, of_largest_polygon = TRUE) |>
  # turn them into straight numbers, not sf geometries:
  st_coordinates() |> 
  # add those coordinates as columns to the original map:
  cbind(m) |>
  # join to the women in parliament data:
  left_join(wom, by = c("iso_a3" = "iso3c")) |>
  rename(x = X, y = Y) |>
  mutate(x = case_when(
    # put Tonga and Samoa back on the map (the land won't show but the bars will)
    x < 0 ~ x + 360,
    # Put Kiribati where Tarawa is rather than its spatial centre:
    country == "Kiribati" ~ 173,
    TRUE  ~ x
  )) |>
  # a couple of manual adjustments to make things more readable, avoiding overlaps
  mutate(x = case_when(
    geounit == "North Korea" ~ x - 1.5,
    geounit == "Vietnam" ~ x + 1,
    geounit == "Laos" ~ x - 0.8,
    geounit == "Indonesia" ~ x - 1.8,
    geounit == "Malaysia" ~ 101.7, # location of Kuala Lumpur
    TRUE ~ x
  )) |>
  # create scaled latitude coordinates for our rectangles and segments:
  mutate(sc = 5,
         yend1 = y + wom_parl / sc,
         yend0 = y + 50 / sc) |>
  filter(!is.na(wom_parl)) |>
  # knock out the middle east because it's cluttered :):
  filter(x > 60)

#------------------map--------------------------

xw <- 1

the_font <- "Calibri"
wom_map <- ggplot(centers) +
  geom_sf(colour = "grey50") +
  geom_rect(aes(xmin = x - xw, xmax = x + xw, ymin = y, ymax = yend0),
            fill = "steelblue", alpha = 0.5) +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend1, colour = wom_parl),
               linewidth = 2) +
  geom_text(aes(label = round(wom_parl), x = x, y = yend1 + 1, colour = wom_parl), 
            vjust = 0, size = 3, family = the_font) +
  # It was useful while troubleshooting to add country names;
  # uncomment the below if you want them:
  # geom_text(aes(label = country, x = x, y = y)) +
  theme_void(base_family = the_font) +
  xlim(60, 200) +
  scale_colour_viridis_c(option = "B", limits = c(0, 60), 
                         label = number_format(suffix = "%")) +
  theme(legend.position = c(0.2, 0.35)) +
  labs( colour = "",
        title = "Percentage of parliamentarians who are women",
        subtitle = "Blue bars represent 50%")
svg_png(wom_map, "../img/0245-wom-map", w = 8, h = 7)



#----------------alternative visualisation-------------
names(centers)

p <- centers |>
  mutate(clps_income = case_when(
    # collapse OECD and non-OECD high income and upper middle together,
    # as they are quite small categories
    grepl("High income", income_grp) | grepl("Upper middle", income_grp) ~ "High or middle income",
    TRUE ~ gsub("[0-9]\\. ", "", income_grp)
  )) |>
  mutate(clps_income = fct_relevel(clps_income, "High or middle income", after = Inf)) |>
  mutate(country = fct_reorder(country, -wom_parl),
         subregion = fct_reorder(subregion, wom_parl)) |>
  ggplot(aes(x = wom_parl, y = country)) +
  geom_segment(aes(xend = 0, yend = country, colour = subregion), linewidth = 1.5) +
  geom_point() +
  geom_text(aes(label = round(wom_parl)), family = the_font, 
            size = 2.5, vjust = 0.5, hjust = 0, nudge_x = 1) +
  facet_wrap(~clps_income, scales = "free_y", ncol = 2) +
  # expand = c(0,0) is needed to put the country labels close to the plot
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 60),
                     label = number_format(suffix = "%")) +
  # exclude the bright yellow from Brewer Set1 colours, which I don't like:
  scale_colour_manual(values = brewer.pal(9, "Set1")[-6]) +
  theme_minimal(base_family = the_font) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y=element_text(margin=margin(r=0)),
        legend.position = c(0.8, 0.2)) +
  labs(y = "",
       x = "Proportion of Parliamentarians that are women",
       colour = "Sub-region",
       title = "Women parliamentarians in the Asia-Pacific")

svg_png(p, "../img/0245-wom-dots", w = 8, h = 7)

save.image("image20230525.RData")
