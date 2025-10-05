library(tidyverse)
library(ggrepel)

# Lae airfield, PNG https://en.wikipedia.org/wiki/Lae_Airfield
# https://en.wikipedia.org/wiki/Howland_Island
# other lat and long similarly taken from the other wikipedia articles

# longitudes that are west of 180 degrees count at first as negative for this
# way of centering a map on Pacific:
earhart <- tribble(~lat,                     ~long,                             ~name,           ~type,
                 -(6 + 43/60 + 59/3600),     (146 + 59/60 + 45/3600),     "Lae Airfield",     "Origin",
                   0 + 48/60 + 25.84/3600,  -(176 + 36/60 + 59.48/3600),  "Howland Island",   "Planned",
                 -(4 + 40/60 + 32/ 3600),   -(174 + 31/60 + 4/3600),      "Nikumaroro",       "Unlikely",
                  15 + 11/60,                (145 + 45/60),                "Saipan",          "Unlikely",
                   5 + 55/60 + 18/3600,      (169 + 38/60 + 33/3600),     "Jaluit Atoll",    "Unlikely",
                   6 + 8/60,                 (171 + 55/60),                "Mili Atoll",       "Unlikely"
                 ) |> 
  # fix those negative longitudes to work on a 0:360 scale:
          mutate(long = ifelse(long < 0 , long + 360, long))

# the 157/337 line of position, centred on Howland Island
# 23 degrees to the west of north, 23 degrees to the east of south
# tan(angle) = opposite / adjacent. so if we set north/south arbitrrily to be 5,
adjacent <- 8
opposite <- tan(-23 * pi / 180) * adjacent
lop <- tibble(lat = earhart[2, ]$lat + c(-1, 1) * adjacent,
              long = earhart[2, ]$long + c(-1, 1) * opposite)

# build a background map out of two maps joined together.
mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
mp <- rbind(mp1, mp2) |>
  filter(long > 90  & long <360 & lat <50 & lat > -60) |> 
  mutate(japan = ifelse(region %in% c("Japan", "Marshall Islands", "Palau", "Northern Mariana Islands",
                                       "Micronesia", "North Korea", "South Korea"),
                        "Japanese-controlled", "Not Japanese-controlled"))

# some points for labels ofjapanese-controlled labels
jap <- mp |> 
  filter(japan == "Japanese-controlled") |> 
  group_by(region) |> 
  summarise(long = median(long), lat = median(lat)) |> 
  # tweaks for label positions
  mutate(lat = case_when(
    region == "Northern Mariana Islands" ~ lat + 2.0,
    region == "Marshall Islands"         ~ lat + 3.3,
    TRUE                                 ~ lat
  ))

# the possible, unlikely colour and linetype
plt <- 2
pc <- "lightsalmon"

# the japanese controlled colour
jc <- "red"

# the colour for the planned line of travel:
plcol <- "darkblue"

# draw the actual plot
p <- ggplot(mp, aes(x = long, y = lat)) +
  # add background map
  geom_polygon(aes(group = group), fill = "grey60") +
  coord_map(xlim = c(100, 206), ylim = c(-22, 35)) +
  # add labels of Japanese-controlled areas:
  geom_label(data = jap, aes(label = region), colour = jc) +
  # three lines from Lae outwards"
  geom_segment(xend = earhart[1, ]$long, yend = earhart[1, ]$lat, data = earhart[-1, ], 
               aes(colour = type, linetype = type), linewidth = 2) +
  # two lines from Howland Island:
  geom_segment(xend = earhart[2, ]$long, yend = earhart[2, ]$lat, data = earhart[-(1:2), ], 
               colour = pc, linetype = plt, linewidth = 2) +
  # line of position reported by Earhart and Noonan
  geom_line(data = lop) +
  # points and labels of various locations
  geom_point(data = earhart, size = 4, colour = "white", shape = 19) +
  geom_label_repel(data = earhart[1:3, ], aes(label = name), colour = "grey20", alpha = 0.9, seed = 123) +
  geom_label_repel(data = earhart[4:6, ], aes(label = name), colour = jc, alpha = 0.9, seed = 123) +
  # annotations:
  annotate("text", x = 182, y = 6.2, hjust = 0, size = 3,
           label = str_wrap("Line of position reported by Earhart and Noonan while seeking Howland Island.", 40)) +
  annotate("text", x = 189, y = -7, hjust = 0, size = 3,
           label = str_wrap("Nikumaroro, or Gardner Island, has been searched repeatedly and no firm evidence found.", 36)) +
  annotate("text", x = 140, y = 21.5, hjust = 0, size = 3,
           label = str_wrap("Witnesses claimed to see the execution of Earhart and Noonan by Japanese soldiers in Saipan but no records, other confirming evidence or motivation have been found.", 58)) +
  annotate("text", x = 152, y = 10, label = "Japan's South Seas Mandate", colour = jc) +
  # scales, colours, themes, etc:
  scale_linetype_manual(values = c(1, plt)) +
  scale_colour_manual(values = c(plcol, pc)) +
  scale_x_continuous(breaks = c(120, 150, 180, 210), 
                     labels = c("120E", "150E", "180", "150W")) +
  labs(x = "", y = "", linetype = "Routes", colour = "Routes",
       title = "Key locations relating to disappearance of Amelia Earhart and Fred Noonan in 1937",
       subtitle = "Most likely explanation was running out of fuel and crash in ocean near Howland Island") +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.position = c(0.963, 0.064))

svg_png(p, "../img/0307-map1", w = 12, h = 7.5)



