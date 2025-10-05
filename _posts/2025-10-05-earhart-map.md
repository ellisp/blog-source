---
layout: post
title: Mapping locations related to the Amelia Earhart disappearance
date: 2025-10-05
tag: 
   - Pacific
   - History
   - Spatial
description: I draw a map of the Pacific showing the key locations associated with the disappearance of Amelia Earhart and Fred Noonan in 1937.
image: /img/0307-map1.png
socialimage: https:/freerangestats.info/img/0307-map1.png
category: R
---

The 1937 disappearance of Amelia Earhart and Fred Noonan was briefly in the news in the last week or so. A campaign, apparently based in the Northern Marianas, to release any papers held by the US government found a willing ear in the current US administration. 

I don't think any of those papers have yet been released, and there is very little chance they have any new information if/when they are. But it did remind me I had long meant to look up the actual key locations involved.

The expectation/hope of [those involved in pushing for a release of papers](https://www.rnz.co.nz/international/pacific-news/574440/trump-to-declassify-amelia-earhart-records-potentially-closing-nearly-90-year-old-mystery) is that they will show Earhart and Noonan ended up in the [Japanese South Seas Mandate](https://en.wikipedia.org/wiki/South_Seas_Mandate), ultimately in Saipan (in the Northern Mariana Islands, now a US territory), and were executed as some local oral traditions claim. 

Why do I think there will be no new information? Consider what would be needed for the US to hold papers casting light on this:

1. Earhart and Noonan would have to gone far off course&mdash;either completely in the wrong direction to end up in Saipan directly which seems to require an implausible degree of incompetence, or still significantly (>1000km off course) to end up in the Marshall Islands.
2. The Japanese would have had to made secret captives of them, rather than either celebrating the rescue as a prestige-boost (remember Japan and the USA were in a tense, but far from war, relationship at this point) or parading them as spies.
3. The Japanese would have to have kept this so secret at the time that no material or archived evidence has ever been found of it, other than by US authorities.
4. The US would have to have found about this&mdash;either in the initial very extensive search in 1937, or after World War II while in occupation of Japan&mdash;and for some unfathomable reason kept it secret themselves.

This combination just seems vanishingly unlikely to me so I bravely make the prediction that the coming release of files from the US will reveal nothing substantive new about the matter. But nothing is certain! I will humbly admit I'm wrong if necessary.

There's decent Wikipedia articles on [Amelia Earhart](https://en.wikipedia.org/wiki/Amelia_Earhart), and on the [speculations about their disappearance](https://en.wikipedia.org/wiki/Speculation_on_the_disappearance_of_Amelia_Earhart_and_Fred_Noonan). I won't try to comprehensively summarise these as I have nothing really to add, but the latter article provides a good, simple summary:

> "Speculation on the disappearance of Amelia Earhart and Fred Noonan has continued since their disappearance in 1937. After the largest search and rescue attempt in history up to that time, the U.S. Navy concluded that Earhart and Noonan ditched at sea after their plane ran out of fuel; this "crash and sink theory" is the most widely accepted explanation. However, several alternative hypotheses have been considered."

Earhart and Noonan took off from [Lae](https://en.wikipedia.org/wiki/Lae_Airfield) in Papua New Guinea and headed to [Howland Island](https://en.wikipedia.org/wiki/Howland_Island) which is now an incorporated territory of the United States.

A key fact is that the last received radio message from Earhart and Noonan was that they were flying along a line of position running north-to-south on 157–337 degrees. This implies they had reached where they thought Howland Island should be and had turned at a sharp angle&mdash;either just east of south, or just west of north&mdash;to try to find it.

The non-insane key hypotheses for what happened once they got roughly in the vicinity of Howland Island are (in descending order of plausibility):

* They found no land, ran out of fuel and crashed into the ocean
* they travelled on a 157 degree bearing (south of south-east), found Gardner Island (now Nikumaroro, part of Kiribati), crashed there and eventually died of thirst or hunger
* they turned too early, travelled on a 337 degree bearing (north of north-west) and ended up in the Marshall Islands, perhaps Mili or Jaluit Atoll and were picked up by the Japanese.

So I drew this chart, with key features including:

* the key places named above
* the modern names of the areas that made up the Japanese-controlled South Seas Mandate
* The 157-337 degree line of position reported by Earhart and Noonan, if it were centred on Howland Island (noting of course that they probably weren't actually centered there, but a bit away from it, or they would have found it).

I've put it on a big enough scale to really get a sense of the vast distances and enormous bodies of water involved.

<object type="image/svg+xml" data='/img/0307-map1.svg' width='100%'><img src='/img/0307-map1.png' width='100%'></object>

What I like about this map is that we can see these locations in context all at once&mdash;nothing in the most accessible parts of the web dealing with this issue does this for me, although I'm sure it's been done somewhere. We can instantly see, for example, how implausible it is that Earhart and Noonan actually flew to Saipan themselves. They may not have been flawless navigators (or I wouldn't be writing this at all), but they were far from clueless beginners to set off at 90 degrees to their intended course.

I think there were a lot of things that might have gone wrong for me here. I had to manually type in a lot of latitudes and longitudes, I only did a few hours reading and thinking on the whole topic, I had to do various ad hoc things to work out what was meant by the line of position, draw a Pacific-centred map, etc. So as usual, any feedback is very welcome and if what I've done is bad enough I'll fix it.

That's it really. Pesonally, I think the generally accepted answer that they got a little off course, ran out of fuel and crashed in the ocean somewhere close to where they were heading (but not close enough, unfortunately) is very likely indeed.

Here's the code that draws the map:

{% highlight R lineanchors %}
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
# tan(angle) = opposite / adjacent. so if we set north/south arbitrarily to be 8 for drawing our line,
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
ggplot(mp, aes(x = long, y = lat)) +
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
{% endhighlight %}

That's all folks. Take your navigation seriously!