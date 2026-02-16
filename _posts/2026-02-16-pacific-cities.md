---
layout: post
title: The world's biggest 'Pacific' cities
date: 2026-02-16
tag: 
   - Pacific
   - Demography
   - WorkRelated
description: My best hasty effort at presenting the cities around the world with the most Pacific Islanders resident. 
image: /img/0312-pacific-cities.svg
socialimage: https:/freerangestats.info/img/0312-pacific-cities.png
category: R
---

This post is the third in a (somewhat interrupted) series on population issues in the Pacific, re-generating the charts I used in a keynote speech before the November 2025 meeting of the Pacific Heads of Planning and Statistics in Wellington, New Zealand. So far we have:
* [Visual summaries of population size and growth](/blog/2025/11/30/pacific-population)
* [Net migration](/blog/2025/12/04/pacific-net-migration)
* World cities with the most Pacific Islanders (this post, today)

We often hear that Auckland is the world's largest Polynesian city, or even the world's largest Pacific Islander city; but which is the second or third largest? 

This will be a short post. The end point is this single chart:

<object type="image/svg+xml" data='/img/0312-pacific-cities.svg' width='100%'><img src='/img/0312-pacific-cities.png' width='100%'></object>

Port Moresby (the capital of Papua New Guinea) is the second biggest urban collection of Pacific Islanders, and in fact it isn't far behind Auckland. Next come the largest cities of Indonesian Western New Guinea. I'm not well familiar with that part of the world and I might have missed some further cities of similar size, but am confident I got the first two. Coming in at numbers 5 and 6 we have Suva in Fiji, and Papeete in French Polynesia (on the island of Tahiti). Then we see that Sydney and Brisbane in Australia probably have more Pacific Islanders than do many of the famous cities of the Pacific, such as Lae, Honiara, Noumea and Port Vila. Samoa's Apia doesn't even make it on to the chart.

I couldn't get data on French cities in the mainland 'hexagon', for which ethnicity information is difficult to obtain for deliberate decisions on the part of the statistical authorities. There are good reasons for this founded in history. But the number is probably too small to make it to the chart. Los Angeles could maybe be there if a broad enough geography is included, but the city definitions were a bit tough for me to deal with and in the end I opted to leave it out.

I'm sure there's some omissions or errors here so would welcome corrections and comments, as usual. But the main point was illustrative, and aimed at pointing out the importance of some cities perhaps not often thought of as Pacific Islander urban concentrations, and I'm happy that it does that reasonably accurately.

There were a few choices here, such as whether to include West Papuans, Māori (who are pretty numerous in Australian cities as well as in New Zealand) and Hawaiians (and less materially as there are less of them, Torres Strait Islanders) as Pacific Islanders. I'm pretty happy that the answer is "yes" to include all of them, for our purposes. Note that if we excluded Māori from the Auckland count, it would no longer be the world's biggest Pacific Islander city.

The real difficulty, and one I'm confident my solution for which could be improved, was in getting consistent definitions of "city" and good estimates of what proportion of that city are Pacific Islanders. The latter can come from census data, but I didn't have time to go to each country's latest census and ensure a comparable number, so had to resort to Wikipedia in some cases.

For example of the problem of a definition of 'city', Honolulu itself has a population of around 350,000, but the [Urban Honolulu metropolitan area](https://en.wikipedia.org/wiki/Honolulu) is around 1 million (only a small proportion of whom are Pacific Islanders). Suva's population is around 100,000; its metropolitan area brings this up to 185,000; and if you include Lami, Nasinu and Nausori (where the airport is) [this becomes 330,000](https://en.wikipedia.org/wiki/Suva). In both these cases I used the greater metropolitan area, but not Nausori, etc. for Suva. 

For Australia and New Zealand I used the "Greater Capital City Statistical Areas" and "Territorial Authorities" respectively. This means I miss out on non-capital cities, like Gold Coast (population around 600,000 and around 1 per cent Pacific Islander) but I think that is ok. It means we are under-counting Wellington by the standard I used for Suva and Honolulu (Lower Hutt and Upper Hutt should probably be included, but they are their own TAs). Again, I think that is probably ok.

There's at least one other more controversial problem I've skimmed over and won't mention.

For cities outside Australia and New Zealand I didn't have time to get definitive estimates directly from each census and relied on Wikipedia and other secondary sources. This bit is highly error-prone, and could do with a more careful approach! Overall I've got a somewhat dim view  of the tossed-together code below, which was a real compromise between time and thoroughness. But hopefully the results are good enough for our illustrative purposes! Anywhere, here's the code:

{% highlight R lineanchors %}
# this is a crude exploration of the question:
# "What are the largest Pacific islander cities in the world?"
# It is doubtless incomplete and there are a bunch of more detailed
# issues to go into if we wanted to do this definitively.
#
# Peter Ellis 2025-11

library(tidyverse)
library(scales)

#--------------------New Zealand census data----------
# Large file of Stats NZ census data to download. apparently the Census 2023
# equivalent is not yet availalbe, so we just use the 2018 version:
dir.create("raw-data")
fn <- "raw-data/nz_census_2018.zip"
if(!file.exists(fn)){
  download.file("https://www3.stats.govt.nz/2018census/8317_Age%20and%20sex%20by%20ethnic%20group%20(grouped%20total%20responses),%20for%20census%20night%20population%20counts,%202006,%202013,%20and%202018%20Censuses%20(RC,%20TA,%20SA2,%20DHB).zip",
              destfile = fn, mode = "wb")
} 

# the file is a zipped collection of long thin coded data table and 
# dimension lookup tables explaining what each of the codes mean:
unzip(fn, exdir = "raw-data")

ethnic <- read_csv("raw-data/DimenLookupEthnic8317.csv")
area <- read_csv("raw-data/DimenLookupArea8317.csv")

# we are going to use the Territorial Authority level so we can pick up
# Christchurch, Wellington which are TAs. Note this means we are 
# not counting eg Lower Hutt as part of Wellington. An interpretation of 'greater Wellington'
# probably would include this. But this is an ok compromise for our purposes, I think?

# Takes a while because there is a mass of very detailed data here
# but we are only using a tiny bit of it - second biggest regional groups
# and just a small subset of the ethnic groups
nz2018 <- read_csv("raw-data/Data8317.csv") |> 
  filter(Year == 2018) |> 
  left_join(ethnic, by = c("Ethnic" = "Code")) |>
  rename(ethnic_name = Description) |> 
  left_join(area, by = c("Area" = "Code")) |> 
  rename(area_name = Description) |> 
  filter(ethnic_name %in% c("Maori", "Pacific Peoples")) |> 
  # only Territorial Authority level:
  filter(str_length(Area) %in% 3) |> 
  filter(!area_name %in% c("Total - Territorial Authority areas")) |> 
  # total all people:
  filter(Age == "999999") |> 
  # total all sexes:
  filter(Sex == 9) |> 
  # just cities (not districts) |> 
  filter(grepl("City", area_name)  | area_name == "Auckland") |> 
  mutate(value = as.numeric(count)) |> 
  select(ethnic_name, area_name, value) |> 
  mutate(country = "New Zealand")

# quick reality check - print to console the biggest TAs with Pacific peoples:
nz2018 |> 
  group_by(area_name) |> 
  summarise(value = sum(value)) |> 
  arrange(desc(value))

nz2018 |> 
  select(ethnic_name, value, area_name) |> 
  spread(ethnic_name, value) |> 
  arrange(desc(`Pacific Peoples`))

#--------------Australian census data--------------
# Originally downloaded from australian tablebuilder,
# file is small so is committed to this repo:
# `/raw-data/ancestry pacific by greater city 2021 australia census.csv`


aus2021 <- read_csv("https://raw.githubusercontent.com/ellisp/blog-source/refs/heads/master/data/ancestry%20pacific%20by%20greater%20city%202021%20australia%20census.csv",
                    skip = 9, n_max = 26) |> 
  select(-Total, -...11) |> 
  rename(ethnic_name = `GCCSA (UR)`) |> 
  filter(!is.na(`Greater Sydney`)) |> 
  gather(area_name, value, -ethnic_name) |> 
  filter(!grepl("Total", ethnic_name)) |> 
  mutate(value = as.numeric(value)) |> 
  mutate(ethnic_name = if_else(
    ethnic_name == "Maori", "Maori", "Pacific Peoples"
  )) |> 
  group_by(ethnic_name, area_name) |> 
  summarise(value = sum(value)) |> 
  mutate(country = "Australia")

#--------------Other--------------
# these estimates from various ad hoc sources, mostly
# Wikipedia. Remembering we want number of pacific islanders,
# not total ppulation. Which means we have two difficult numbers
# to get hold of. So this bit is certainly wrong! - just the
# best estimate I could do in a hurry.
other <- tribble(~area_name, ~value, ~country,
                 "Port Moresby", 400000, "PNG",
                 "Lae",           100000, "PNG",
                 "Mount Hagen", 50000, "PNG",
                 # pop is 400k+ but what proportion is pacific? - generally west papua about 75% papuans:
                 "Jayapura", 320000, "Indonesia",
                 "Sorong", .75 * 300000, "Indonesia",
                 "Greater Suva", 185000, "Fiji", # not counting nausori
                 "Lautoka", 75000, "Fiji",
                 "Nasinu", 74000, "Fiji",
                 # Only about 9% of greater honolulu identify as pacific islander:
                 "Honolulu urban area", 0.09 * 1e6, "USA",
                 "Greater Noumea", 0.26 * 200000, "New Caledonia",
                 "Papeete", 137000, "French Polynesia",
                 "Honiara", 80000, "Solomon Islands",
                 "South Tarawa", 70000, "Kiribati",
                 "Majuro", 20000, "Marshall Islands",
                 "Apia", 30000, "Samoa",
                 "Port Vila", 50000, "Vanuatu"
        ) |> 
  mutate(ethnic_name = "Pacific Peoples")

#----------------draw bar chart--------------
nz2018 |> 
  rbind(aus2021) |> 
  rbind(other) |> 
  group_by(area_name) |> 
  mutate(total = sum(value)) |> 
  ungroup() |> 
  arrange(desc(total)) |> 
  slice(1:24) |> 
  mutate(area_name = fct_reorder(area_name, -value, .fun = sum)) |> 
  mutate(country_type = case_when(
    country %in% c("Australia", "New Zealand", "France", "USA") ~ "Metropolitan SPC member",
    country %in% c("Indonesia")  ~ "Non-SPC member" ,
    TRUE ~ "Pacific island SPC member")) |> 
  ggplot(aes(y = value, x = area_name, fill = country_type)) +
  geom_col(position = "stack") +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c("darkgreen", "brown", "steelblue")) +
  labs(fill = "", x = "", y = "Number of Pacific Islanders
(including Māori, Papuans and Hawaiians)",
       title = "The world's largest Pacific Islander cities",
      subtitle = "Treat these estimates with some caution... corrections are welcomed!",
       caption = "Source: Australia Census 2021, New Zealand Census 2018, Wikipedia and author estimates ") +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.7),
        plot.caption = element_text(colour = "grey50"))
{% endhighlight %}

That's all for now. Coming up we have a look at how much of Pacific Islander populations are in the "home" country and how much elsewhere (e.g. New Zealand); some more on population profiles; remittances data; and a summary post where I'll tie things together with the messaging I used in the actual talk. 
