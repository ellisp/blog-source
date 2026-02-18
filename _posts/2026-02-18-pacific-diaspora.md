---
layout: post
title: Pacific diaspora
date: 2026-02-18
tag: 
   - Pacific
   - Demography
   - WorkRelated
description: Some Pacific countries have dramatically more of "their" people living overseas than in the origin country. And some don't. 
image: /img/0313-diaspora-bar.svg
socialimage: https:/freerangestats.info/img/0313-diaspora-bar.png
category: R
---

This post is the fourth of a series of (probably) seven on population issues in the Pacific, re-generating the charts I used in a keynote speech before the November 2025 meeting of the Pacific Heads of Planning and Statistics in Wellington, New Zealand. The seven pieces of the puzzle are:
* [Visual summaries of population size and growth](/blog/2025/11/30/pacific-population)
* [Net migration](/blog/2025/12/04/pacific-net-migration)
* [World cities with the most Pacific Islanders](/blog/2026/02/16/pacific-cities) 
* Pacific diaspora (this post, today)
* Population pyramids (to come)
* Remittances (to come)
* Tying it all together (to come)

Today's post is all about creating this one eye-catching chart, comparing the number of people in a country with its diaspora&mdash;people who ethnically or otherwise identify with the country but are living overseas:

<object type="image/svg+xml" data='/img/0313-diaspora-bar.svg' width='100%'><img src='/img/0313-diaspora-bar.png' width='100%'></object>

As the chart notes, the diaspora numbers are an underestimate because I've only drawn on the New Zealand, Australian and USA censuses, and only partly for that. For example, I decided the number of Papua New Guineans living in New Zealand and USA wasn't material and they haven't been included. I'm confident this doesn't change the look of the chart, but obviously if I were trying to create the best possible comprehensive estimates I should include these.

It's a pretty dramatic story. We can see seven countries with more people living overseas than in the country itself: Niue, Pitcairn Islands, Cook Islands, Tokelau, Samoa, Tonga and Marshall Islands. Apart from Marshall Islands, these are all Polynesian. In fact, Tuvalu is the only Polynesian country in this collection that has more people living in-country than overseas (for now&mdash;this is likely to change now that Australia has agreed with Tuvalu for a regular annual intake of people via lottery).

Note that the three French territories (New Caledonia, Wallis and Futuna, and French Polynesia), and three American territories (American Samoa, Northern Mariana Islands and Guam) have been excluded from the plot.

For the four small countries along the bottom row of the chart, the difference is particularly important&mdash;a big majority of their people are living overseas. From my last post we know that a lot of these are in Auckland. Pitcairn is the only one of these four that has more of its diaspora in Australia than New Zealand (there are Pitcairn-identifying people in the UK too, but not enough to make me systematically add in the UK to my data in what was essentially a pragmatic and visual exercise&mdash;see comments above).

> 96% of Niueans, 90% of Cook Islanders and 59% of Marshall Islanders live overseas.

And for the four countries at the top of the chart&mdash;somewhat larger and distinctly poorer than many of the others, and three of them Melanesian&mdash;we see no significant diaspora, relative to the home population.

Here's the code that creates this bar chart. Note that the data here are typed in by hand (!!) from various sources&mdash;not something I'd normally do, and would never recommend except for these really "small data" situations. I've checked it as thoroughly as I reasonably can, and the version I used in my talk that I'm adapting here was also peer reviewed by a work colleague.

{% highlight R lineanchors %}
# This script draws some charts of the diaspora of Pacific island countries and territories.
# It's pretty rough and certainly incomplete. The approach was to use the census figures
# for resident population of Pacific islander ancestry currently living in USA, Australia
# and New Zealand; and compare that to populations resideing in the countries themselves.
#
# All sorts of known limitations which we are prepared to live with for these crude comparisons:
# - different reference years (2025 for populations, and census years are 2018, 2020 and 2021)
# - populations residing in the Pacific islands themselves are all ethnicities (e.g. will include
#   Australian-descent people rsideing in those countries), haven't bothered to limit to just "true" Tongans, Samoans, etc
# - not comprehensive e.g. I know there are some Pitcairn-descended people in UK but haven't included them. And of course
#   there must be many others of these people in countries other than Australia, NZ and USA
# - France not included at all. No ancestry data in French censuses so this would be tricky.
#
# Peter Ellis 2025-11

#---------------------Data prep-------------------------

library(tidyverse)
library(rsdmx)
library(ISOcodes)

# Current populations of PICTs:
pops <- rsdmx::readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A..MIDYEARPOPEST._T._T?startPeriod=2025&endPeriod=2025&dimensionAtObservation=AllDimensions") |> 
  as_tibble() |> 
  left_join(select(ISOcodes::ISO_3166_1, Alpha_2, pict = Name), by = c("GEO_PICT" = "Alpha_2")) |> 
  select(pict, pop = obsValue) |> 
  drop_na()

# out of interest what is the total population of all PICTs, Austrlaia and NZ together? (about 47m):
picts_and_anz <- c(sum(pops$pop), 28.1e6, 5.3e6)
sum(picts_and_anz)

# https://tools.summaries.stats.govt.nz/ethnic-group/tongan and similar for 2023 NZ figure
# table builder for Australian 2021 figures - see `https://raw.githubusercontent.com/ellisp/blog-source/refs/heads/master/data/total%20by%20pacific.csv`
# Wikipedia for US figures, from 2020 census. Search for e.g. "Palauans in USA wikipedia"
diaspora <- tribble(~pict, ~dest, ~people,
                    "Tonga",            "New Zealand", 97824,
                    "Niue",             "New Zealand", 34944,
                    "Tokelau",          "New Zealand", 9822,
                    "Cook Islands",     "New Zealand", 94176,
                    "Samoa",            "New Zealand", 213069,
                    "Tuvalu",           "New Zealand", 6585,
                    "Fiji",             "New Zealand", 25038 + 23808, # includes Fijian Indian
                    "Papua New Guinea", "Australia", 22668,
                    "Vanuatu",          "Australia", 2380,
                    "Solomon Islands",  "Australia", 2704,
                    "Kiribati",         "Australia", 1263,
                    "Fiji",             "Australia", 48354,
                    "Nauru",            "Australia", 571,
                    "Cook Islands",     "Australia", 27494,
                    "Tokelau",          "Australia", 2544,                    
                    "Tonga",            "Australia", 43469,
                    "Niue",             "Australia", 6225,
                    "Samoa",            "Australia", 98022,
                    "Tuvalu",           "Australia", 995,
                    "Pitcairn",         "Australia", 1123,
                    "Marshall Islands", "USA", 52624, # 47300 if just 'alone' 
                    "Palau",            "USA", 12202,
                    "Micronesia, Federated States of", "USA", 21596)

# Australia checked
# New Zealand checked
# USA checked


#--------------------------Bar chart------------------------
# data frame to inspect to get percentages.
pops_with_prop <- pops |> 
  inner_join(diaspora) |> 
  mutate(pict = gsub("Federated States of", "Fed. St.", pict)) |> 
  group_by(pict, pop) |> 
  summarise(Overseas = sum(people)) |>
  ungroup() |> 
  mutate(prop = Overseas / (pop + Overseas)) |> 
  mutate(pict = fct_reorder(pict, prop))

pops_with_prop |> 
  select(-prop) |> 
  rename(`Origin country` = pop) |> 
  gather(variable, value, -pict) |> 
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_col(width = 0.8) +
  facet_wrap(~pict, scales = "free_y") +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c("steelblue", "brown")) +
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.caption = element_text(colour = "grey50")) +
  labs(x = "", y ="Number of people",
        title = "Pacific Islander diaspora, arranged from lowest proportion overseas to highest",
        subtitle = "Diaspora is a lower bound of full figure as it is based on just Australia, USA and New Zealand censuses.",
       caption = "Source: PDH.Stat for populations; Australian, USA and New Zealand Censuses for diaspora.")
{% endhighlight %}

## An alternative (not so good) visualisation

I used the same data to also make this scatter plot:

<object type="image/svg+xml" data='/img/0313-diaspora-scatter.svg' width='100%'><img src='/img/0313-diaspora-scatter.png' width='100%'></object>

But I don't much like it. It's difficult to interpret, and while it has a bit of extra information (which country the diaspora is in) this doesn't outweigh the interpretation problems. it probably shouldn't have a log scale as we really want to add up the numbers; but using a non-transformed scale makes it even more of a visual mess. I'm including it here really just for the record and to illustrate that the first try at visualising something isn't always the best (and sometimes, a humble bar chart ends up being what you want). Here's the code for the scatter plot:

{% highlight R lineanchors %}
pops |> 
  inner_join(diaspora) |> 
  ggplot(aes(x = pop, y = people, label = pict, colour = dest)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey") +
  geom_point() +
  geom_text_repel(size = 2.5) +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  scale_colour_manual(values = c("blue", "black", "darkred")) +
  labs(x = "People living in origin country in 2025",
       y = "Diaspora in another country, recent census",
       colour = "Disapora country",
      title = "Pacific Island home population and diaspora in various countries")
{% endhighlight %}
