---
layout: post
title: Men's domestic chores and fertility rates - Part II, technical notes
date: 2025-08-16
tag: 
   - ModellingStrategy
   - DataFromTheWeb
description: A few specific notes on technical issues relating to a previous post; on drawing graphs with different coloured edges, modelling strategy, different specifications of models, and accessing UN SDG and gender inequality data.
image: /img/0290-compare-ti-and-s.png
socialimage: https:/freerangestats.info/img/0290-compare-ti-and-s.png
category: R
---

This post is a collection of more technical notes forming a companion piece to [the previous post on men's time spent on domestic chores and total fertility rates at the country level](/blog/2025/08/15/timeuse-summary). 

The market audience for today's post is future-me, and anyone else similarly interested in the quite specific issues it is jotting down notes for. Those issues are:

* Drawing directed graphs with `ggdag` that have different coloured edges
* Accessing the UN Sustainable Development Goals (SDGs) database API
* Data options on gender inequality
* Fitting the same mixed effects model with `lme4::lmer`, `mgcv::gamm` and `mgcv::gam`, comparing results and extracting group-level residuals
* Hand-drawing prediction plots to show the results of models with interactions and comparing that to the `marginaleffects` package
* Modelling strategy questions relating to use of splines and interaction effects

My approach to presenting this (both today, and in Part I) is different to my usual one, where I try to make the post itself a stand-alone, reproducible artefact with all the code necessary to produce the results in the post. That approach just didn't work out in this case; the code was too long and boring to include in the main blog, there are too many different audiences to write for, and I went down too many dead ends in exploring some of the modelling options. Even in this technical companion piece, this post isn't going to be fully reproducible, but just have the snippets of code that best illustrate the point.


To make up for this, as ever, the full code to produce that blog post is on [GitHub as part of my overall blog-source repository](https://github.com/ellisp/blog-source/blob/master/_working/0290-house-work.R).

### Directed graphs with different coloured edges

For the main post, I had to draw a couple of directed graphs with different coloured edges connecting the nodes. Specifically, I wanted to use a red line to show a negative direction effect, and blue to show a positive, like this:

<object type="image/svg+xml" data='/img/0290-dg-simplified.svg' width='100%'><img src='/img/0290-dg-simplified.png' width='100%'></object>

This was surprisingly fiddly to do and required a bit of a hack. Specifically, you have to explicitly call the `tidy_dagitty` function, which turns a `ggdag` graph object of class `dagitty` into a data frame of class `tidy_dagitty`; then you add a column to that data frame which has the *actual colours* as its values, conditional on whatever algorithm you need to determine those colours. In this example, I want it to be red when the line segment is connect "opp" (Opportunities for women and girls) to "tfr" (Total fertility rate), and blue otherwise. 

As far as I could tell, you can't just map a column of character or factor values to colour and let the colour scale match it, which would be the approach more consistent with the `ggplot2` philosophy. Instead, you only have the choice of an identity scale, which is why that column `edge_type` I  add has to have the values "darkred" and "steelblue". That's the main trick for doing this.

{% highlight R lineanchors %}
dg2 <- dagify(tfr ~ opp + hw ,
             hw ~ opp,

             labels = c(
               "tfr" = "Total fertility rate",
               "hw" = "Men doing housework",
               "opp" = "Opportunities for\nwomen and girls"
             ),
             outcome = "tfr",
             exposure = "hw"
)  |> 
  # explicitly call this usually hidden function so we can colour the edges:
  ggdag::tidy_dagitty(seed = 124) |> 
  # colour the edges. Need to specify identity of colour here, not use scale_
  mutate(edge_type = ifelse(to == "tfr" & name == "opp", "darkred", "steelblue"))


# Draw the simplified causal graph
set.seed(124)
dg2 |> 
  ggplot(aes(x = x, y = y, xend = xend, yend =yend)) +
  geom_dag_node(colour = "grey") +
  geom_dag_edges(aes(edge_colour = edge_type), 
                 arrow_directed = grid::arrow(length = unit(12, "pt"), type = "closed")) +
  geom_dag_text_repel(aes(label = label), col = lab_col) +
  theme_dag(base_family = "Roboto")
{% endhighlight %}



### Accessing the UN SDGs database

I couldn't find a simple way of accessing the United Nations Statistics Division's invaluable definitive database of the SDG indicators for all countries of the world. By which I mean, it has an API, but I didn't see anyone who'd written a nice R package to conveniently interact with it. If anyone knows of someone who's done this, or wants to do it themselves, please let me know.

So I had to write my own API request by myself, like an animal. I did this in what I am sure is a suboptimal way, but it works. From playing around with [the UN's API](https://unstats.un.org/sdgs/UNSDGAPIV5/swagger/index.html) I found the `curl` command I wanted to download the data:

```
curl -X POST --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/octet-stream' -d 'seriesCodes=SL_DOM_TSPD' 'https://unstats.un.org/sdgapi/v1/sdg/Series/DataCSV'
```

Then I used functions from Bob Rudis' `curlconverter` R package to convert this to a request for the old-fashioned `httr` package to use. As the comments in this code say, I know all this is outmoded; but it works for now.

{% highlight R lineanchors %}
#-----------downloading some SDG time use data from the UN database-------------
# Note sure this is the best way to do this, it was clunky to work out,
# but it works. Someone should (or have they already?) build an R package.
#
# this is all httr, I understand httr2 is the  current thing now, but this still works 
library(curlconverter)
library(httr)
request <- "curl -X POST --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/octet-stream' -d 'seriesCodes=SL_DOM_TSPD' 'https://unstats.un.org/sdgapi/v1/sdg/Series/DataCSV'" |> 
  straighten() |> 
  make_req()

gender_txt <- content(request[[1]](), as = "text")

gender <- read_csv(gender_txt) |> 
  clean_names()
{% endhighlight %}

The end results is I want a variable, from that `SL_DOM_TPSD` indicator (time spent on domestic chores and care work by sex and urban/rural location) that can be represented like this:

<object type="image/svg+xml" data='/img/0290-time-share-bar.svg' width='100%'><img src='/img/0290-time-share-bar.png' width='100%'></object>

There are significant data wrangling challenges, though, in particular the different age categories used in each country, the different years that surveys were conducted, and the presence of multiple observations for some but not all countries.

The main reason for including this next snippet is to remind myself of what was needed to do to fiddle with those age categories. For example, note that some countries have values for multiple open ended categories like 3+ and 15+; we need a rule for deciding which of these is best for our desired constructed variable of men's share of adult domestic domestic and care work (in this case, 15+ is better than 3+, when both are available for a country):

{% highlight R lineanchors %}
count(gender, sex)      # two categories, FEMALE and MALE - no TOTAL
count(gender, age)      # many different ages used for different countries
count(gender, location) # there's ALLAREA, RURAL and URBAN

# should be only one indicator:
stopifnot(length(unique(gender$series_description)) == 1)
# which is 
# Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%) 

time_chores <- gender |> 
  # we don't want rural and urban, just country total:
  filter(location == "ALLAREA") |> 
  # we want the ages like 15+, 12+ etc, not those like 15-59 with an upper bound
  filter(grepl("^[0-9]*\\+$", age)) |> 
  # but not the retirees, which some countries include. We want the 15+, not 15+
  # and 65+ separately:
  filter(!age %in% c("65+", "85+", "60+")) |> 
  # calculate the male time spent as a proportion of total (male and female) time spent
  group_by(geo_area_name, time_period, age) |> 
  summarise(prop_male = value[sex == 'MALE'] / sum(value[sex == 'MALE'] + value[sex == 'FEMALE'])) |> 
  group_by(geo_area_name) |> 
  # Label the latest survey per country. Note that any modelling needs to
  # include a country random effect for the multiple observations per country:
  mutate(is_latest = ifelse(time_period == max(time_period), "Most recent", "Earlier")) |> 
  # limit to just the best age group, closest to adults, for each country/time:
  group_by(geo_area_name, time_period) |> 
  mutate(age = factor(age, levels = c("15+", "16+", "18+", "12+", "10+", "6+", "5+", "3+"))) |> 
  arrange(geo_area_name, time_period, age) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(iso3_code = countrycode(geo_area_name, origin = "country.name.en", destination = "iso3c"))
{% endhighlight %}


### Data on gender inequality

I spent quite a bit of time looking for data on gender inequality independent of the housework question. I wanted something that resembled women's and girls' opportunities in education and the economy more broadly. I had various deadends in pursuing this. My first idea was some kind of literacy measure - female literacy at some given age, or for adults overall, as a ratio for equivalent male literacy. But the various sources for this just didn't have enough observations. 

The main sources for literacy would be self-report in a census or possibly a social survey; or a standardised test at a given year of schooling. After some fruitless time with SDGs, the World Bank's World Development Indicators, and various other sources, I concluded that neither of these seem to be readily available in a comparable basis for enough years that matched with the year-country combinations that I had time-use data for.

I ended up using the [Gender Inequality Index (GII) from the UNDP](https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII) instead. Now, this index is complex and relies on a bunch of indicators that are obviously going to be at least as hard to measure as literacy - like level of secondary education (needs admin data or survey or census) and maternal mortality ratio (needs good civil registry, or survey data as a less satisfactory alternative). Here's how the GII is constructed:

<img src="/img/0290-gii-diagram.png" width = "100%">

But the GII is available for all country-year combinations, which simply can't be based on direct observations of these variables. Obviously the UNDP do a bunch of modelling to interpolate all the missing values. I didn't look into this but just trusted the UNDP to have done the best job possible. It's certainy very convenient to get this measure of gender inequality for so many countries (206 'countries', but this includes some regional groupings), and for so many years.

<object type="image/svg+xml" data='/img/0290-gii-lollipop.svg' width='100%'><img src='/img/0290-gii-lollipop.png' width='100%'></object>

There were multiple ways to download this GII data from the Human Development Reports website, but it turns out the best is to download all the Human Development Report data for the latest year in a single, big CSV:

{% highlight R lineanchors %}
# You can download all the HDR components (including GII):
df <- "hdr25.csv"

if(!file.exists(df)){
  download.file("https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv",
                destfile = df)
}
hdr <- read_csv(df)
{% endhighlight %}

From there it's straightforward data wrangling to extract just the GII data and combine with my other datasets using year and the ISO three character codes for countries to join by.

### Fitting the same mixed effects model with `lmer`, `gam` and `gamm`


{% highlight R lineanchors %}

{% endhighlight %}


<object type="image/svg+xml" data='/img/0290-country-effects-pairs.svg' width='100%'><img src='/img/0290-country-effects-pairs.png' width='100%'></object>


### Showing marginal effects


{% highlight R lineanchors %}

{% endhighlight %}


<object type="image/svg+xml" data='/img/0290-home-made-preds.svg' width='100%'><img src='/img/0290-home-made-preds.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0290-margeff-preds.svg' width='100%'><img src='/img/0290-margeff-preds.png' width='100%'></object>



### Interaction effects and splines



{% highlight R lineanchors %}

{% endhighlight %}


<object type="image/svg+xml" data='/img/0290-diagnose-0.svg' width='100%'><img src='/img/0290-diagnose-0.png' width='100%'></object>

<img src="/img/0290-compare-ti-and-s.png" width = "100%">



