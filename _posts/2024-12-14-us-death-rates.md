---
layout: post
title: Death rates by cause of death
date: 2024-12-14
tag: 
   - Health
   - Economics
description: I explore death rates by cause of death with OECD data, for the USA and other countries. Causes of death that are relatively high in the USA include assaults, accidents, suicides; diseases of the nervous systems (including Alzheimer's); and diseases of the circulatory system (including heart attacks).
image: /img/0285-us-1719-lim.png
socialimage: https:/freerangestats.info/img/0285-us-1719-lim.png
category: R
---
## Overview
OK, so the USA health system is very much in the news. One thing that has been getting a lot of coverage, not least because it was explicitly referred to in a written statement from the guy who seems to have murdered UnitedHealthcare CEO Brian Thompson, is the low life expectancy in the USA compared to the amount spent in that country on health services. So I have seen several variants on this chart going around:

<object type="image/svg+xml" data='/img/0285-life-exp-scatter.svg' width='100%'><img src='/img/0285-life-exp-scatter.png' width='100%'></object>

It's more common for the charts you see to focus on the upper right quadrant of that chart, relatively wealthy countries, in which case the USA is an even more prominent outlier. I like to see the whole world as context, not least because it shows other countries (including a couple in my working life patch, Tuvalu and Nauru) have as large a disappointing gap in life expectancy from what you might hope given income, spend on health, etc. There's also some countries - like Sri Lanka, Thailand and Bangladesh - with relatively high life expectancy given income and health spend.

Now, it only takes a moment's reflection to realise that this isn't just a matter of spend quality, effectiveness and efficiency of the health system. Life expectancy is heavily determined by other things. Most obviously:
* if people can't afford enough food, they die earlier for reasons related to malnutrition
* if people eat too much and too unhealthy types of food, they die earlier from diseases related to obesity (diabetes, heart attacks, etc) 
* if housing is poor they are more likely to get diseases like tuberculosis
* when water and sanitation is unsafe, people (including children, with a big impact on life expectancy) will die of cholera, diarrhea, typhoid, etc.
* if high numbers of young people commit suicide, die in accidents or are murdered it will directly and substantially reduce life expectancy.

These are all "public health" issues, but the causes and solutions are (mostly) not to be found in health expenditure but in general economic and social conditions. I say 'mostly' because of course health interventions can indeed make a difference to people with diabetes, mental health problems contemplating suicide, people who have been assaulted but picked up by the emergency services before dying, etc. But these are very much "ambulance at the bottom of the cliff" measures rather than a fence at the top.

Of course all this is well known in the right circles, and in fact an article had come out some weeks before Thompson's murder showing how the low life expectancy in the USA was materially impacted by deaths associated with [alcohol, drugs and firearms](https://www.aamcresearchinstitute.org/our-work/data-snapshot/narrowing-gap):

> "In 2022, there were more than 48,000 firearm-related deaths (more than half were recorded as suicides...); nearly 108,000 drug-related deaths; and more than 51,000 alcohol-induced deaths.While these numbers made up a small fraction of the nearly 3.3 million deaths in 2022 in the United States, they disproportionately affected children and younger adults — and, as a result, lowered the U.S. life expectancy at birth. If these deaths were eliminated (and other causes of death remained the same), life expectancy at birth would increase by 1.6 years."

So remembering that when I read the 'manifesto' of Thompson's alleged killer prompted me to look into death rates by cause of death myself. There is good data on this from the OECD, and not just for OECD countries. Here is my best chart I came up with illustrating the issues for the USA:

<object type="image/svg+xml" data='/img/0285-us-1719-lim.svg' width='100%'><img src='/img/0285-us-1719-lim.png' width='100%'></object>

As you can see, the horizontal axis in that  chart shows the difference in the age-standardised death rate between the USA and a set of comparison countries. The comparison countries I chose are those with a higher life expectancy than the USA that also had cause of death data available in the OECD database. In effect, these are the countries the USA might try to catch up to, if it were seeking to improve its life expecancy.

What we see in this chart is that indeed, the cause of death that is most disproportionately high in the USA is 'external' - assaults, accidents and suicides. With relation to cancer - one of the diseases where the health system is most important in terms of treatment prolonging life - the USA has unusually low death rates. (Also for the mysterious 'symptoms, signs, ill-defined causes' cause of death, which unfortunately seems to be a measurement anomaly that is much more present in a small number of countries than others).

The second most relatively high cause of death for the US compared to its comparator countries is diseases of the nervous system, which includes Alzheimer's. Then we have circulatory problems (eg heart attacks) and respiratory (influenza, pneumonia etc).

I drew lots more charts, some of which are highlighted in the rest of this blog, but if you're interested in them systematically here are PDFs that include the chart for each country available for different time periods: [2017 to 2019](/img/0285-all-countries-2017-2019.pdf) and for [2021](/img/0285-all-countries-2021.pdf). Adobe Acrobat on full screen mode (crtl-l) works well to flick through these. I considered making a Shiny app to let people explore this but decided the PDFs actually work better.

## More detail

OK that was the no-code summary but here is some more detail including how I drew those charts, some interesting variants including for other countries, and a bit of an exploration of which countries suffer most from which causes of death.

### Drawing the life expectancy scatter plot

First, here is the code for the life expectancy chart. You can see I am drawing the data from the World Bank's World Development Indicators. There's quite a lag in reporting both these figures which comes from the complexity of collecting the data and analysing them; 2021 was the year that had the most complete data for a large number of countries.

{% highlight R lineanchors %}
#===============Setup==================
library(tidyverse)
library(rsdmx)
library(janitor)
library(glue)
library(ggbiplot)
library(WDI)
library(ggrepel)

#' Collapse a character vector
#' 
#' @param ... passed through to \code{paste()}
#' @details
#' this is just a convenience wrapper around paste()
#' 
pastec <- function(...){
  paste(..., collapse = ', ')
}

#===============Part 1 - life expectancy compared to health spend============
#---------------life expectancy--

# had to explore a bit to find the indicators to use
# w <- WDIsearch("health")
# View(w)

h <- WDI(indicator = c(health_spend = "SH.XPD.CHEX.PC.CD", 
                       life_exp = "SP.DYN.LE00.IN"),
         start = 2020) 

# find the 'countries' that are really groupings of countries         
h  |>
  filter(!(iso2c %in% c("ZW", "ZM", "ZA", "XK") | 
             !(grepl("^[XZ]", iso2c) |
             iso2c %in% c("EU", "V1", "V2", "V3", "V4", "OE")))) |>
  distinct(country, iso2c)


h2 <- h  |>
  # remove regions and groupings (which have ISO2 code beginning with Z or X
  # but leave in 4 countries with real codes beginning Z or X)
  # and also knock out some specific things like EU, OECD average, etc
  filter((iso2c %in% c("ZW", "ZM", "ZA", "XK") | 
            !(grepl("^[XZ]", iso2c) |
                iso2c %in% c("EU", "V1", "V2", "V3", "V4", "OE")))) |>
  drop_na() |>
  group_by(country) |>
  arrange(desc(year)) |>
  # originally i picked the latest year for each country, but in so many
  # cases it was 2021 I decided to just force it to be 2021
  filter(year == 2021) |>
  slice(1)  |>
  ungroup()

# going ot use this model to identify 'outlier' countries worth labelling
mod <- lm(life_exp ~ log(health_spend), data = h2)

# add the residuals of hte model and label any countries not from 2021 (this
# label change was more necessary when I had multiple years in the data, but
# I've left it in for future reference)
h3 <- h2 |>
  mutate(res = as.numeric(residuals(mod)),
         country_yr = ifelse(year == 2021, 
                             country, glue("{country}, {year}")))

# Draw scatter plot
h3 |> 
  ggplot(aes(x = health_spend, y = life_exp)) +
  geom_smooth(method = "lm", colour = "lightgreen", se = FALSE) +
  geom_point() +
  # label some interesting countries:
  geom_text_repel(data = 
                    filter(h3,
                           health_spend > 8500 | 
                             life_exp > 84 | 
                             life_exp < 55 |
                             res < -10 |
                             res > 7),
                  aes(label = country_yr,
                      # annoying ggrepel couldn't put Norway in a good position automatically
                      # so I am going to move it ad hoc:
                      x = ifelse(country == "Norway", health_spend * 1.189, health_spend)),
                  colour = "steelblue", 
                  seed = 125,
                  size = 2.8) +
  scale_x_log10(label = dollar) +
  labs(x = "Health expenditure per capita (US dollars, logarithmic scale)",
       y = "Life expectancy",
       title = "Health expenditure is associated with higher life expectancy",
       subtitle = "USA is a high income outlier, but many poorer countries also have poor life expectancy given health spend per person.",
       caption = "Source: data for 2021 from the World Development Indicators; analysis by freerangestats.info")
{% endhighlight %}

Probably the only noteworthy thing in that code was that I explicitly fit a model of expected life expectancy given a level of health spending, purely to use the residuals from that model to identify outliers to draw on the chart. An interesting example of a model servicing a visualisation rather than the other way around.

### Wrangling cause of death data and metadata and drawing the cause of death plots

Next, how did I get the data for the cause of death from the OECD? It's all available in OECD.Stat, and you can use the [Data Explorer](https://data-explorer.oecd.org/vis?_ga=2.241262853.682686041.1734140631-789640812.1734140631&tm=cause%20of%20death&pg=0&snb=13&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HEALTH_STAT%40DF_COM&df[ag]=OECD.ELS.HD&df[vs]=1.0&dq=.A......_T.STANDARD....&pd=2015%2C&to[TIME_PERIOD]=false) to find the data you want. Once there you can use the 'Developer API' button to get a link to both the metadata and the data itself. We need the metadata in this case because the data download just has codes for cause of death, and we want the full names.

The meta data is extensive and comes in the form of a complex nested XML object, in SDMX format. We use this at my work but I personally am not an expert so there was a bit of fiddling to find and extract the codelists that I wanted.
{% highlight R lineanchors %}
#===============Part 2 - comparing various cause of death numbers=========

#--------------metadata for causes of death------------------
md <- readSDMX("https://sdmx.oecd.org/public/rest/dataflow/OECD.ELS.HD/DSD_HEALTH_STAT@DF_COM/1.0?references=all")

# English names for each of the code lists
sapply(md@codelists@codelists, \(x)x@Name$en)
# noting number 7 is Cause of death and 2 is area ie country
{% endhighlight %}

That last command extracts from the `md` S4 object (hold your breath): the English element from the `Name` slot from the `codelists` slot from the `codelists` slot (yes, that's nested `codelists` slots). It gives us this list of the codelists available:

```
> sapply(md@codelists@codelists, \(x)x@Name$en)
 [1] "Codelist for age"                          
 [2] "Codelist for reference areas and geocoding"
 [3] "Codelist for concept \"Sex\""              
 [4] "Codelist for Unit of measure"              
 [5] "Codelist for calculation methodology"      
 [6] "Codelist for cancer site"                  
 [7] "Cause of death"                            
 [8] "Codelist for diseases"                     
 [9] "Gestation period threshold"                
[10] "Health status"                             
[11] "Codelist for health status measures"       
[12] "Socio-economic status"                     
[13] "Decimals"                                  
[14] "Code list for concept \"Frequency\""       
[15] "Observation Status"                        
[16] "Code list for the Unit Multiplier" 
```
I only need numbers 2 and 7, which I extracted in this hacky way:

{% highlight R lineanchors %}
#' convenience function for extracting codelists from sdmx metadata
#' 
#' This is not very robust but does the job for today
extract_codes <- function(metadata, id, description_name = "description"){
  codes <- md@codelists@codelists[[id]]@Code
  lookup <- tibble(code = sapply(codes, \(x)x@id), 
         description = sapply(codes, \(x)unlist(x@name)))
  names(lookup)[2] <- description_name
  return(lookup)
}

cod_codes <- extract_codes(md, 7, "cause_of_death") 
area_codes <- extract_codes(md, 2, "country") |>
  # country is still a list within a list so need to extract further
  mutate(country = as.character(sapply(country, \(x){x['en']}))
)
{% endhighlight %}

That gives me code-to-description lookup tables for both the cause of death and area (ie country) codes.

Now, the cause of death codes are hierarchical and some of them are parents of others. For example, tuberculosis is a subset of 'Certain infectious and parasitic diseases' and if you include them both you are in a sense double-counting. I decided for my purposes I only wanted the highest level codes. This must be encoded in the SDMX somewhere (because the OECD Data Explorer shows it correctly), but I couldn't find it in the metadata. For example, the snippet below just returns all NAs for the `parentCode` slot in the metadata that I would have thought should indicate a parent when one existed (like for Tuberculosis).

{% highlight R lineanchors %}
# the codeelists Code should have parentCode to show the hierarchical nature
# of the cause of death codes, but it doesn't seem to be there
sapply(md@codelists@codelists[[7]]@Code, \(x)x@parentCode)
{% endhighlight %}

In the end I had to make by hand a vector of the highest level codes, by printing to the console all the codes we had and eliminating the ones that the Data Explorer showed me were children of others:

{% highlight R lineanchors %}
# you are meant to be able to get parent codes from this but they all look to be
# NA (see above( so I couldn't see how to do this. So instead I've made a list
# by hand of all those at the top level of hte classification
high_level_cod <- c(
  "Certain infectious and parasitic diseases",
  "Neoplasms",
  "Diseases of the blood and blood-forming organs",
  "Endocrine, nutritional and metabolic diseases",
  "Mental and behavioural disorders",
  "Diseases of the nervous system" ,
  "Diseases of the respiratory system" ,
  "Diseases of the circulatory system" ,
  "Diseases of the digestive system",
  "Diseases of the skin and subcutaneous tissue",
  "Diseases of the musculoskeletal system and connective tissue" ,
  "Diseases of the genitourinary system"  ,
  "Certain conditions originating in the perinatal period",
  "Congenital malformations, deformations and chromosomal abnormalities",
  "Symptoms, signs, ill-defined causes" ,
  "External causes of mortality",
  "Codes for special purposes: COVID-19" 
)
{% endhighlight %}

OK, so now we can import the data, and join it to the codelist looks I made earlier:

{% highlight R lineanchors %}
# The actual death rates (standardised by age)
death_rates <- readSDMX("https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_COM,1.0/.A..DT_10P5HB.._T...STANDARD....?startPeriod=2015&dimensionAtObservation=AllDimensions") |>
  as_tibble() |>
  clean_names() |>
  left_join(cod_codes, by = c("death_cause" = "code")) |>
  left_join(area_codes, by = c("ref_area" = "code"))
{% endhighlight %}

It took me a while but I realised eventually I had a challenge that not all countries had data for all years. The first few versions of charts I drew were misleading in comparing (for example) countries with data only up to 2019 with countries like the USA that had data up to 2021, or Australia which has data to 2022. In a stable time when cause of death doesn't change much year by year this might not matter, but that doesn't describe the 2020s! My mistake made it look like the USA was worse relatively speaking in terms of Covid-19 deaths than they actually are. Here's some summaries of how many countries have data each year:

```
> # A problem, its different sets of countries for each year
> count(death_rates, time_period)
# A tibble: 8 × 2
  time_period     n
  <chr>       <int>
1 2015         2144
2 2016         2154
3 2017         2059
4 2018         2057
5 2019         2047
6 2020         1908
7 2021         1414
8 2022          439
> 
> death_rates |>
+   count(country, time_period) |>
+   count(country, sort = TRUE, name = "number_years") |>
+   count(number_years, name = "number_countries")
# A tibble: 6 × 2
  number_years number_countries
         <int>            <int>
1            2                3
2            4                1
3            5                3
4            6               11
5            7               19
6            8                9
> 
> death_rates |>
+   count(country, time_period) |>
+   count(time_period, sort = TRUE, name = "number_countries") 
# A tibble: 8 × 2
  time_period number_countries
  <chr>                  <int>
1 2015                      45
2 2016                      45
3 2017                      43
4 2018                      43
5 2019                      43
6 2020                      39
7 2021                      29
8 2022                       9
```
Some countries have as few as 2 years of data (noting that I set 2015 as the starting point myself); most have 6 or 7.

OK, so once I'd recovered from that fishhook, and had a bit of trial and error with drawing charts, I eventually put all my code doing this into this single-analysis (ie not portable, only for use in this script) function. You can see it takes arguments for the years being used, which countries to compare to, and different ways of colouring the bars. I only ended up using the version that colours them according to the all-country average value because that seemed to me to give extra information for minimal extra visual clutter; but the alternative is ok too.

{% highlight R lineanchors %}
#' Draw a bar chart for one country showing its death rates in comparison to an average
bar_one_country <- function(the_country, 
                            comparison_countries = NULL,
                            subtitle = NULL,
                            st_width = 120,
                            years = 2017:2019,
                            bar_fill = c("country_average", "this_country")){
  
  bar_fill <- match.arg(bar_fill)
  
  death_rates2 <- death_rates |>
    filter(time_period %in% years)
  
  if(!the_country %in% death_rates2$country){
    stop(glue("Could not find {the_country} in {paste(years, collapse = ', ')}"))
  }
  
  
  if(is.null(comparison_countries)){
    
  } else {
    check <- comparison_countries %in% death_rates2$country
    if(!all(check)){
      warning(glue("Could not find {pastec(comparison_countries[!check])}"))
    }
    
    death_rates2 <- death_rates2 |>
      filter(country %in% c(the_country, comparison_countries))
    
    if(is.null(subtitle)){
      subtitle <- glue("Comparison: {pastec(comparison_countries[check])}")
    }
  }
  

  
  # some countries won't have data for all years
  bad_countries <- death_rates2 |>
    count(country, time_period)|>
    count(country) |>
    filter(n != length(years)) |>
    pull(country)
  
  if(length(bad_countries) > 0){
    warning(glue("Dropping {pastec(bad_countries)} as missing some years"))
    if(the_country %in% bad_countries){
      stop("This is fatal for drawing this graph...")
    }
    death_rates2 <- death_rates2 |>
      filter(!country %in% bad_countries)
    
    comparison_countries <- comparison_countries[!comparison_countries %in% bad_countries]
  }
  
  one_wide <- death_rates2 |>
    filter(cause_of_death %in% high_level_cod) |>
    mutate(cause_of_death = case_when(
      cause_of_death == "Diseases of the nervous system" ~ "Diseases of the nervous system (includes Alzheimer's and Parkinson's)",
      cause_of_death == "Mental and behavioural disorders" ~ "Mental and behavioural disorders (includes Dementia)",
      cause_of_death == "External causes of mortality" ~ "External causes of mortality (includes accidents, assaults and self-harm)",
      cause_of_death == "Neoplasms" ~ "Neoplasms (eg cancer)",
      TRUE ~ cause_of_death
    )) |>
    group_by(cause_of_death, time_period) |>
    mutate('country_average' = mean(obs_value)) |> 
    ungroup() |>
    filter(country == the_country) |>
    select(cause_of_death, time_period, one_country = obs_value, 'country_average') |>
    mutate(time_period = as.numeric(time_period)) |>
    mutate(cause_of_death = fct_reorder(str_wrap(cause_of_death, 30), one_country)) |>
    group_by(cause_of_death) |>
    mutate(comp_diff = mean(one_country) - mean(country_average),
           country_average = mean(country_average)) |>
    ungroup()
  
  p <- one_wide |>
    distinct(cause_of_death, comp_diff, country_average) |>
    mutate(cause_of_death = fct_reorder(str_wrap(cause_of_death, 40), comp_diff)) |>
    ggplot(aes(x = comp_diff, y = cause_of_death)) +
    scale_fill_viridis_c() +
    labs(x = glue("Age standardised deaths per 100,000 in {the_country} compared to unweighted average of {length(unique(death_rates2$country))} countries"),
         y = "",
         caption = "Source: OECD. Analysis by freerangestats.info",
         title = glue("Where do extra deaths in {the_country} come from in {pastec(years)}?"),
         subtitle = str_wrap(subtitle, width = st_width))
  
  if(bar_fill == "this_country"){
    p <- p +
      geom_col(aes(fill = comp_diff)) +
      labs(fill = glue("Rate in {the_country}"))
    
  } else {
    p <- p +
      geom_col(aes(fill = country_average)) +
      labs(fill = "All-country average")
    
  }
  
  return(p)
}
{% endhighlight %}

Now that I've got a function to draw a chart for one country's cause of death in a given subset of years, compared to the other countries that all also have data for that subset of years, I'm able to start producing graphs! Here's the first one I actually made, even though I haven't highlighted it in the "Overview" part of this post. USA 2017 to 2019, compared to all countries available:

<object type="image/svg+xml" data='/img/0285-us-1719.svg' width='100%'><img src='/img/0285-us-1719.png' width='100%'></object>

The difference between this and the one I used in the overview is the comparison to all countries available, not just those with higher life expectancy than the USA which I'll come to later. I think this makes it a bit less useful for the purpose of our fundamental question which is "what is wrong with the USA", although the results aren't radically different.

I also looked at the USA in 2021, during the time of Covid-19:

<object type="image/svg+xml" data='/img/0285-us-21.svg' width='100%'><img src='/img/0285-us-21.png' width='100%'></object>

We see that for this period at least, the USA experienced a lower death rate from Covid-19 than other countries were.

Those two USA-compared-to-all-countries charts were just made with these short snippets of code:

{% highlight R lineanchors %}
bar_one_country("United States", bar_fill = "country_average",
                subtitle = "Comparison to all countries with available cause of death rates.")

bar_one_country("United States", years = 2021, 
                comparison_countries = unique(death_rates$country))
{% endhighlight %}

To draw the PDFs with one page per country, I made use of the fact that each time you print a new chart to an open PDF device it adds a page, making this a very quick and easy way to store all the charts available with some combination of parameters: 

{% highlight R lineanchors %}
Cairo::CairoPDF("0285-all-countries-2017-2019.pdf", 
                width = 11, height = 8, title = "Cause of deaths 2017-2019")
for(tc in sort(unique(death_rates$country))){
  try(print(bar_one_country(tc, bar_fill = "country_average")))
}
dev.off()

Cairo::CairoPDF("0285-all-countries-2021.pdf", 
                width = 11, height = 8, title = "Cause of deaths 2021")
for(tc in sort(unique(death_rates$country))){
  try(print(bar_one_country(tc, years = 2021, bar_fill = "country_average")))
}
dev.off()
{% endhighlight %}

For comparsions with just those countries that had better life expectancy than the USA, I went back to my data for that life expectancy scatter plot and extracted the countries from there. One of them (South Korea) needed to have its name changed by hand to work with the OECD data. Once we have that vector of countries, the `bar_one_country()` function Just Works; here is the code to draw charts for the USA, Australia and Estonia at different times:

{% highlight R lineanchors %}
#----------comparisons with a smaller set of countries--------------

better_countries <- h2 |>
  filter(life_exp > h2[h2$country == "United States", ]$life_exp) |>
  mutate(country = ifelse(grepl("Korea, Rep.", country, fixed = TRUE), "Korea", country)) |>
  pull(country)

bar_one_country("United States", 
                comparison_countries = better_countries, 
                years = 2017:2019)

bar_one_country("United States", 
                comparison_countries = better_countries, 
                years = 2021)

bar_one_country("Australia", 
                comparison_countries = better_countries, 
                years = 2022)

bar_one_country("Estonia", 
                comparison_countries = better_countries, 
                years = 2022)
{% endhighlight %}

And here are the three charts (the United States 2017 to 2019 chart has already been shown in the 'Overview' section of this blog):

<object type="image/svg+xml" data='/img/0285-us-21-lim.svg' width='100%'><img src='/img/0285-us-21-lim.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0285-aus-22-lim.svg' width='100%'><img src='/img/0285-aus-22-lim.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0285-est-22-lim.svg' width='100%'><img src='/img/0285-est-22-lim.png' width='100%'></object>

Australia's highest relative cause of death being 'endocrine, nutritional and metabolic diseases' (eg diabetes), and with relatively low death rate from circulatory diseases.

Estonia in stark contrast having a very high death rate from circulatory disease, compared to these particular high-life-expectancy countries.

### Which causes are associated with which countries?

Finally, I also scratched my curiousity itch on whether principal components analysis and visualisations could help create and communicate a sense of which countries are associated with which cause of death. This blog post is already very long so I won't go into much detail on this. I'll just say that the changing country availability by year was again a fishhook, and the main visualisaiton problem was the long names of the causes of death. In the end, I had to hide most of the labels of causes of death other than those that really made a contribution to the variation in the first two principal components.

{% highlight R lineanchors %}
#-----------biplot--------

cod_biplot <- function(years, cod_col = "steelblue", min_loading = 0.25, ...){
  all_countries <- death_rates |>
    filter(cause_of_death %in% high_level_cod &
             time_period %in% years) |>
    group_by(country, cause_of_death) |>
    summarise(value = sum(obs_value)) |>
    spread(cause_of_death, value, fill = 0)
  
  all_countries_m <- as.matrix(all_countries[ ,-1])
  # decided not to scale the countries, but it wouldn't be insane; here's how to do it:
  #all_countries_m <- t(scale(t(all_countries_m)))
  
  row.names(all_countries_m) <- pull(all_countries, 1)
  colnames(all_countries_m) <- gsub("Diseases of the ", "", colnames(all_countries_m))
  colnames(all_countries_m) <- gsub("Certain ", "", colnames(all_countries_m))
  #colnames(all_countries_m) <- gsub(" system", "", colnames(all_countries_m))
  
  pc <- princomp(all_countries_m)
  select_cols <- loadings(pc)[, 1:2] |>
    as_tibble() |>
    mutate(cn = row.names(loadings(pc)[,1:2])) |>
    mutate(cn = ifelse(abs(Comp.1) > min_loading | abs(Comp.2) > min_loading, cn , "")) |>
    pull(cn)
  
  colnames(all_countries_m) <- select_cols
  pc <- princomp(all_countries_m)
  
  
  p <- ggbiplot(pc, labels = row.names(all_countries_m),
           varname.color = cod_col, ...) +
    labs(subtitle = glue("Cause of death by country: {pastec(years)}"))
  
  return(p)
}

cod_biplot(2017:2019, min_loading = 0.3)
cod_biplot(2020, min_loading = 0.1)
cod_biplot(2021)
{% endhighlight %}

Here are the three charts drawn by that code, each one representing a different time period.

<object type="image/svg+xml" data='/img/0285-biplot-1719.svg' width='100%'><img src='/img/0285-biplot-1719.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0285-biplot-20.svg' width='100%'><img src='/img/0285-biplot-20.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0285-biplot-21.svg' width='100%'><img src='/img/0285-biplot-21.png' width='100%'></object>

It never pays to overthink a biplot. Just go with the flow and interpret them intuitively.

Do these plots help? I think they do a bit. For example, it helps to see in a single chart - say, the one for 2017 to 2019 - the eastern European countries associated with circulatory system diseases, and Latin America and others with diabetes; cancer in the Balkans.

That's all for now. Hopefully this helps at least someone understand the variation in cause of death across countries, and how the USA lowish life expectancy is a more complex phenomenon than just being the fault of its health insurance industry.









