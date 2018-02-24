---
layout: post
title: Inter-country inequality and the World Development Indicators
date: 2017-07-22
tag: 
   - Reproducibility
   - DataFromTheWeb
   - Inequality
   - Economics
   - R
description: I play around with population-weighted income inequality of countries with data from the World Development Indicators, re-creating (with some amendments) some graphics from Branko Milanovic's recent book "Global Inequality".
image: /img/0105-scatter.svg
socialimage: http://ellisp.github.io/img/0105-scatter.png
category: R
---

I recently read the high quality book [*Global Inequality*](http://www.hup.harvard.edu/catalog.php?isbn=9780674737136) by Branko Milanovic.  When reading this sort of thing, I often find I can increase my engagement with a topic by playing around with the data myself.  In this day and age, this is much more likely to be possible than a couple of decades back!  I remember when I first studied development economics, typing into Lotus 123 the data from tables in the back of a *World Development Report*.  These days to get the same data we just fire up R and get the data from the `WDI` package, which speaks directly to the World Bank's World Development Indicators API.

## Re-creating three charts

Chapter 4 of *Global Inequality* is entitled "Global Inequality in This Century and the Next".  It explores a range of issues to do with inter-country inequality and global inequality. The difference between the two is that "global" compares the income (or wealth, although there usually isn't adequate data to do this) of all global citizens on an equal basis whereas "inter-country" uses the average income in each country and helps explore the idea of "citizenship rent" ie the bonus one gets from having been born in a wealthier place.  I won't try to summarise the discussion in this excellent chapter, I just recommend reading the original.

I set out to reproduce the first three figures in the chapter and had an interestingly partial success.  That is, a partial success, that was partial for interesting reasons.

### Global income inequality among countries
First, here is my version of Milanovic's Figure 4.1.  His original figure looks close to an extended version of what I have in just the left panel. This is based on purchasing-power parity (PPP) GDP per capita - that is, 
adjusted not just for exchange rates but for differing price levels in different countries.  However, Milanovic used data back to 1960, and for more countries than I have available.  Like mine, his data were drawn 
from the World Development Indicators.  But in 2014, the World Development Indicators were updated with new PPP 
references.  Because of [concerns about inaccuracy](http://databank.worldbank.org/data/download/WDIrevisions.xls) the data were limited to 1990 onwards.  It seems also at some time in the past few years 
the WDI were restricted to data about currently existing countries (eg excluding data from the USSR or Yugoslavia).  

For these reasons, the left pane of my chart below differs from the original in *Global Inequality*.  And to get the picture over a longer period, in the right pane I had to use a constant US dollar GDP series instead of a PPP one.  I think this not only overstates inequality (because a dollar buys more in poor countries than rich ones), it hides some of the patterns; but it's better than nothing.  

If this were important, I'd definitely want to do something about the slowly-changing-dimension problem; that is, the people who have been excluded from counting because their countries have not been in continuous existence over the whole period in question.  *What* to do about this could be a blog post (or a book) in itself...

<img src='/img/0105-gini-usd.svg' width='100%'>

However, the basic story is still supported even though absolute levels differ from my version to the one based on previous data.  Reflecting income growth in China and India, there is a steep decline in the population-weighted Gini coefficient from 1990 or 2000 in my chart; in Milanovic's original there was a gradual decline from 1980, accelerating from 2000.  My unweighted Gini coefficients show similar patterns to his, with inequality declining from around 2000.  Average incomes in Latin America, Eastern Europe and Africa failed to catch up with those in wealthier countries in the 1980-2000 period, but have been converging since 2000.

### Difference in the combined (population-weighted) growth rates

The second chart I tried to reproduce was Figure 4.2, which shows differing growth rates between the advanced economies (treated as a single bloc) and the "principal emerging economies (excluding China)".  Milanovic defined this latter group as India, Brazil, Indonesia, South Africa and Vietnam; I've chosen a larger group.  When the bar is above 0, the emerging economies have grown faster than the advanced economies.

<img src='/img/0105-growth-gap.svg' width='100%'>

The data for Vietnam available to me only started in the 1980s, and this alone leads to quite a difference in conclusion and interpretation between my chart above and Milanovic's original.  Vietnam was ravaged by war for decades until 1975.  It's a populous country and if its data were included in the chart above it would definitely drag *down* the "emerging" economies' pre-1980 combined growth rate, and *up* their rate since 1980.  Without Vietnam in the dataset, I see considerably more growth in the historical "emerging" economies than is present in the figure in the original book.  Playing around with different combinations of the "principal" emerging economies didn't make much difference.  I chose the countries I did on the basis of population size and data availability.

As in the original chart, I still have a generally stronger relative performance by the (non-China) emerging economies from 2000 onwards than previously; but the trend over time isn't as dramatic when Vietnam is excluded, more emerging economies are added in, and the updated data source used.

### Level of GDP per capita in 1970 and average subsequent growth

Finally, I had good success in creating a substantively similar chart to Milanovic's Figure 4.3, shown below:

<img src='/img/0105-scatter.svg' width='100%'>

The descriptive/historical interpretation of my chart is identical to Milanovic's.  For Asian and Western countries, the countries with lower incomes in 1970 have successfully played catch-up, with higher average annual economic growth for poorer countries.  For Eastern European, Latin American and African countries, this hasn't been the case, with no relationship between income in 1970 and subsequent growth.

I have a little more information in my charts than was in the original.  Point sizes now show population, which is important for appreciating the significance of India and China in the right hand panel in particular.  I also include both weighted and unweighted regression lines; Milanovic discusses the importance of weighting by population in this sort of analysis but I think in the chart he has presented only the unweighted regression line (the substance of the conclusion stays the same).

If you're wondering who the tiny "Asian" country is that had negative average GDP per capita growth over this 46 year period (!) it is Kiribati (pronounced [K EE R ih b ae s](http://www.pronouncenames.com/pronounce/Kiribati) - unlike as spoken when it was mentioned once on the Gilmore Girls).  Following Milanovic, I included Pacific countries in the "Asian" grouping.  Of countries with data available, only Liberia, Democratic Republic of the Congo, Central African Republic, Madagascar, Niger had worse shrinkage in economic production per capita.

## Delving

Here's how I did this.  First, here's the code to load up R functionality for the whole session, set a colour palette I'm going to use a couple of times, explore the names of the various data collections available in the `WDI` interface, and download the datasets I need for GDP per capita (constant US dollars), GDP per capita (constant PPP US dollars) and population:

{% highlight R %}
library(tidyverse)
library(scales)
library(WDI)
library(ineq) # for Gini
library(acid) # for weighted.gini
library(forcats)
library(RColorBrewer)
library(directlabels)
library(ggrepel)
library(testthat)
library(forecastHybrid)
library(stringr) # for str_wrap

# Colour palette for making sure charts use the same colours:
gini_palette <- brewer.pal(3, "Set2")
names(gini_palette)[c(1,2)] <- c("Unweighted", "Weighted")


#==============download data===========
tmp1 <- WDIsearch("GDP per capita")
tmp2 <- WDIsearch("population")
# "NY.GDP.PCAP.PP.KD"    "GDP per capita, PPP (constant 2005 international $)"  
# - this one sounds and looks like what Milanovic used but now it only goes to 1990
# In http://databank.worldbank.org/data/download/WDIrevisions.xls,
# it is stated that from May 2014 PPP data are onlyu provided back to 1990 due
# to concerns about the risk of inaccuracy the further back from the benchmark year

# alternatives:
# "NY.GDP.PCAP.KD"       "GDP per capita (constant 2000 US$)" ie not adjusted for inter-country price differences
# "GDP.PC.KD"              "GDP per Capita, constant US$, millions" - doesn't seem to exist
# "NY.GDP.PCAP.CD"       "GDP per capita (current US$)"    ie not corrected for inflation at all
#  "NY.GDP.PCAP.KN"       "GDP per capita (constant LCU)"  ie local currency
# SPPOPTOTL Population, millions at least this is straightforward...

gdp_ppp <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", "SP.POP.TOTL"), start = 1960, end = 2016, extra = TRUE)
gdp_constant <-  WDI(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL"), start = 1960, end = 2016, extra = TRUE)
{% endhighlight %}

I wanted to get a feel for the data in its most basic presentation, so I drew myself a time series line chart to see if China's economy grew as fast in the data as I expected it to:

<img src='/img/0105-line-charts.svg' width='100%'>

{% highlight R %}
#============time series chart=================
p1 <- gdp_constant %>%
  rename(gdp = NY.GDP.PCAP.KD)  %>%
  filter(country %in% c("United States", "United Kingdom", "France", "China", "India", "Thailand", "South Africa")) %>%
  ggplot(aes(x = year, y = gdp, colour = country)) +
  geom_line() +
  scale_y_log10("GDP per capita, constant US dollars", label = dollar) +
  labs(x = "", caption = "Source: World Development Indicators") +
  ggtitle("GDP per capita over time, selected countries")

direct.label(p1)
{% endhighlight %}

Next I had to sort a problem of which "countries" to include.  The WDI data includes a number of regional aggregations, and also countries that are missing quite a bit of data.

I had thought that we might find the number of countries with GDP data (not the PPP data, which I knew started in 1990) might increase to 1990 and then be flat, but we see in the chart below that it's more complicated than that:

<img src='/img/0105-data-coverage.svg' width='100%'>

Here's a chart of the countries that have at least *some* data, but not for every year from 1960 to 2016.  As well as things I expected (Russian Federation and Germany don't enter the dataset until relatively late, due to changing national boundaries), there are a few surprises; for example, New Zealand's GDP per capita in this particular dataset doesn't start until 1977:

<img src='/img/0105-countries-coverage.svg' width='100%'>

Here's the code for sorting through the country and regional issues:

{% highlight R %}
#============country coverage==========
# first we need to work out which "countries" are actually regional aggregations
country_codes <- gdp_ppp %>%
  dplyr::select(iso2c, country) %>%
  distinct()
# everything begining with X except XK (Kosovo) is a grouping eg "Euro area"
# everything with a number in it is a a group eg T5 is South Asia (IDA and IBRD)
# ZF, ZG, ZQ, ZT, ZJ are regions
# OE is the OECD
# EU is the European Union

codes <- country_codes$iso2c
exes <- codes[grepl("^X", codes)]
exes <- exes[exes != "XK"]

# regs has the iso2c codes for regional aggregfations:
regs <- c(exes, codes[grepl("[0-9]", codes)], "ZF", "ZG", "ZQ", "ZT", "ZJ", "OE", "EU")

# how many countries have data each year?
gdp_constant %>%
  filter(!iso2c %in% regs) %>%
  group_by(year) %>%
  summarise(countries = sum(!is.na(NY.GDP.PCAP.KD))) %>%
  ggplot(aes(x = year, y = countries)) +
  ggtitle("Variable GDP coverage in the World Development Indicators") +
  geom_line() +
  labs(y = "Number of countries with \nconstant US prices GDP data in the WDI", 
       caption = "Source: World Development Indicators")

country_coverage <- gdp_constant %>%
  filter(!iso2c %in% regs) %>%
  group_by(country) %>%
  summarise(prop_complete = sum(!is.na(NY.GDP.PCAP.KD)) / n()) %>%
  arrange(desc(prop_complete))

# some surprises here.  For example New Zealand only has data from 1977 onwards
country_coverage %>%
  filter(prop_complete != 0 & prop_complete != 1) %>%
  mutate(country = fct_reorder(country, prop_complete)) %>%
  ggplot(aes(x = prop_complete, y = country)) +
  geom_point() +
  ggtitle("Countries with partial GDP data in the WDI") +
  scale_x_continuous("Percentage of years 1960-2016 with data on GDP in constant US dollars",
                     label = percent) +
  labs(y = "", caption = "Source: World Development Indicators")
{% endhighlight %}

Here's the code that draws the three main charts.  The `tidyverse` makes it pretty easy to do this calculation of summary statistics like Gini or weighted Gini for arbitrary groupings and turn them into nice charts.

{% highlight R %}
#================Variant of figure 4.1 on Gini over time===========
# constant prices version, which we will then rbind onto the PPP version:
tmp <- gdp_constant %>%
  rename(gdp = NY.GDP.PCAP.KD) %>%
  mutate(var = "USD exchange rates")

gdp_ppp %>%
  rename(gdp = NY.GDP.PCAP.PP.KD) %>%
  mutate(var = "Purchasing Power Parity") %>%
  rbind(tmp) %>%
  filter(country %in% filter(country_coverage, prop_complete == 1)$country) %>%
  # need to knock out the NA values in the PPP data:
  filter(!is.na(gdp)) %>%
  group_by(year, var) %>%
  summarise(Unweighted = Gini(gdp),
            Weighted = weighted.gini(gdp, SP.POP.TOTL)$bcwGini) %>%
  gather(variable, value, -year, -var) %>%
  mutate(variable = fct_reorder(variable, -value, fun = last)) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  facet_wrap(~var) +
  geom_line() +
  theme(legend.position = "right") +
  labs(colour = "",
       y = "Gini or weighted Gini",
       caption = "Source: World Development Indicators") +
  ggtitle("Inter-country inequality over time",
          "GDP per capita, two different methods of comparing across countries
Only includes countries with full coverage of data 1960 to 2016
Inspired by Figure 4.1 from Milanovic, Global Inequality") +
  scale_x_continuous("", limits = c(1960, 2020)) +
  scale_colour_manual(values = gini_palette) 

#================Figure 4.2====================

# excluding Vietnam because they don't have any data now in WDI until the 1980s
# adding in some other countries by population - not clear why excluded from the original
emerging <- c("India", "Brazil", "Indonesia", "South Africa", "Pakistan", "Bangladesh", "Nigeria",
              "Mexico", "Thailand", "Philippines", "Egypt, Arab Rep.", "Turkey", "Korea, Rep.",
              "Iran, Islamic Rep.", "Myanmar", "Colombia", "Congo, Dem. Rep.")

expect_equal(sum(!emerging %in% gdp_constant$country), 0)

gdp_constant %>% 
  rename(gdp = NY.GDP.PCAP.KD,
         pop = SP.POP.TOTL) %>%
  filter(country %in% c("European Union", "United States", "Japan", emerging)) %>%
  mutate(type = ifelse(country %in% emerging, "emerging", "advanced")) %>%
  group_by(year, type) %>%
  # needs to be false so bars don't draw when not present
  summarise(gdp = sum(gdp * pop, na.rm = FALSE)) %>%
  group_by(type) %>%
  mutate(diffgdp = c(NA, diff(gdp)),
         growth = diffgdp / (gdp - diffgdp)) %>%
  dplyr::select(year, type, growth) %>%
  spread(type, growth) %>%
  mutate(difference = emerging - advanced) %>%  
  ggplot(aes(x = year, weight = difference)) +
  geom_bar() +
  ggtitle("GDP growth gap between 'emerging' and 'advanced' countries over time",
          "Excluding China for illustrative reasons, and Vietnam for data reasons.
Inspired by Figure 4.2 from Milanovic, Global Inequality") +
  scale_y_continuous("Difference in population-weighted growth rate between 
emerging and advanced economies, in percentage points",
                     label = percent) +
    labs(x = paste(str_wrap(paste0("Emerging countries defined as ", 
                             paste(emerging, collapse = ", ")),
                             100),
             "\n\nAdvanced countries defined as EU, USA and Japan"), 
         caption = "Source: World Development Indicators")

# This is much more positive in the late 1960s and 1970s than in the original,
# and perhaps this is all because he had Vietnam (during the war...) and I don't
# have data.


#====================Figure 4.3 scatter plots========
end_year <- 2016
gdp_summary <- gdp_constant %>%
  filter(!iso2c %in% regs & year >= 1970) %>%
  rename(gdp = NY.GDP.PCAP.KD,
         pop = SP.POP.TOTL) %>%
  arrange(year) %>%
  mutate(country_type = ifelse(income == "High income: OECD", "Western", "Other"),
         country_type = ifelse(region == "East Asia & Pacific (all income levels)" | 
                                 country %in% c("India", "Bangladesh", "Nepal", "Pakistan"), 
                               "Asian", country_type)) %>%
  group_by(iso2c, country, country_type) %>%
  summarise(gdp1970 = gdp[year == 1970],
            growth = (gdp[year == end_year] / gdp1970) ^ (1 / (end_year - 1970)) - 1,
            pop1970 = pop[year == 1970]) %>%
  filter(!is.na(growth))  %>%
  mutate(plot_label = ifelse(country_type == "Other", 
                                    "Excluding Asian and Western countries", 
                                    "Asian and Western countries"))

gdp_summary$plot_label <- relevel(factor(gdp_summary$plot_label), "Excluding Asian and Western countries")
gdp_summary$country_type <- fct_relevel(factor(gdp_summary$country_type), c("Asian", "Western"))

gdp_summary %>%
  ggplot(aes(x =  gdp1970, y = growth)) +
  facet_wrap(~plot_label) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", linetype = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", aes(weight = pop1970)) +
  geom_point(aes(colour = country_type, size = pop1970 / 10^6), alpha = 0.5) +
  geom_point(aes(size = pop1970 / 10^6), colour = "black", shape = 1) +
  # next line would give labels for points but horribly cluttered plot
  # geom_text_repel(aes(label = iso2c)) +
  scale_x_log10("\nGDP per capita in 1970\n\nDotted line is unweighted regression; solid line is population-weighted regression.\n", 
                label = dollar, breaks = c(1000, 5000, 20000)) +
  scale_y_continuous(paste("Average growth between 1970 and", end_year), label = percent) +
  scale_size_area("Population\nin 1970 (m)", max_size = 10) +
  labs(colour = "Region",
       caption = "Source: World Development Indicators") +
  ggtitle("GDP in 1970 and subsequent growth",
          "Constant prices, US dollars\nInspired by Figure 4.3 from Milanovic, Global Inequality") +
  theme(legend.position = "right")

# Note the pattern is the same as in Milanovich's graphic, even though 
# the growth rates seem different
{% endhighlight %}


## Bonus - forecasting inter-country inequality

An excursus in Milanovic's book refers to some work  by others forecasting trends in global inequality if current trends continue.  I'm only working with inter-country data today, nothing that features the two individual-level global inequality, buit I was sufficiently interested in the idea to knock together my own very crude forecasts. I basically projected current country-level trends in GDP per capita and population growth with straightforward time series methods (a hybrid of Hyndman's `auto.arima` and `ets` methods, as conveniently pulled together in the `forecastHybrid` package by David Shaub with minor help from myself).  The result shows the expected continued decline in weighted inequality as China and India continue to "catch up":

<img src='/img/0105-gini-forecasts.svg' width='100%'>

Here's the code for that forecasting exercise:

{% highlight R %}
#================forecasts of global gini=============
# this is only a taster for what is described in Excursus 4.1 in Milanovic's book
# because I only deal with inter-national inequality, and use very crude forecasts!
# but it comes up with a modestly similar result.  I see a drop in inter-national inequaltiy
# of about 0.07 whereas they find a drop in global inequality of 0.04.  These feel quite
# potentially consistent (because most forecasts for intranational inequaltiy are increases).

# could do the forecasts of Gini by forecasting GDP per capita, and using the population 
# forecasts from https://esa.un.org/unpd/wpp/Download/Standard/Population/
# but.... effort!...
# so let's just do our own forecasts

#' Forecast gdp and pop for one country gdp_constant
#' and combines it with the historical data
#' context-specific function, only works in this session
forecast_gdp_pop <- function(the_country, h = 20){
  # assumes presence in environment of a data.frame called gdp_constant
  country_vars <- gdp_constant %>%
    rename(gdp = NY.GDP.PCAP.KD,
           pop = SP.POP.TOTL) %>%
    arrange(year) %>%
    filter(country == the_country) %>%
    dplyr::select(gdp, pop) %>%
    ts(start = 1960) 

  # a bit of fiddliness is needed because some countries are missing early GDP per person data.
  # maybe they should be knocked out altogether of course... but let's decide that
  # later and explicitly, this function shoudl be able to handle it either way
  years_orig <- time(country_vars)
  rows_include <- which(!is.na(country_vars[ , "gdp"]))
  # some "countries" have virtually no observations, so don't even try to forecast them...
  if(length(rows_include) > 5){ 
    
    country_vars <- country_vars[rows_include, ]
  
    mod_gdp <- hybridModel(country_vars[ , "gdp"], 
                           lambda = 0, models = "ae", verbose = FALSE)
    mod_pop <- hybridModel(country_vars[ , "pop"], 
                           lambda = 0, models = "ae", verbose = FALSE)
    
    fc_gdp <- forecast(mod_gdp, h = h)
    fc_pop <- forecast(mod_pop, h = h)
    
    years <- years_orig[rows_include]
    years <- c(years, max(years + 1):(max(years) + h))
    
    
    tmp <- data.frame(
      country = the_country,
      gdp_pp = c(fc_gdp$x, fc_gdp$mean),
      pop = c(fc_pop$x, fc_pop$mean),
      year = years
    )} else {
      tmp <- NULL
    }
  return(tmp)
}

# test cases.  Is designed to work even with countries with some NA values,
# even though when doing it for real I decided to drop them.
forecast_gdp_pop("Australia", h = 100) %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line() +
  ggtitle("Crude forecast of GDP per person and population for Australia")

forecast_gdp_pop("China", h = 5) %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line() +
  ggtitle("Crude forecast of GDP per person and population for China")

forecast_gdp_pop("Vietnam") %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line()

# vector of all the countries with complete data:
all_countries <- filter(country_coverage, prop_complete == 1)$country

# do the actual forecasts:
all_forecasts <- lapply(all_countries, forecast_gdp_pop)
all_forecasts_df <- do.call("rbind", all_forecasts)

# draw graphic:
all_forecasts_df %>%
  group_by(year) %>%
  summarise(Unweighted = Gini(gdp_pp),
            Weighted = weighted.gini(gdp_pp, pop)$bcwGini) %>%
  gather(variable, value, -year) %>%
  mutate(variable = fct_reorder(variable, -value, fun = last)) %>% 
  mutate(type = ifelse(year <= 2016, "Actual", "Forecast")) %>%
  ggplot(aes(x = year, y = value, colour = variable, linetype = type)) +
  geom_line() +
  theme(legend.position = "right") +
  labs(colour = "", linetype = "", x = "",
       y = "Gini or weighted Gini",
       caption = "Source: World Development Indicators") +
  ggtitle("Inter-country inequality over time, including crude forecasts",
          "GDP per capita, two different methods of comparing across countries
Only includes countries with complete data 1960 to 2013") +
  scale_colour_manual(values = gini_palette)
{% endhighlight %}


