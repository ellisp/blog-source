---
layout: post
title: Demystifying life expectancy calculations
date: 2018-05-31
tag: 
   - Demography
   - R
description: Life expectancy is calculated directly from death rates. And mathematically speaking, changes in infant mortality have a much greater impact on life expectancy than do changes in death rates in any other year.
image: /img/0123-0.svg
socialimage: http://freerangestats.info/img/0123-0.png
category: R
---

## Life expectancy and death rates

A chance comment on Twitter about historical increases in life expectancy (in the USA) with fairly constant death rates got me wondering about the exact calculations of these things so I did some Googling.  Warning on what follows - I am strictly an amateur in demographics, and was deliberately working things out from basics as a learning exercise, so there are possibly better ways to do some of the things below.

According to Wikipedia which is always good at this sort of thing, [life expectancy can be calculated two ways](https://en.wikipedia.org/wiki/Life_expectancy):

- by observing a cohort until they are all dead, and taking the average age.  Obviously this only works with historical data.  This is referred to as "cohort life expectancy at birth".
- by taking a set of death rates by age bracket, and calculating the average age of death of a hypothetical cohort of people who move through life with those death rates. This is called "period life expectancy at birth" and is the method used for reporting by national statistics offices.

By convention, period life expectancy applies *today's* death rates to an infant born today.  Hence, there's no estimation of those death rates changing; we assume that by the time this infant is 60 years old, the death rate of 60 year olds in 2078 will be the same as now.  This has some rather obvious flaws, but it's also easy to see why that approach is convenient, particularly for standardising data across countries.  It means that the reported life expectancies aren't really the best estimates of how long a baby born today will live conditional on standard expectations about civilization continuing at all, because we actually expect death rates to continue to decline (although some days I wonder).  But it does mean that "life expectancy" is very clearly defined and with minimum discretion needed in the calculation.  So long as we remember that period life expectancy is really a summary statistic of today's death rates by age, not really how long people are expected to live but a sort of hypothetical life length, we are ok.

Life expectancy can go up while crude death rates are also going up (or vice versa) because of changing age composition in a population.  All the age-specific death rates (and hence any age-adjusted death rate) might be going down, but if more and more people are in the higher-rate age brackets the overall crude death rate might be increasing.  We can see this happening for Japan in the image below:

<img src='/img/0123-life-death-csp.svg' width='100%'>

Here's the R code that grabbed the data for that image and built it.  ggplot2 afficianados might be interested in how I've used `geom_blank` to increase the scales a bit beyond the defaults, which were too narrow and had my text banging against the axes (a common problem).  It's a bit of a hack but it works nicely:

{% highlight R lineanchors %}
library(frs) # if necessary, install with devtools::install_github("ellisp/frs-r-package/pkg")
library(tidyverse)
library(scales)
library(WDI)

# Use WDIsearch to find the indicators we want
# View(WDIsearch("life"))

death_rate <- WDI(indicator = "SP.DYN.CDRT.IN", start = 1960, end = 2100)
life_exp <- WDI(indicator = "SP.DYN.LE00.IN", start = 1960, end = 2100)

selected_countries <- c("United Arab Emirates", "Suriname", "Georgia", "Barbados", "Kiribati", 
                        "Moldova", "Ghana", "Japan", "New Zealand")

# connected scatter plot:
death_rate %>%
  filter(country %in% selected_countries) %>%
  inner_join(life_exp, by = c("iso2c", "country", "year")) %>%
  # take out NAs which confuse the geom_text call later:
  filter(!is.na(SP.DYN.LE00.IN)) %>%
  # order country by life expectancy so graphic facets make some sense:
  mutate(country = fct_reorder(country, SP.DYN.LE00.IN)) %>%
  ggplot(aes(x = SP.DYN.CDRT.IN, y = SP.DYN.LE00.IN, colour = country)) +
  geom_path() +
  # add labels of the year at max and min points
  geom_text(aes(label = ifelse(year %in% range(year), year, "")), colour = "grey30", size = 3) +
  facet_wrap(~country, scales = "free") +
  # these next two geom_blank calls are a hack to make sure enough space is given for the labels which
  # otherwise crash into the axes, and can't be easily controlled with scale limits:
  geom_blank(aes(x = SP.DYN.CDRT.IN * 1.1, y = SP.DYN.LE00.IN * 1.05)) +
  geom_blank(aes(x = SP.DYN.CDRT.IN * 0.9, y = SP.DYN.LE00.IN * 0.95)) +
  theme(legend.position = "none") +
  labs(x = "Crude death rate (per 1,000 people)",
       y = "Life Expectancy",
       caption = "Source: World Bank World Development Indicators") +
  ggtitle("Differing relationship of crude death rate and life expectancy",
          "Countries with aging populations (eg Georgia, Japan, in the below) can experience increases in both simultaneously.")
{% endhighlight %}

## Calculating life expectancy

To be sure I understood how it worked, I had a go at estimating some life expectancies myself.  I started with some [French death rates per age group](https://www.ined.fr/en/everything_about_population/data/france/deaths-causes-mortality/mortality-rates-sex-age/) from 2015, because these were the most convenient to hand.  Death rates are reported as deaths per 1,000 people per year.  Here's how they look when you convert them into the probability of surviving the year at any particular age:

<img src='/img/0123-french-rates.svg' width='100%'>

Here's how that was drawn.  The `french_death_rates_2015` object [comes from the `frs` package](https://github.com/ellisp/frs-r-package/blob/master/pkg/R/death_rates.R) where I store miscellaneous things to help out with this blog.

{% highlight R lineanchors %}
french_death_rates_2015 %>%
  gather(sex, value, -age) %>%
  ggplot(aes(x = age, y = (1000 - value) / 1000, colour = sex)) +
  geom_line() +
  labs(x = "Age", y = "Probability of surviving to the next year", colour = "",
       caption = "https://www.ined.fr/en/everything_about_population/data/france/deaths-causes-mortality/mortality-rates-sex-age/") +
  coord_cartesian(ylim = c(0.8, 1), xlim = c(0, 100)) +
  ggtitle("French death rates in 2015") +
  theme(legend.position = c(0.6, 0.6))
{% endhighlight %}

To actually convert these probabilities into a life expectancy, we need to estimate the proportion of our hypothetical population that will die at each age.  There's lots of different ways you might do this but the one that came to mind to me was:

- Create interpolated death rates for each integer age (because typically death rates are given for a bracket of ages, not every single age)
- Estimate the proportion still alive at any age, starting with 1 at birth and 0 at some arbitrary end point (I chose 150 which seems reasonable).  This is the cumulative product of the yearly survival rates, which are of course 1 - death rate (where death rate has been converted to a probability rather than a factor out of 1,000).
- Estimate the difference between those proportions for each year, which gives you the proportion of the total population that died in that year.
- Take the average of all of our ages, weighted by the proportion of the population that died each year.

Now that I write it up this seems a bit more involved than I would have thought.  It's possible there's a simpler way.  Anyway, here's the function that implements my approach

{% highlight R lineanchors %}
#' @param age vector of ages in years
#' @param rate death rate in deaths per 1000 people alive at the given age.  Must be the same length as age.
#' @param data optionally, a data frame or matrix where the first column is used as age and the second column as rate
#' @examples
#' life_expectancy(data = french_death_rates_2015[ , c("age", "female")])
#' life_expectancy(age = french_death_rates_2015$age, rate = french_death_rates_2015$male)
life_expectancy <- function(age = data[, 1], rate = data[ , 2], data = NULL){
  if(length(age) != length(rate) | 
     !class(age) %in% c("numeric", "integer") | 
     !class(rate) %in% c("numeric", "integer")){
    stop("age and rate should be integer or numeric vectors of the same length")
  }
  if(max(rate) != 1000) {
    stop("The highest value of rate should be exactly 1000, indicating the age at which everyone is guaranteed to die at or before.")
  }
  # interpolated version so we have a death rate for every age:
  dr <- stats::approx(age, rate, xout = 0:max(age))
  
  prop_alive <- c(1, cumprod((1000 - dr$y) / 1000))
  deaths <- -diff(prop_alive)
  return(sum(deaths * 0:max(age)))
}
{% endhighlight %}

With my original data it gives plausible results: 85.3 for females and 79.1 for males.  These differ minutely from the [published figures](http://www.worldlifeexpectancy.com/france-life-expectancy) for France in 2015 of 85.4 and 79.4; I imagine the difference is in how the age brackets are treated.  I made some fairly cavalier simplifications in using the middle year of the age bracket as a reference point and interpolating between those points, which on reflection will cause some small problems when there are rapid changes in mortality (the most likely being from the age 0-1 bracket to the age 2-5 bracket).

## Impact on life expectancy of changing a single age groups death rate

It's interesting to play with how changing death rates at a particular part of the life cycle change the life expectancy calculation.  Infant mortality is the biggest driver.  The intuition behind this is that everyone who is born alive gets a chance to survive the first year, so an improvement here impacts on the biggest part of our population.  If you improve the odds of surviving from 110 to 111 it has minimal impact on life expectancy because most people are already dead at that point.

So here's what happens if we make infant mortality (ie death per thousand in the first year of life) arbitrarily small or large, using French males in 2015 as a reference point.  The blue dot represents the current actual death rate in first year of life and overall life expectancy; the rest of the line shows what happens for hypothetical different death rates (which for France with its low infant mortality, historically and internationally speaking, means higher ones):

<img src='/img/0123-0.svg' width='100%'>

Obviously, if we say 1,000 out of 1,000 people die in the first year, our life expectancy becomes zero.  More realistically, if death rates in the first year went up to 250 out 1,000 (which would be around the worst current day level, but well within historical ranges), life expectancy comes down from 79 to around 50, *despite* death rates at all other ages staying the same as in 2015 France.

On the other hand, what if we make a spike in deaths at age 18, perhaps due to a strange disease or social custom that makes this a uniquely hazardous age (the the hazard going down to normal levels at age 19).  Even if the entire population dies at this age, the life expectancy is still 17 or so; and improvements in mortality rates for 18-year olds accordingly have less relative impact on life expectancy than was the case when we "improved" infant mortality:


<img src='/img/0123-17.5.svg' width='100%'>

For the older population, the issue is more marked again:

<img src='/img/0123-85.svg' width='100%'>

Finally, consider the case where a medical advance guarantees a uniform yearly survival rate for anyone who reaches 85 until they turn 150:

<img src='/img/0123-85-over.svg' width='100%'>

Even if we make that survival rate 0 (ie all 85 year olds are guaranteed to reach 150), life expectancy only gets up to about 91.

The code for those simulations is below.  It's a bit repetitive, but with the fiddles I wanted to labels and so on and with limited future re-use expected, it didn't seem worth writing a function to do this job.

{% highlight R lineanchors %}
m <- french_death_rates_2015[ , c("age", "male")]
the_age <- which(m$age == 0) # row in the data frame for this age of interest
current_level <- life_expectancy(data = m)
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[1, 2] <- i  # m is modified only within the environment of this function
    life_expectancy(data = m) 
})
) %>% 
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different infant mortality rates on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths in first year of life per thousand live births",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))

the_age <- which(m$age == "17.5")
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[the_age, 2] <- i
    m <- rbind(m, data.frame(age = c(16.5, 18.5), male = c(0.3, 0.3)))
    life_expectancy(data = m)  
  })
) %>%
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different death rates in early adulthood on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths at age 17.5 per thousand people living to 16.5",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))

the_age <- which(m$age == "85")
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[the_age, 2] <- i
    m <- rbind(m, data.frame(age = c(84, 86), male = c(78, 78)))
    life_expectancy(data = m)  
  })
) %>%
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different death rates in old age on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths at age 85 per thousand people living to 84",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))

the_age <- which(m$age == "85")
mr <- nrow(m) - 1
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[the_age:mr, 2] <- i
    life_expectancy(data = m)  
  })
) %>% 
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different death rates in old age on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths at age 85 and all older ages until 150 (when all die), per thousand people living to 84",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))
{% endhighlight %}

Hmm, ok, interesting.  I have some more thoughts about the arithmetic of demography, but they can come in a subsequent post.