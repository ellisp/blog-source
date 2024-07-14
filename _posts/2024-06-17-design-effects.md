---
layout: post
title: Design effects for stratified sub-populations
date: 2024-06-17
tag: 
   - Surveys
   - WorkRelated
   - Tools
description: I look at the two different sorts of design effects that Stata will report for estimates from sub-populations of a complex survey, which vary depending on whether or not the hypothetical simple random sample we are comparing the complex survey to has the same sub-population sample sizes as the actual sample.
image: /img/0262-vanuatu-pyramid.svg
socialimage: https:/freerangestats.info/img/0262-vanuatu-pyramid.png
category: R
---

I set out to see if it was possible to reproduce the UN's [2022 Revision of World Population Prospects](https://population.un.org/wpp/) for a given country by cohort component projection from the fertility, mortality and immigration rates and population starting point published as part of their projection. The motivation is to make small changes to some of those parameters - for example by substituting in a recent census result for the population and a given year - and "re-run" the projections to see the impact of changes, or to get a more up-to-date version with data that wasn't available to the UN at the time of their projection.

It turns out this wasn't too hard (one morning's work for the modelling, then a few hours of write-up), particularly in cases where migration is small in the projection period. I was able to reproduce almost exactly population, birth and death totals to 2100 for Vanuatu, and demonstrate the impact of updating the 2020 year for their recent census population totals and fertility rates, and getting a slightly lower projection as a result.

Here's my reproduction of Vanuatu's population projection from 2020 to 2100, using just the 2020 population totals and the forecast fertility, mortality and migration rates. As you can see it's basically identical to the UN totals:

<object type="image/svg+xml" data='/img/0262-vanuatu-pop.svg' width='100%'><img src='/img/0262-vanuatu-pop.png' width='100%'></object>

And here's the same method tweaked for the actual 2020 census total and with a rough adjustment made to fertility rates based on what was observed at the 2020 census:

<object type="image/svg+xml" data='/img/0262-vanuatu-revised.svg' width='100%'><img src='/img/0262-vanuatu-revised.png' width='100%'></object>

Of course, this method delivers a full set of projections by age and sex, and we could construct life tables or any indicators we want from it. Here are population pyramids comparing the published UN projections for 2050 with my revised set. Not visually stunning in its comparison, but enough to prove that it's possible:

<object type="image/svg+xml" data='/img/0262-vanuatu-pyramid.svg' width='100%'><img src='/img/0262-vanuatu-pyramid.png' width='100%'></object>

## Reproducing UN projections

So here's how I went about that. 

First, downloading all the data. The UN recommend bulk downloads of their CSV files. For my purposes I first need the fertility, mortality and population by sex and one year age groups. Of the population original data I am only going to use the 2020 year, and then project it forward myself based on fertility, mortality and migration; but I want the full set for comparison purposes. For migration, I couldn't see in my hasty look at the UN site a dataset of migration projections by age and sex, so I just use the much simpler net migration rate (per thousand people) per year in the projection period. Here's code to download all this UN data:

{% highlight R lineanchors %}
library(tidyverse)
library(glue)
library(scales)
library(patchwork)

dir.create("data-pop-proj-2022", showWarnings = FALSE)

#------------------download and import data for all countries from existing projections----------------
list.files("data-pop-proj-2022")

files <- c("WPP2022_Fertility_by_Age1.zip",
           "WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.zip",
           "WPP2022_DeathsBySingleAgeSex_Medium_2022-2100.zip",
           "WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip",
           "WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip",
           "WPP2022_Demographic_Indicators_Medium.zip"
           )

# let downloads take up to 10 minutes rather than 1 minute max, as files are
# large (largest is fertility for single age, 78MB)
options(timeout=600)

if(!file.exists("data-pop-proj-2022/WPP2022_Demographic_Indicators_Medium.csv")){
  # download the zip files:
  for(i in 1:length(files)){
    download.file(glue("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/{files[i]}"),
                   destfile = glue("data-pop-proj-2022/{files[i]}"), mode = "wb")
  }
  
  # unzip them
  for(i in 1:length(files)){
    unzip(glue("data-pop-proj-2022/{files[i]}"), exdir = "data-pop-proj-2022")
  }
}

fert_all    <- read_csv("data-pop-proj-2022/WPP2022_Fertility_by_Age1.csv")
mort_past   <- read_csv("data-pop-proj-2022/WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.csv")
mort_future <- read_csv("data-pop-proj-2022/WPP2022_DeathsBySingleAgeSex_Medium_2022-2100.csv")
pop_past    <- read_csv("data-pop-proj-2022/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv")
pop_future  <- read_csv("data-pop-proj-2022/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv")
indicators  <- read_csv("data-pop-proj-2022/WPP2022_Demographic_Indicators_Medium.csv")

mort_all <- rbind(mort_past, mort_future)
pop_all <- rbind(pop_past, pop_future)

# clean up
rm(mort_past, mort_future, pop_past, pop_future)

{% endhighlight %}

Next, I wrote my own cohort component population projection function. I wanted to do this from scratch rather than using an existing demography package to make sure I understood what was happening (I'm not a demographer) and could make tweaks as necessary to match the UN approach. I used [this tutorial](https://farid-flici.github.io/tuto.html) by Farid Flici as my starting point; abstracted his code into a function for easy use with multiple countries, and added a net migration component. 

Because migrants' ages tend to be dissimilar from the country they are migrating too - they are more likely to be in the prime of their working / family life I believe - I needed a way to set the ages of migrants. In the function below I defaulted to a normal distribution of ages, mean 28 and standard deviation 11, which worked well to get similar results to the UN in a few countries I tried. This was the hardest and most discretionary part of the exercise.

My observation is that demographers seem to think in terms of matrices of numbers rather than database-oriented tidy data, and I have kept that matrix approach in this function.

{% highlight R lineanchors %}
#----------------------projections for one country-------------------

# adapting the implementation of the method for cohort component projection at 
# https://farid-flici.github.io/tuto.html


#' @param start_pop_m is a vector of population of males at single year age
#'   periods beginning of year starting age 0
#' @param start_pop_f as start_pop_m but for females
#' @param start_year first year ie the year for which the actual population
#'   number refers to
#' @param end_year end year for the population projection
#' @param fertility a matrix with rows for female agegroups 15 to 49 and a
#'   column for each year, values are births per woman (not per thousand women)
#'   that year and age. Must have 35 rows and number of columns equal to
#'   end_year minus start_year plus one.
#' @param mort_m a matrix with rows for age 0 to at least 50 and columns for
#'   each year, for males. Must have number of columns equal to end_year minus
#'   start_year plus one.
#' @param mort_f as mort_m but for females. Must have same number of rows and
#'   columns as mort_m.
#' @param sex_ratio_birth number of boys born for every girl born, vector with
#'   length of years to be projected.
#' @param net_migration vector of proportions of the population that migrate to
#'   the country, net of those who leave. Defaults to a vector of zeroes.
#' @param female_share_migration vector of proportions of net migrants that are
#'   female. Defaults to a vector of 0.5s.
#' @param migration_age_weights vector of relative distribution of the age of
#'   net migrants. Must be 121 values representing relative proportion of net
#'   migrants aged zero to 120. Defaults to a normal distribution of mean 28 and
#'   standard deviation 11. Note that this distribution currently must be the
#'   same through the projection period (unlike eg fertility and mortality,
#'   which have individual values for each age group and sex for each year).
#'   Only the total net migration proportion can change over time, not its age
#'   make-up.
#' @author Peter Ellis, expanding on a less-featured example by Farid Flici
#'   https://farid-flici.github.io/tuto.html
pop_proj <- function(start_pop_m, 
                     start_pop_f, 
                     start_year, 
                     end_year, 
                     fertility, 
                     mort_m,
                     mort_f,
                     sex_ratio_birth = rep(1.045, end_year - start_year + 1),
                     net_migration = rep(0, end_year - start_year + 1),
                     female_share_migration = rep(0.5, end_year - start_year + 1),
                     migration_age_weights = dnorm(0:120, mean = 28, sd = 11)
                     ){
  
  stopifnot(nrow(fertility) == 35)
  stopifnot(length(migration_age_weights) == 121)
  
  ncols <- end_year - start_year + 1
  
  stopifnot(ncol(fertility) == ncols)
  stopifnot(ncol(mort_m) == ncols)
  stopifnot(ncol(mort_f) == ncols)
  stopifnot(length(start_pop_m) > 50)
  stopifnot(length(start_pop_m) < 120)
  stopifnot(length(start_pop_f) > 50)
  stopifnot(length(start_pop_f) < 120)
  stopifnot(length(net_migration) == ncols)
  stopifnot(length(sex_ratio_birth) == ncols)
  stopifnot(length(female_share_migration) == ncols)
  stopifnot(min(female_share_migration) >= 0)
  stopifnot(max(female_share_migration) <= 1)
  
  stopifnot(nrow(mort_m) == nrow(mort_f))
  
  # fill in with high probability of death for ages beyond where we have mortality data
  if(nrow(mort_m) < 121){
    extra_deaths_m <- matrix(max(mort_m), nrow = 121 - nrow(mort_m), ncol = ncol(mort_m))
    extra_deaths_f <- matrix(max(mort_f), nrow = 121 - nrow(mort_f), ncol = ncol(mort_f))
    
    mort_m <- rbind(mort_m, extra_deaths_m)
    mort_f <- rbind(mort_f, extra_deaths_f)
  }
  
  # Create matrices for male and female population for the full projections
  # each row of the matrix is an age group, each column is a year
  PopM <- matrix(0, nrow=121, ncol = ncols)
  PopF <- matrix(0, nrow=121, ncol = ncols)
  deaths <- rep(NA, ncols)
  
  rownames(PopF) <- rownames(PopM) <- c(0:120)                # ages
  colnames(PopF) <- colnames(PopM) <- c(start_year:end_year)  # years
  
  # first year gets populated with the actual population numbers that we have:
  PopM[1:length(start_pop_m), 1] <- start_pop_m
  PopF[1:length(start_pop_f), 1] <- start_pop_f
  
  # subsequent years get modified by people aging one year, deaths, births, and net migration
  for (i in 2 : ncols) {
    
    # Age one and above:
    PopM[2:121, i] <- PopM[1:120, i-1] * (1 - mort_m[1:120, i-1])
    PopF[2:121, i] <- PopF[1:120, i-1] * (1 - mort_f[1:120, i-1])
    
    deaths[i-1] <- sum(PopM[, i-1]) - sum(PopM[, i]) + sum(PopF[, i-1]) - sum(PopF[, i])
    
    
    # migration. 
    total_migrants <- (sum(PopM[,i]) + sum(PopF[,i])) * net_migration[i]
    migf = total_migrants * female_share_migration[i] * migration_age_weights / sum(migration_age_weights)
    migm = total_migrants * (1 - female_share_migration[i]) * migration_age_weights / sum(migration_age_weights)
    
    PopM[, i] <- PopM[, i] + migm
    PopF[, i] <- PopF[, i] + migf
    
    
    # Age zero ie births. Fertility rate by the number of women in the middle of the previous year.
    # Note that PopF rows 16:50 equates to women age 15:49
    reproducing_women <- (PopF[16:50, i-1] + PopF[16:50, i]) / 2
    
    prop_boys <- sex_ratio_birth[i] / (1 + sex_ratio_birth[i])          
    prop_girls <- 1 - prop_boys
    
    PopM[1, i] <-  reproducing_women %*% fertility[, i-1] * prop_boys
    
    PopF[1, i] <- reproducing_women %*% fertility[, i-1] * prop_girls 

  }
  
  # Return a list of the two matrices
  return(list(
    PopM = PopM,
    PopF = PopF,
    deaths = deaths
  ))
}
{% endhighlight %}

Next, I wrote a function to extract the necessary fertility, mortality, migration rates and 2020 starting population from the UN data, feed it into my `pop_proj()` function and return the result. Unlike `pop_proj()`, which is to some degree fully portable, this function is very much specific to this particular project and is really just a convenience function for grabbing the data and turning it into the right units and shapes (vectors and matrices) needed for `pop_proj()`.

{% highlight R lineanchors %}
repeat_un_proj <- function(the_country, the_years = 2020:2100){
  
  if(!the_country %in% unique(indicators$Location)){
    stop("Country not found")
  }
  
  this_fert <- fert_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time %in% the_years) |>
    # Age-Specific Fertility rate
    select(Time, AgeGrp, ASFR) |>
    mutate(Time = as.character(Time),
           # turn into proportions, not rates per 1000:
           ASFR = ASFR / 1000) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = ASFR) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  # should be 35 rows ie fertilities for ages 15 to 49
  stopifnot(nrow(this_fert) == 35)
  # should be 81 columns, 1 column for each year from 2020 to 2100
  stopifnot(ncol(this_fert) == length(the_years))
  
  
  # Mortality is in numbers not a ratio so we need to join to the population data to turn it into a ratio
  pop_years <- pop_all |>
    filter(Location == the_country & Variant == "Medium" & Time %in% the_years) |>
    select(Time, PopMale, PopFemale, AgeGrp)

  this_mort <- mort_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time %in% the_years) |>
    left_join(pop_years, by = c("Time", "AgeGrp")) |>
    # next step important because we will be sorting by AgeGrp
    mutate(AgeGrp = case_when(
      AgeGrp == "100+" ~ 100,
      TRUE ~ suppressWarnings(as.numeric(as.character(AgeGrp)))
    ))
  
    
    
  this_mort_m <- this_mort |>
    # sometimes more deaths than people (eg 1 death, 0 people) so cap the death ratio at 1
    mutate(DeathMale = pmin(1, DeathMale / PopMale)) |>
    select(Time, AgeGrp, DeathMale) |>
    mutate(Time = as.character(Time)) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathMale) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  this_mort_f <- this_mort |>
    mutate(DeathFemale = pmin(1, DeathFemale / PopFemale)) |>
    select(Time, AgeGrp, DeathFemale) |>
    mutate(Time = as.character(Time)) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathFemale) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  # check the years are correct, didn't get mangled or reordered
  stopifnot(all(colnames(this_mort_f) == the_years))
  stopifnot(all(colnames(this_mort_m) == the_years))
  
  this_pop <- pop_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time == min(the_years)) |>
    # next step important because we will be sorting by AgeGrp
    mutate(AgeGrp = case_when(
      AgeGrp == "100+" ~ 100,
      TRUE ~ suppressWarnings(as.numeric(as.character(AgeGrp)))
    )) |>
    arrange(AgeGrp) 
  
  # convert to units, not thousands of people:
  this_pop_m <- this_pop$PopMale * 1000
  this_pop_f <- this_pop$PopFemale * 1000
  
  # reality check
  # Population in millions; should be about 0.3 if the_country is Vanuatu, about 1400 if India:
  (sum(this_pop_m) + sum(this_pop_f) ) / 1e6
  
  # net migration
  this_cnmr <- indicators |>
    filter(Location == the_country & Time %in% the_years) |>
    arrange(Time) |>
    pull(CNMR) / 1000
  
  # sex ratio at birth
  this_srb <- indicators |>
    filter(Location == the_country & Time %in% the_years) |>
    arrange(Time) |>
    pull(SRB) / 100
  
  this_proj <- pop_proj(
                  start_pop_m = this_pop_m, 
                  start_pop_f = this_pop_f, 
                  start_year = min(the_years), 
                  end_year = max(the_years), 
                  fertility = this_fert, 
                  mort_m = this_mort_m,
                  mort_f = this_mort_f,
                  net_migration = this_cnmr,
                  sex_ratio_birth = this_srb
    
  )
  return(list(un_proj = this_proj,
         un_pop_m = this_pop_m,
         un_pop_f = this_pop_f,
         un_fert = this_fert,
         un_mort_m = this_mort_m,
         un_mort_f = this_mort_f,
         un_cnmr = this_cnmr,
         un_srb = this_srb))
}
{% endhighlight %}

Note that this function returns, in addition to the results of the population projection, the various inputs in their correct units and shape. This will be useful later when we want to modify some of those inputs.

Now that we've got these functions, using them to do projections from 2020 and compare those projections to the published numbers is pretty straight forward. Here's the code to do that for Vanuatu, which produces the first chart at the top of this blog post:

{% highlight R lineanchors %}
the_country <- "Vanuatu"
my_proj <- repeat_un_proj(the_country)$un_proj

# total population
comp_data <- indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `UN original` = TPopulation1Jan) |>
  mutate(`Reproduction` = as.numeric(apply(my_proj$PopM, 2, sum) + apply(my_proj$PopF, 2, sum)) / 1000) 

# First year should be an exact match:
stopifnot(comp_data[1, ]$`UN original` == comp_data[1, ]$Reproduction)


comp_data |>
  gather(variable, value, -Time) |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Population", x = "", colour = "")
{% endhighlight %}

<object type="image/svg+xml" data='/img/0262-vanuatu-pop.svg' width='100%'><img src='/img/0262-vanuatu-pop.png' width='100%'></object>

As we can see the results are pretty much identical. Let's look at my projected births and deaths based on fertility and mortality rates, and compare them to the published projected numbers


{% highlight R lineanchors %}
# births
indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `UN original` = Births) |>
  mutate(Reproduction = as.numeric(my_proj$PopM[1,] + my_proj$PopF[1, ]) / 1000) |>
  gather(variable, value , -Time)  |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Births (thousands)", x = "", colour = "")

# deaths
indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `UN original` = Deaths) |>
  mutate(Reproduction = as.numeric(my_proj$deaths) / 1000) |>
  gather(variable, value , -Time)  |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Deaths (thousands)", x = "", colour = "")
{% endhighlight %}

<object type="image/svg+xml" data='/img/0262-vanuatu-births.svg' width='100%'><img src='/img/0262-vanuatu-births.png' width='100%'></object>
<object type="image/svg+xml" data='/img/0262-vanuatu-deaths.svg' width='100%'><img src='/img/0262-vanuatu-deaths.png' width='100%'></object>

I'm pretty happy with those. There's definitely some discrepancies and a lag in the births which I suspect come down to how one treats populations of mothers - numbers on 1 January v 1 July, that sort of thing. But in the scheme of things these are very small.

Now, Vanuatu is relatively easy because the UN assumed net zero migration in the projection period. We can see this by comparing the net migration rates in their Indicators dataset for a few countries, with this code:

{% highlight R lineanchors %}
plot_mig <- function(the_country){
  p <- indicators |>
    filter(Location == the_country) |>
    ggplot(aes(x = Time, y = CNMR)) +
    geom_vline(xintercept = 2022, lty = 2, colour = "steelblue") +
    geom_hline(yintercept = 0, lty = 2, colour = "steelblue") +
    geom_line() +
    labs(title = the_country,
         subtitle = "Net migration rate per thousand people",
         y = "", x = "",
         caption = "Source: UN World Population Prospects 2022")
  
  return(p)
}

plot_mig("Vanuatu") + plot_mig("Fiji") + 
  plot_mig("Australia") + plot_mig("India") +
  plot_layout(ncol = 2)
{% endhighlight %}

<object type="image/svg+xml" data='/img/0262-migration.svg' width='100%'><img src='/img/0262-migration.png' width='100%'></object>

For countries that have good data on it, migration is a big deal in the projections; but forecasting is hard, particularly of the future.

My first few goes at reproducing the projections for Australia and India tended to be badly out because I had added in net migration evenly across the whole age distribution. My eventual solution, making net migration bell curved with an average age of 28, is a bit of a hack with the parameters chosen to make Australia's projections come out right. Definitely a better method would be to have actual age-specific net migration forecasts. Whether such things are possible will very much depend on the country; it's probably possible for Australia, but not for most of the countries I work with.

Here's the final result comparing UN projections with mine for a few interesting countries:

<object type="image/svg+xml" data='/img/0262-Australia-pop.svg' width='100%'><img src='/img/0262-Australia-pop.png' width='100%'></object>
<object type="image/svg+xml" data='/img/0262-China-pop.svg' width='100%'><img src='/img/0262-China-pop.png' width='100%'></object>
<object type="image/svg+xml" data='/img/0262-India-pop.svg' width='100%'><img src='/img/0262-India-pop.png' width='100%'></object>

## Adjusting the starting point

Now, the whole point of this exercise was to see if we can plausibly adjust the starting point - say the population totals in 2020, or the forecast fertility rates - and say we are building on the UN's projections to get our own.  Here's my rough demo of how we might do that, again using the case of Vanuatu. Vanuatu's 2020 census wasn't available at the time of the UN's 2022 population projections, so the actual population and fertility numbers for 2020 differ somewhat (of course) from what was projected. 

For the below, I am relying on [the analytical report of the Vanuatu census](https://sdd.spc.int/digital_library/vanuatu-2020-national-population-and-housing-census-analytical-report-volume-2). On a quick glance I didn't see a table of the actual total population by single year age and sex, so I just adjusted the UN's projected totals by a factor to make them add up to the correct 2020 total males and females. Of course if we were doing this for real we'd get the real numbers straight from the census; I actually can do this easily enough at work but obviously for this blog wanted to use only easily accessible public data.

{% highlight R lineanchors %}
#-----------------changing one or two factors while keeping the rest the same--------------

# Say we had the 2020 Vanuatu census so we knew the real population then, and wanted
# to redo the population projections with everything staying as they were before


# file:///C:/Users/Peter/Downloads/Vanuatu_2020_Census_Analytical_report_Vol_2.pdf
revision_country <- "Vanuatu"
van_orig <- repeat_un_proj(revision_country)

# rough adjustment of starting population in 2020 to make totals match the Census totals.
# In principle could of course use the actual numbers by single age category.
revised_pop_m <- van_orig$un_pop_m * 151597 / sum(van_orig$un_pop_m)
revised_pop_f <- van_orig$un_pop_f * 148422 / sum(van_orig$un_pop_f)

# calculate the crude birth rate in the current projection
un_cbr_2020 <- (van_orig$un_proj$PopM[1, 2]  + van_orig$un_proj$PopF[1, 2]) / 
  sum(van_orig$un_proj$PopM[, 2], van_orig$un_proj$PopF[, 2]) * 1000

# make a rough adjustment of future fertility rates assuming they are "out" by the
# same proportion that crude birth rate was out in 2020
adj_ratio <- 28.2 / un_cbr_2020
revised_fert <- van_orig$un_fert * adj_ratio

# Refit the projection with the above rough adjustments:
revised_proj <- pop_proj(
  start_pop_m = revised_pop_m,
  start_pop_f = revised_pop_f,
  start_year = 2020,
  end_year = 2100,
  fertility = revised_fert,
  mort_m = van_orig$un_mort_m,
  mort_f = van_orig$un_mort_f,
  sex_ratio_birth = van_orig$un_srb,
  net_migration = van_orig$un_cnmr)


projected_pop <- apply(revised_proj$PopM, 2, sum) + apply(revised_proj$PopF, 2, sum)

#-----------------Compare total population----------------
comp_data <- indicators |>
  filter(Location == revision_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `2022 UN projections` = TPopulation1Jan) |>
  mutate(`Revised with 2020 census` = as.numeric(projected_pop / 1000) )


p5 <- comp_data |>
  filter(Time <= 2050) |>
  gather(variable, value, -Time) |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = revision_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Population", x = "", colour = "")

#--------------Compare population pyramids-------------------------
d <- tibble(value = c( revised_proj$PopM[, 31], revised_proj$PopF[, 31],   
                  van_orig$un_proj$PopM[, 31],  van_orig$un_proj$PopF[, 31]),
       sex = rep(c("Male", "Female", "Male", "Female"), each = 121),
       model = rep(c("Revised projections with 2020 census, made in 2024", "UN projections, made in 2022"), each = 242),
       age = rep(0:120, 4)) |>
  mutate(agef = fct_reorder(as.character(age), age),
         model = fct_rev(model)) |>
  filter(age < 100)

d |>
  filter(sex == "Female") |>
  ggplot(aes(x = value, y = agef)) +
  facet_wrap(~model) +
  geom_col(fill = "brown", colour = NA) +
  geom_col(data = filter(d, sex == "Male"), aes(x = -value), fill = "orange", colour = NA) +
  geom_vline(xintercept = 0, colour = "white") +
  labs(x = "Number of people", y = "",
       title = "Comparison of UN original and revised population projections",
       subtitle = "Population age distribution in 2050") +
  scale_x_continuous(breaks = c(-4000, -2000, 0, 2000, 4000), labels = c("4,000", "Male", 0, "Female", "4,000")) +
  theme(panel.grid = element_blank()) +
  scale_y_discrete(breaks = 1:20 * 5)
{% endhighlight %}

And that's what gets us these results:

<object type="image/svg+xml" data='/img/0262-vanuatu-revised.svg' width='100%'><img src='/img/0262-vanuatu-revised.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0262-vanuatu-pyramid.svg' width='100%'><img src='/img/0262-vanuatu-pyramid.png' width='100%'></object>

I'm hoping this might be actually useful for pragmatic updates of the UN population projections when more current data is available, without having to revise everything from scratch.

OK, that's all for today.
