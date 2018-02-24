---
layout: post
title: Family violence and economic deprivation in New Zealand
date: 2017-07-02
tag: 
   - NewZealand
   - Crime
   - R
description: I look at the interaction between deprivation, being Māori, and family violence - combining data from the New Zealand census, the New Zealand index of deprivation, and the Family Violence Death Review Committee.
image: /img/0103-interaction.svg
socialimage: http://ellisp.github.io/img/0103-interaction.png
category: R
---

New Zealand's Family Violence Death Review Committee recently released their [Fifth Report Data: January 2009 to December 2015](https://www.hqsc.govt.nz/assets/FVDRC/Publications/FVDRC_2017_10_final_web.pdf), some grim reading on what it describes rightly as "the devastation of family violence".

I was interested by Figure 3 on page 21, which shows a bar chart of the number of offenders by two-classification ethnicity and the socio-economic deprivation of their area of residence.  It showed only raw counts of offenders without reference to the size of the sub-populations in question.  This potentially either hides or magnifies any interaction effect between these two factors.  So I created my own version:

<img src='/img/0103-interaction.svg' width='100%'>

We can see clearly that proportionately speaking, there is a much higher probability of someone being an offender in a family violence death in higher deprivation areas.  The relationship with area deprivation is much stronger for Māori people.  Māori living in the most deprived quintile are around four times as likely to become such offenders than Māori in the middle quintile; for non-Māori the ratio is a bit less than two.  Māori and non-Māori living in the 60% least deprived areas (ie quintiles 1, 2 and 3) have similar (low) probabilities of becoming an offender.

I like my chart because it highlights not just the importance for understanding family violence of the deprivation of the area of residence and of being Māori, but the critical inflating effect of the *combination* of the two.

A quote from the foreword to the report makes a related point:

> "The over-representation of Māori among family violence deceased and offenders, as well as those
from the most deprived neighbourhoods, illustrates that family violence is not distributed equally. For
Māori, multiple intersecting disadvantages, both contemporary and historical, continue to contribute
to the violence within whānau seen today. For those experiencing family violence and living in the
most deprived neighbourhoods, including many Māori whānau, the health sector’s continued focus on
improving equity in the quality and safety of services for all populations is paramount."

*Professor Alan Merry, Chair, Health Quality & Safety Commission, June 2017*

Pulling together this chart was surprisingly fiddly and I had to draw on three different data sources:

- the offender numbers in the Committee's report (which I couldn't find as a table, so had to read off the bar chart)
- The [New Zealand index of deprivation](http://cphronline.massey.ac.nz/maps/maps_nz_dep_index.html)
- Stats NZ's [2013 Census meshblock dataset](http://www.stats.govt.nz/Census/2013-census/data-tables/meshblock-dataset.aspx), which I've previously made available in pre-processed form in my [nzcensus R package](https://github.com/ellisp/nzelect).

## Combining data

Here's R code that gets going with joining the New Zealand index of deprivation to Stats NZ's census data as pre-packaged in `nzcensus` (by me, not by Stats NZ - use at your own risk).  Only 1,867 area units are listed in the deprivation data, dropping 145 areas that are in the census data.

An apology in advance - some of the macrons in "Māori" are missing in what follows.  Some upgrade somewhere has changed the handling of unicode characters on my machine and I haven't had time to fix it before releasing this.

{% highlight R %}
# Install nzcensus package, if necessary:
devtools::install_github("ellisp/nzelect/pkg2")

library(nzcensus)
library(tidyverse)
library(scales)
library(forcats)
library(testthat)
library(gridExtra)

# Download the New Zealand Deprivation Index by area unit from Massey University
nzdep <- read.csv("http://cphronline.massey.ac.nz/data/csv?inline=true&viewId=96&viewName=New+Zealand+Index+of+Deprivation+Atlas&geoId=15&subsetId=&instances=11701",
                  stringsAsFactors = FALSE)
names(nzdep) <- c("AU", "AU_NAM", "Dep", "X")


# check we have matches for all the area units:
expect_equal(sum(nzdep$AU_NAM %in% AreaUnits2013$AU_NAM),
             nrow(nzdep))

# merge the deprivation index with the Area Unit data from the nzcensus package:
d <- nzdep %>%
  select(-X) %>%
  left_join(AreaUnits2013, by = "AU_NAM") %>%
  mutate(Dep5 = ceiling(Dep / 2)) %>%
  mutate(denominator = ResidentPop2013,
         Maori = PropMaori2013 * denominator,
         Maori = ifelse(is.na(Maori), 0, Maori),
         Pacific = PropPacific2013 * denominator,
         Pacific = ifelse(is.na(Pacific), 0, Pacific),
         Other = denominator - Maori - Pacific) %>%
  as_tibble()
{% endhighlight %}

## Exploration

My first step was to some exploration and reality-checking to see if I've obtained plausible results with my joining of data sets.  I started with some basic examination of population totals by ethnicity and deprivation decile, and saw the expected pattern of increasing numbers of Māori and Pasifika in the lower decile areas:

<img src='/img/0103-deprivation-ethnicity-barchart.svg' width='100%'>

From my understanding of the method of the deprivation index, there should be equal numbers of people in each decile of deprivation, meaning all the columns in the chart above should all be the same height.  Clearly they're not.  I'll file that one down to investigate later.  I did try constructing my own populations with both "Resident Population" and "Population on Census night", which didn't make any noticeable difference.

Of more interest, here's a comparison of the deprivation index with some of the census variables.  We see that the more deprived area units also have lower incomes, more smokers, more people of Pacific origin, and less people with partners:

<img src='/img/0103-boxplots.svg' width='100%'>

It's generally agreed that marital or partnering status is correlated with health, income and happiness, with causality probably going in both directions.  I hadn't expected this individual-level effect to show up so clearly when aggregated to area units as it does in the graphic above.

Here's the code for those exploratory plots:

{% highlight R %}
#-----------------exploratory graphics----------------------
d %>%
  group_by(Dep) %>%
  summarise(Maori = sum(Maori),
            Pacific = sum(Pacific),
            Other = sum(Other)) %>%
  gather(Ethnicity, Value, -Dep) %>%
  mutate(Ethnicity = fct_relevel(Ethnicity, "Other")) %>%
  ggplot(aes(x = as.factor(Dep), weight = Value, fill = Ethnicity)) +
  geom_bar(position = "stack") +
  scale_y_continuous("Usual resident population", label = comma) +
  labs(x = ("Massey Uni deprivation decile\n(higher numbers indicates living in a more deprived area)"),
       fill = "")

#' convenience function, just for this post, which draws
#' area unit box plots of a census variable by deprivation index
bp <- function(variable, ylab = NULL, label = percent, title = "",
               xlab = "Massey Uni deprivation decile\n(higher numbers indicates living in a more deprived area)"){
  if(is.null(ylab)){
    ylab <- gsub("Prop", "Proportion ", variable)
    ylab <- gsub("2013", "", ylab)
  }
  d %>%
    mutate(DepF = as.factor(Dep)) %>%
    ggplot(aes_string(x = "DepF", y = variable)) +
    geom_boxplot() +
    scale_y_continuous(ylab, label = label) +
    labs(x = xlab) +
    ggtitle(title)
  }

grid.arrange(
  bp("PropPacific2013", xlab = "", title = "Data by area unit from the New Zealand census 2013"),
  bp("PropSmoker2013", xlab = ""),
  bp("PropManagers2013"),
  bp("MedianIncome2013", "Median Income", dollar)
)
{% endhighlight %}

## Modelling
Finally, I needed to combine my area unit data with the family violence deaths aggregate data in the Committee's report to create the graphic I started this post with, illustrating the strength of the combined effect of being Māori and living in a deprived area:

<img src='/img/0103-interaction.svg' width='100%'>

I did some statistical modelling too, using a generalized linear model with a binomial response, taking the liberty of treating deprivation quintile as a continuous explanatory variable.  For those interested in such things, the results show evidence of the interaction effect.  The full model with the interaction is superior in terms of AIC (54 compared to 60), having residual deviance that matches the degrees of freedom, and a p-value showing "statistical significance" for what it's worth.

```
glm(formula = as.matrix(d2[, c("Offenders", "NonOffenders")]) ~ 
    Ethnicity * Dep5, family = binomial, data = d2)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.7477  -0.3859  -0.1435   0.4975   0.9886  

Coefficients:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)         -11.38628    0.27361 -41.615  < 2e-16 ***
EthnicityMaori       -0.78663    0.78752  -0.999    0.318    
Dep5                  0.28560    0.07298   3.913 9.11e-05 ***
EthnicityMaori:Dep5   0.43892    0.17473   2.512    0.012 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 121.6809  on 9  degrees of freedom
Residual deviance:   6.7239  on 6  degrees of freedom
AIC: 54.372
```

Here's the R code for merging the census data with the family violence data, drawing the chart and doing that statistical modelling.

{% highlight R %}
#==================family violence deaths==================
# Data from Figure 3, page 21 of  "Family Violence Death Review Committee" Report 5

fv <- data_frame(
  Dep5 = rep(1:5, 2),
  Ethnicity = rep(c("Maori", "NonMaori"), each = 5),
  Offenders = c(0,  1,  4,  17, 46, 
                13, 14, 22, 18, 37)
)

# check if got roughly equal to what it should be
expect_equal(sum(fv$Offenders), 195 - 9 - 5 - 9) 

fv %>%
  group_by(Ethnicity) %>%
  summarise(Offenders = sum(Offenders))

d2 <- d %>%
  mutate(NonMaori = Pacific + Other) %>%
  select(NonMaori, Maori, Dep5) %>%
  gather(Ethnicity, Population, -Dep5) %>%
  group_by(Dep5, Ethnicity) %>%
  summarise(Population = sum(Population)) %>%
  left_join(fv) %>%
  mutate(NonOffenders = round(Population - Offenders),
         Proportion = Offenders / Population,
         Ethnicity = fct_relevel(Ethnicity, "NonMaori"))


mod_saturated <- glm(as.matrix(d2[ , c("Offenders", "NonOffenders")]) ~ Ethnicity * Dep5, 
            family = binomial, data = d2)  

mod_simple <- glm(as.matrix(d2[ , c("Offenders", "NonOffenders")]) ~ Ethnicity + Dep5, 
                     family = binomial, data = d2)  

summary(mod_simple)
summary(mod_saturated)
anova(mod_saturated)

d2 %>%
  ggplot(aes(x = Dep5, y = Proportion, colour = Ethnicity)) +
  geom_point(aes(size = Population), alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 2, alpha = 0.5) +
  geom_point(aes(size = Population), shape = 1, colour = "black") +
  scale_y_continuous("Proportion of area sub-population that\nwas an offender in family violence deaths",
                     label = percent) +
  scale_size_area(max_size = 10, label = comma) +
  labs(x = "Deprivation quintile (higher means more deprived)",
       colour = "",
       caption = "Source: Stats NZ census 2013 meshblock data, Massey University 2013 NZ Deprivation Index,
Family Violence Death Review Committee Fifth Report; 
analysis at https://ellisp.github.io.") +
  ggtitle("Offenders in family violence deaths New Zealand 2009-15",
          "By deprivation of area and ethnicity (n=172)")
{% endhighlight %}


