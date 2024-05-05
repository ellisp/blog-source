---
layout: post
title: Spend on petrol by income
date: 2018-07-01
tag: 
   - Demography
   - Simulations
   - R
description: I explore the relationship between household income and expenditure on gasoline and motor oil in the USA Bureau of Labor Statistics' Consumer Expenditure Survey.
image: /img/0124-scatter-1.svg
socialimage: https:/freerangestats.info/img/0124-scatter-1.png
category: R
---

## Fuel tax debates
So, there's currently a [vibrant debate on a small New Zealandish corner of Twitter](https://twitter.com/simonbwilson/status/1012792274332233728) about a petrol tax coming into effect in Auckland today, and the different impacts of such taxes on richer and poorer households.  

The Government has released analysis from the Stats NZ Household Expenditure Survey [showing higher petrol consumption per household for higher income households](https://www.nzherald.co.nz/nz/news/article.cfm?c_id=1&objectid=12078915) (and hence paying more of the new tax).  [Sam Warburton, an economist with the New Zealand Institute, argues in response](https://twitter.com/Economissive/status/1012944031469338625) that poorer households have older, often larger, less efficient vehicles, leading to higher fuel costs per kilometre.  This means a fuel tax will not only result in poor people paying more as a percentage of their income (as any sales tax on basic commodities will do), but paying more *per kilometre*.  Further, poor people are more likely to live out of the big urban centres, and when in an urban area are less likely to be well serviced by public transport.  

Mr Warburton also argues that the results from the Household Expenditure Survey are misleading because they include "people who can't afford or otherwise don't own cars".  [His own analysis ](https://twitter.com/Economissive/status/981306458699476992) from the Ministry of Transport's Household Travel Survey shows that households including two or more children, with Māori, with unemployed people, or in the poorer regions all pay more tax per kilometre.

I'm totally convinced by Mr Warburton on the argument that poorer people are paying more tax per kilometre, which makes a fuel tax a particularly [regressive tax](https://en.wikipedia.org/wiki/Regressive_tax). Even paying the same rate per kilometre would be regressive because transport costs are a higher proportion of income for poorer people; so more tax *per kilometre* is really rubbing salt into the wound.  I'm not convinced though by the suggestion that they will pay more of the new tax even in absolute terms; I'm inclined to trust the Household Economic Survey on this one.

## The USA Consumer Expenditure Survey

One good thing about this debate was it motivated me to do something that's been on my list for a while, which is to get my toes in the water with the USA Bureau of Labor Statistics' impressive [Consumer Expenditure Survey](https://www.bls.gov/cex/home.htm).  Huge amounts of confidentialised microdata from this mammoth program are freely available for download - without even filling in any forms!  This makes it suitable to use in a blog post in a way I couldn't with the New Zealand or Australian Household Expenditure Surveys (both of which have Confidentialised Unit Record Files for researchers, but with restrictions that get in the way of quick usage in public like this).

Big caveat for what follows - literally I looked at this survey for the first time today, and it is *very complex*.  Just for starters, the Consumer Expenditure Survey really [comprises two surveys](https://www.bls.gov/cex/) - an Interview Survey for "major and/or recurring items" and the Diary Survey for "minor or frequently purchased items".  It is very possible that I am using not-the-best variables.  Feedback welcomed.

### Densities of income and fuel spend

Let's get started by downloading the data.  Here's a couple of graphs of what I think are the main variables of interest here.  These draw on:

- `gasmocq` "Gasoline and motor oil this quarter"
- `fincbtxm` "Total amount of family income - imputed or collected" (in the past 12 months? although the data dictionary only implies this)
- `fam_size` "Number of Members in CU" (ie in responding household)
- `bls_urbn` "Is this CU located in an urban or a rural area"


<img src='/img/0124-densities.svg' width='100%'>

<img src='/img/0124-densities-inc.svg' width='100%'>

Both these graphics are showing a quantity divided by household size to get a simple estimate of amount per person.  A better approach would be to used equivalised figures, taking into account economies of scale for larger households and different cost structures for different age groups, but it probably would have taken me all morning just to work out a safe way of doing that so I've stuck with the simpler method.

Both these graphics - and most of those that follow - also use [John and Draper's modulus transform that I wrote about in some of my first ever posts on this blog](/blog/2015/09/05/creating-a-scale-transformation).  It's a great way to effectively visualise heavily skewed data that has zero and negative values as well as positive, a common occurrence with economic variables.  The helper functions for the transformation are available in my bag-of-misc R package `frs` available from GitHub.

Here's code to do that download, prepare the data for later and draw graphs:

{% highlight R lineanchors %}
# mandatory reading: https://www.bls.gov/cex/pumd_novice_guide.pdf

library(haven)
library(tidyverse)
library(scales)
library(frs)    # for modulus transforms of scales
library(MASS)   # for rlm.  Watch out for dplyr::select and MASS::select clash.
library(ggExtra)
library(survey)

# download the latest year's data (caution - 71MB):
dir.create("bls-cd")
download.file("https://www.bls.gov/cex/pumd/data/stata/intrvw16.zip",
              destfile = "bls-cd/intrvw16.zip", mode = "wb")

# download the data dictionary
download.file("https://www.bls.gov/cex/pumd/ce_pumd_interview_diary_dictionary.xlsx",
              destfile = "bls-cd/ce_pumd_interview_diary_dictionary.xlsx", mode = "wb")

unzip("bls-cd/intrvw16.zip", exdir = "bls-cd")

# The FMLI files have characteristics, income, weights, and summary expenditure
# the numbers in file names refer to year and quarter.  So intrvw16/fmli162.dta is 
# 2016, second quarter

# Import 2017 first quarter:
fmli171 <- read_dta("bls-cd/intrvw16/fmli171.dta")

# looks like no attributes associated with the various columns; have to use the Excel data dictionary
str(attributes(fmli171))
attr(fmli171, "labels")
attr(fmli171, "label")

# which columns are  numeric, candidates to hold income and expenditure non-binned data:
formats <- sapply(fmli171, function(x){attr(x, "format.stata")})
formats[grepl("[0-9]g", formats)]

# Bit of basic processing for convenience.  Make a new data frame called d with some labels we need later,
# and the per person variables calculated in one place.  
# First, the code labels for income classes:
inclasses <- c("Less than $5,000", "$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999",
    "$20,000 to $29,999", "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $69,999",
    "$70,000 and over")

inclass_lu <- data_frame(
  inclass = paste0("0", 1:9),
  inclass_ch = ordered(inclasses, levels = inclasses)
) 

# create the working version of the data frame:
d <- fmli171 %>%
  mutate(gas_pp =  gasmocq / fam_size * 4,
         inc_pp = fincbtxm / fam_size,
         gas_p_inc = gasmocq * 4 / fincbtxm,
         no_gas = as.integer(gas_pp == 0),
         bls_urbn_ch = ifelse(bls_urbn == 1, "Urban", "Rural"),
         ) %>%
  left_join(inclass_lu, by = "inclass")


# parameter for how much to transform the dollar scales:
lambda <- 0.2

ggplot(d, aes(x = gas_pp, colour = bls_urbn_ch)) +
  geom_density() + 
  scale_x_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) + 
  geom_rug() +
  ggtitle("Spend on petrol has a spike at $0 and a skewed distribution beyond that",
          "Unweighted, 2017 quarter 1. Horizontal scale has been transformed.") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       colour = "",
       x = "Expenditure per person on gasoline and motor oil this quarter x 4")

# some looks at various variables as described in the Excel data dictionary:
summary(fmli171$cuincome) # total income; not present
summary(fmli171$earnincx) # earnings before tax; not present
table(fmli171$earncomp)   # composition of earners
table(fmli171$cutenure)   # housing tenure (6 categories)
table(fmli171$inclass)    # income bracket
summary(fmli171$inc_rank) # Weighted cumulative percent ranking based on total current income before taxes (for complete income reporters)
summary(fmli171$othrincm) # other income
summary(fmli171$ffrmincx) # farm income, not present
summary(fmli171$fincbtax) # total amount of family income before taxes in the past 12 months
summary(fmli171$finlwt21) # final calibrated weight

ggplot(d, aes(x = inc_pp, colour = bls_urbn_ch)) +
  geom_density() +
  geom_rug() +
  ggtitle("After imputation, income has a skewed distribution with no spike at zero",
          "Unweighted, 2017 quarter 1. Horizontal scale has been transformed.") +
  scale_x_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       colour = "",
       x = "Family income per person before taxes in the past 12 months")

{% endhighlight %}

### Relationship of income and fuel spend

Here are some different ways of looking at the relationship between household income and the amount spent on fuel.  They all show a lot of variation between individual households, but significant evidence of a material positive relationship between the two.

First, here's a graph that tries to combine the binned "income classification" with the ranking of the household  on income(ie its quantile if you like).  The categories aren't as neat as might be expected, I'm pretty sure because of these variables representing different states of imputation:

<img src='/img/0124-quantile.svg' width='100%'>

The collection of people who don't spend any money on petrol drags the regression line downwards, but it's the slope that counts; definitely upwards.  The higher ranked a household is on income, the more they spend per person on gasoline and motor oil.

BTW, note that the points in these plots are different sizes.  The size is mapped to the calibrated survey weight indicating how many people in the US population each sample point is representing; this is a good starting point for trying to represented weighted data in a scatter plot.

I'm wary of using quantiles or rankings in this sort of analysis; I don't see much or any gain over other transformations and new risks and interpretability problems are introduced.  Perhaps more usefully, here are some straightforward scatterplots of income per person and vehicle fuel expenditure per person:

<img src='/img/0124-scatter-1.svg' width='100%'>

<img src='/img/0124-scatter-2.svg' width='100%'>

No doubt about that strong relationship; poorer households spend less on fuel (and nearly everything else, of course, although that's not shown) than do richer households.

On the other hand, there's equally no doubt that poorer households spend more on fuel as a proportion of their income:

<img src='/img/0124-props.svg' width='100%'>

Here's the code for those four graphics:

{% highlight R lineanchors %}
#-----------income quantile v fuel scatter plot-------------------
ggplot(d, aes(y = gas_pp, x = inc_rank, size = finlwt21)) +
  geom_point(aes(colour = inclass_ch), alpha = 0.2) + 
  geom_smooth(method = "rlm", aes(weight = finlwt21)) +
  scale_y_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  labs(x = "Income quantile") +
  ggtitle("Spend on petrol increases as income quantile of the household increases",
          "Blue line shows robust regression using M estimator.  Vertical axis is transformed.") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       colour = str_wrap("Income class of household based on income before taxes", 20),
       y = "Expenditure per person on gasoline and motor oil this quarter") +
  theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  scale_size_area(guide = "none")

#-----------scatter plots----------------
p <- ggplot(d, aes(x = inc_pp, y = gas_pp, size = finlwt21)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "rlm") +
  scale_x_continuous("Income per person in household in past 12 months",
                     label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  scale_y_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  theme(legend.position = "none") +
  ggtitle("Spend on petrol increases as income of the household increases",
          "Blue line shows robust regression using M estimator.  Both axes are transformed.") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       y = "Expenditure per person on gasoline and motor oil this quarter x 4") 

  
ggMarginal(p, type = "density", fill = "grey", colour = NA)

ggMarginal(p %+% filter(d, gas_pp > 0 & inc_pp > 0), 
           type = "density", fill = "grey", colour = NA)

#-----------proportion spent on fuel-------------		   
d %>%
  filter(inc_pp > 1000) %>%
  ggplot(aes(x = inc_pp, y = gas_p_inc, size = finlwt21)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous("Income per person in household in past 12 months",
                     label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous() +
  geom_smooth(se = FALSE) +
  theme(legend.position = "none") +
  ggtitle("Poorer households spend more on petrol proportionately than do richer households") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       y = "Expenditure per person on gasoline and motor oil\nas a proportion of income") 
{% endhighlight %}

### Who is likely to spend nothing on petrol at all?

Finally, I was interested in who spends nothing on petrol at all.  This relates to Mr Warburton's argument that the New Zealand Household Economic Survey if flawed for these purposes because the average spend on petrol includes people who have been priced out of vehicles altogether.  In fact, with the US data, there is a very strong negative relationship between household income and the probability of spending zero on gasoline and motor oil.:

<img src='/img/0124-no-gas.svg' width='100%'>
<img src='/img/0124-no-gas2.svg' width='100%'>

However, as the previous scatterplots showed, removing either or both of the zero income and zero fuel spend cases from the US Consumer Expenditure survey doesn't serve to remove the relationship between income and gasoline spend.

Finally, here's the code for this last bit of analysis:

{% highlight R lineanchors %}
p1 <- d %>%
  ggplot(aes(x = inc_pp, y = no_gas, size = finlwt21)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_x_continuous("Income per person in household in past 12 months",
                     label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  labs(y = "Probability of spending $0 on gasoline and motor oil this quarter\n") +
  theme(legend.position = "none") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey 2017 Q1") +
  ggtitle("Poorer households are more likely to spend nothing on petrol at all",
          "Blue line shows logistic regression; points show American households")

print(p1)

p2 <- p1 %+% filter(d, inc_pp > 0) + 
  ggtitle("", "Effect holds even if restricted to households with positive income")
print(p2)

# Check out the relationship with slightly more formal modelling than just on the fly in the graphic.
# crude approximation of the survey design, that takes just the primary sampling units and weights
# into account but ignores the stratification and post-stratification calibration (this will be
# conservative for inference so that's ok).  
# Also, better would be to transform `inc_pp`, or use a gam, or something.  This will do for now!
dd <- svydesign(~psu, data = d, weights = ~finlwt21)

mod <- svyglm(no_gas ~ inc_pp, design = dd, family = "quasibinomial")
summary(mod)
{% endhighlight %}

Just as I was finalising this post, [new discussion started](https://twitter.com/Thoughtfulnz/status/1013288397392117760) suggesting the American Time Use Survey as a good source of microdata to directly analyse the question of whether poorer households travel more or less.  Looks a good topic to come back to at some point.



