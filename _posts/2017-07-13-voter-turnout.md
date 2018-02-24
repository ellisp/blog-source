---
layout: post
title: Who turned out to vote in the 2014 New Zealand general election?
date: 2017-07-13
tag: 
   - VotingBehaviour
   - R
description:  I explore the demographic characteristics of who voted (and who didn't), out of people on the electoral roll, in the 2014 New Zealand general election.  I use multiple imputation and a generalized linear model with a quasibinomial response.  The people who vote tend to have characteristics associated with doing ok out of society (owning a home, having a partner, university qualifications, etc).
image: /img/0104-model-results.svg
socialimage: http://ellisp.github.io/img/0104-model-results.png
category: R
---

## Motivation

This [thread on Twitter](https://twitter.com/ellis2013nz/status/884653816368422912) prompted some questions for me about who actually turns up to vote in New Zealand's elections.  With limited time, I can't answer many of the important questions raised in that thread and the article it refers to critically.  However, I can use the [New Zealand Election Study](http://www.nzes.org/) to look into the specific question raised about the demographics of people on the electoral roll who fail to vote, and in particular if income is a significant factor.  

To do this I adapted code from an [earlier post](http://ellisp.github.io/blog/2017/05/06/nz-first) where I modelled party vote based on individual socio-economic characteristics.  For the main analysis, I used "Voted" as the response variable in a regression model with about 20 possible explanatory variables; this lets us see the impact of each variable while simultaneously controlling for the others.  Based on thinking that I was interested in behaviour rather than reasons, I wrapped up into one category those who claimed they "chose not to vote" and those who just "didn't manage to vote".

The NZES sample is drawn from the electoral roll, which means the important question of who gets enrolled in the first place can't be analysed from it.

## Results

To cut to the chase, it's not *clear* that household income is a factor.  However, there are so many people who did not tell the surveyors their income that in this analysis and in my earlier post I was obliged to code these people separately; in the chart below they show up as "HHIncome don't know / NA".  Unlike the two "lower" and "higher" income groups they are contrasted to, this variable does show up as negatively related to likelihood to turn up to vote, even after controlling for all the other variables in the chart:

<img src='/img/0104-model-results.svg' width='100%'>

To be clear on the interpretation of this chart, the following are characteristics for which there was significant evidence of a relationship with *more* likely to vote:

- Own their own house or flat
- Someone in the household is a member of a professional association
- Live in a city
- Work part time (this is the only one that surprised me)
- Has a university qualification
- Older
- Married or with a long term partner

Generally speaking, these are mostly things associated with people who are doing well out of society.

The following factors are associated with being *less* likely to vote:

- Male
- Young
- Not European
- Income not known (ie not told to the interviewer)

I was interested that being Māori did *not* show up as significantly related to non-voting, above and beyond the general "non-European" factor (remembering that multiple ethnicities are usually allowed in New Zealand surveys and censuses).  Assuming the chart in the Twitter thread referred to above is correct, this must mean that the Māori indicator is conflated with some of the other variables - such as being younger, not owning one's own house or flat, not living in a city, not European, not having a university qualification, etc.  I re-ran a regression, this time with "being Māori" as the response variable, to check that and found this conflation was indeed happening.  

All up, this is pretty strong evidence for socio-economic disadvantage being a unifying factor in non-voting behaviour by people on the electoral roll.   The fact that the "being Māori" non-voting effect disappears when we control for these other factors probably counts as a real finding of interest.

It's a shame that we can't see a clear income effect in itself (other than the people who don't report income to surveyors), but income is notoriously difficult to measure in any social science context, so not that surprising.

## More exploration

I poked around the data just a little more before writing up.

### More granular exploration of income

Here is a mosaic plot of the original survey question on income matched to whether the respondent voted in the 2014 election:

<img src='/img/0104-mosaic-dhhincome.svg' width='100%'>

Once one is familiar with these charts, they are a powerful way of visualising a two-way cross-tab.  They are conceptually related to the basic Chi-square test used to test for independence of the two variables in a cross-tab like this.  The cells coloured blue indicate a "surprisingly" high number of people in that cell, relative to the null hypothesis of no relationship between the two variables.  Red means a surprisingly low number.  The area of each box indicates the number of people in that particular cell of the table.  For this "income by voting" plot we see:

- there are less people in the "don't know income and did vote" category than would be expected if the two variables were unrelated
- there are more people in the "don't know income" and "chose not to vote" or "didn't manage to vote" categories than would be expected
- people with incomes between $31,000 and $55,000 turned up in "chose not to vote" more than would have been expected
- people with incomes between $76,000 and $148,000 were less likely to be in "chose not to vote" than would have been expected

All up, those exploratory findings broadly match an expectation that people with lower incomes didn't vote, and those with higher incomes did; albeit with some complications in the detail.

This mosaic plot gives a more nuanced view of income than my regression, where I had to lump together categories from both variables.  For example, a regression that differentiated between those who "chose not to vote" and those "didn't manage to vote" would have been interesting but taken us into the world of multinomial responses which are extremely hard to explain visually, and which suck up more degrees of freedom from our fairly small sample size.

There's not a lot of people in many of the cells in this table.  With a bit more data, and a preparedness for some modelling complexity, I suspect we'd find an income effect somewhere.  To tackle this seriously and with a big enough sample size I'd want to use all the election studies from previous years.

### Some attitudinal variables

Here I present without comment some similar graphics comparing voting behaviour to some of the attitudinal questions in this survey:

<img src='/img/0104-mosaic-dinterest.svg' width='100%'>
<img src='/img/0104-mosaic-ddiffvoting.svg' width='100%'>
<img src='/img/0104-mosaic-ddiffpower.svg' width='100%'>
<img src='/img/0104-mosaic-ddemo.svg' width='100%'>
<img src='/img/0104-mosaic-dbigbus.svg' width='100%'>

There's lots to say here but to do it justice would require engaging much more with the political science literature than I have time for  just now.

## Method

Around a third of the 2,835 rows of data are missing at least one of the variables I wanted to include in my regression, so I needed to think carefully about my modelling strategy.  Choosing a simpler variant of the different methods I tried in my [earlier post on party vote](http://ellisp.github.io/blog/2017/05/06/nz-first), I used:

- the survey weights provided by the NZES organizers
- multiple imputations by chained equations (with the R `mice` package), imputing five alternative values for each missing value so we can fit five regressions and pool the results
- `glm` (with a quasi-binomial response to be safe) from the standard `stats` package with R, because it plays nicely with `mice` and my previous experience suggested there wasn't much to gain by using `svyglm` from the `survey` package with this particular dataset.

If I had more time and it was more important to me, I would have used `survey::svyglm` in combination with a bootstrap that encompasses the imputation process, as per the previous post.  My experience suggests that this is unlikely to change the result materially.

## Code

Here's the R code that did the analysis.  Two small points to note were that since my last post using this data, with the upgrade to R3.4.x,

- the `foreign` package seems to import the SPSS data slightly differently, which required a tweak to some of the code handling factors (on the plus side, I *think* it preserves more information from the SPSS version in doing so)
- the `mice` package stores the contrasts for factors it used in imputation in a different spot

Both these issues were food for thought and required a small amount of bug hunting.

{% highlight R %}
library(tidyverse)
library(scales)
library(foreign)   # for importing SPSS data
library(survey)    # for survey weighting and analysis
library(forcats)   # for manipulating factor levels
library(mice)

#-------------convenience functions--------------
camel_to_english <- function(camelCase){
  return(gsub("([a-z])([A-Z])", "\\1 \\L\\2", camelCase, perl = TRUE))
}

# Convert five category membership question (for trade unions, business
# associations, etc) into Yes or No.
membership <- function(x){
  tmp <- fct_recode(x,
                    Yes = "I belong, but no one else in the house does",
                    Yes = "I do, plus another in the house",
                    Yes = "Another person belongs, but not me",
                    No  = "No, no one belongs",
                    No  = "Don't know") 
  tmp <- ifelse(tmp == "Yes", 1, 0)
  # Uncomment the next line if we want to turn NA into 0.
  # tmp <- ifelse(is.na(tmp), 0, tmp)
  return(tmp)
}

#------------------data download-----------------

# Data downloaded from http://www.nzes.org/exec/show/data and because
# they want you to fill in a form to know who is using the data, I
# won't re-publish it myself
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE)
varlab <- cbind(attributes(nzes_orig)$variable.labels)

#============rationalised version of feature creation===========
# This is a single longish command to aggregate various answers into
# simpler cateogrisations, because we don't have enough data to 
# analyse the original granular detail.
nzes <- nzes_orig %>%
  
  # Two degrees of freedom for ethnicity:
  # Oddly, this has changed since a few months ago.  An upgrade to foreign (I think)
  # means that the SPSS data comes in with its labels.  So this bit of code is slightly
  # different to some I'd previously blogged about:
  mutate(NotEuropean = 1 - (dethnicity_e == "NZ European"),
         Maori = 1 * (as.numeric(dethnicity_m) == 2)) %>%
  
  # Two degrees of freedom for income (lower, higher, don't know):
  mutate(HHIncome = fct_recode(dhhincome,
                               Lower = "No income",
                               Lower = "$NZ23,000 or less",
                               Lower = "$NZ23,001-$NZ31,000",
                               Lower = "$NZ31,001-$NZ39,800",
                               Lower = "$NZ39,801-$NZ55,000",
                               Higher = "$NZ55,001-$NZ76,100",
                               Higher = "$NZ76,101-$NZ110,800",
                               Higher = "$NZ110,801-$NZ147,699",
                               Higher = "$NZ147,700 or over",
                               `Don't know / NA` = "Don't know"),
         HHIncome = ifelse(is.na(HHIncome), "Don't know / NA", as.character(HHIncome)),
         HHIncome = fct_infreq(HHIncome)) %>%
  
  ## Two - four degrees of freedom for associations?
  mutate(HHMemberTradeUnion = membership(dtradeunion),
         HHMemberProfAssoc = membership(dprofassoc)) %>%
  
  ## One degree of freedom for born in NZ
  mutate(NZBorn = ifelse(dnzborn == "New Zealand", 1, 0)
         # uncomment the next line if you want to turn NA into zero:
         # , NZBorn = ifelse(is.na(NZBorn), 0, NZBorn)
  ) %>%
  
  ## One for sex
  mutate(Male = 1 * (dsex == "Male"),
         Male = ifelse(dsex == "Transsexual or transgender", NA, Male)) %>%
  
  ## Two-four for age
  mutate(Age = fct_collapse(as.character(dage),
                            `18-29` = as.character(18:29),
                            `30-55` = as.character(30:55),
                            `56+` = as.character(56:100)),
         # Uncomment the next line if you want to explicitly code the NAs
         # Age = ifelse(is.na(dage), "unknown", as.character(Age)),
         Age = fct_relevel(Age, "30-55")
  ) %>%
  
  ## One for housing.  Note there is also an alternative question "do you or any family member own a residence"
  mutate(OwnHouseOrFlat = 1 * grepl("Own house or flat", dhousing)) %>%
  
  # Two for religion
  mutate(Religion = fct_lump(dreligion, 2)) %>%
  
  # One for marital status
  mutate(Marital = 1 * (dmarital == "Married, in a civil union, or living with a partner")) %>%
  
  # One for self-identified class
  mutate(IdentifyWorkingClass = 1 * (dclass == "Working class")) %>%
  
  ## Two for education (University, None, Other)
  mutate(HighestQual = fct_collapse(dedcons, University = c("Undergraduate Degree", "Postgraduate degree", "PhD")),
         HighestQual = fct_lump(HighestQual, 2),
         HighestQual = ifelse(dedcons == "Not known", NA, as.character(HighestQual)),
         HighestQual = fct_relevel(HighestQual, "Other")
  ) %>%
  
  ## Two for working status
  mutate(WorkStatus = ifelse(!is.na(dwkpt), "Part time", NA),
         WorkStatus = ifelse(!is.na(dwkft), "Full time", WorkStatus),
         WorkStatus = ifelse(is.na(WorkStatus), "Other or unknown", WorkStatus),
         WorkStatus = fct_infreq(WorkStatus),
         Student = 1 * (!is.na(dwksch))) %>%
  
  ## One for occupation
  mutate(SuperviseAnyone = 1 * (dsupervise == "Yes")) %>%
  # Note there is detailed occupation information (for respondent and partner)
  # but I don't think we hav eneough data to use this in the model.
  
  ## One degree of freedom for area lived in?
  # Five nice categories here, not enough data so we'll split into one
  mutate(City = 1 * (dregsize == "A major city (over 100,000 population)")) %>%
  
  mutate(Voted = 1 * (ddidvote == "Did cast a vote"))

# Mysterious Code 9 from outer space (in the original, see http://www.jackvowles.com/SectionD2014.htm)
is.na(nzes$Voted) <- (nzes$ddidvote == "9")

#==============descriptive stats==================
# create a survey design object that understand the weights:
nzes_svy <- svydesign(~1, weights = ~dwtfin, data = nzes)


#' Function to estimate a survey cross tab and draw a mosaic plot, comparing
#' a given variable to whether the person voted (excluding those who said "9"
#' in response to the question on whether they voted)
mp <- function(variable, ylab = ""){
  form <- as.formula(paste("~", variable, "+ ddidvote"))
  tab <- svytable(form, nzes_svy)
  oldpar <- par(font.main = 1)
  mosaicplot(t(tab[ , -4]), shade = TRUE, las = 2, 
             main = "New Zealand Election Study 2014",
             xlab = "D2: Did you vote or not vote?",
             ylab = ylab)
  par(oldpar)
}

# Draw the mosaic plots used in the post:
mp("dinterest", "A1: how interested in politics")
mp("ddiffpower", "A10: does it make a difference who is in power")
mp("ddemo", "A12: how satisfied with how democracy works in NZ")
mp("ddiffvoting", "A11: does voting make any difference to what happens")
mp("dbigbus", "C7m: big business in NZ has too much power")
mp("dhhincome", "F26: household income between 1 April 2013 and 31 March 2014")

# too much missing data - nearly a third of rows missing a column - for this to be any use, but as a taster:
mod1 <- svyglm(Voted ~ NotEuropean + Maori + HHIncome + HHMemberTradeUnion + HHMemberProfAssoc +
                 NZBorn + Male + Age + OwnHouseOrFlat + Religion + Marital +
                 IdentifyWorkingClass + HighestQual + WorkStatus + 
                 Student + SuperviseAnyone + City, 
               design = nzes_svy, family = "quasibinomial")
summary(mod1)
anova(mod1)


#============modelling with imputation===============
# create a dataset of just the variables we're using:
nzes_subset <- nzes %>%
  select(Voted, NotEuropean, Maori, HHIncome, HHMemberTradeUnion,
         HHMemberProfAssoc, NZBorn, Male, Age, OwnHouseOrFlat,
         Religion, Marital, IdentifyWorkingClass, HighestQual, WorkStatus,
         Student, SuperviseAnyone, City, dwtfin)

# check how many complete cases there are:
sum(complete.cases(nzes_subset))
nrow(nzes_subset)

# create 5 different datasets with (different) imputed values where missing
nzes_imp <- mice(nzes_subset)

# remove contrasts to make it easier to interpret once the model is fit
# note that these contrasts are now in the $data object - some time in the 
# last year this changed, compared to what worked in my 6 May 2017 blog.
attributes(nzes_imp$data$Religion)$contrasts    <- NULL
attributes(nzes_imp$data$WorkStatus)$contrasts  <- NULL
attributes(nzes_imp$data$HHIncome)$contrasts    <- NULL
attributes(nzes_imp$data$HighestQual)$contrasts <- NULL
attributes(nzes_imp$data$Age)$contrasts         <- NULL

# fit model
mod2 <- with(nzes_imp, 
             glm(Voted ~ NotEuropean + Maori + HHIncome + HHMemberTradeUnion + HHMemberProfAssoc +
                              NZBorn + Male + Age + OwnHouseOrFlat + Religion + Marital +
                              IdentifyWorkingClass + HighestQual + WorkStatus + 
                              Student + SuperviseAnyone + City, 
                 family = "quasibinomial", weights = dwtfin))

# turn into a graphic:
coefs <- as.data.frame(summary(pool(mod2)))
coefs$variable <- camel_to_english(row.names(coefs))

coefs %>%
  # remove intercept as uninteresting:
  slice(-1) %>%
  # reorder:
  mutate(variable = fct_reorder(variable, t)) %>%
  # draw graphic:
  ggplot(aes(x = `lo 95`, xend = `hi 95`, y = variable, yend = variable)) +
  geom_vline(xintercept = 0, size = 2.5, colour = "orange") +
  geom_segment(size = 5, colour = "steelblue", alpha = 0.8) +
  ggtitle("Who voted in New Zealand's Election 2014?",
          "Of those enrolled to vote, when controlling for other variables, which variables were associated with voting.
Width of blue bars indicates uncertainty associated with sampling and imputation.") +
  labs(x = "Less likely to vote                        -----------------                            More likely to vote                           ",
       y = "Compared to enrollees of no religion, aged 30-55, 
high household income, school qualification, 
working full time",
       caption = "Source: New Zealand Election Study; analysis at http://ellisp.github.io")


#=================who is Maori=======================
mod3 <- with(nzes_imp, 
             glm(Maori ~ NotEuropean + HHIncome + HHMemberTradeUnion + HHMemberProfAssoc +
                   NZBorn + Male + Age + OwnHouseOrFlat + Religion + Marital +
                   IdentifyWorkingClass + HighestQual + WorkStatus + 
                   Student + SuperviseAnyone + City, 
                 family = "quasibinomial", weights = dwtfin))


coefs3 <- as.data.frame(summary(pool(mod3)))
coefs3$variable <- camel_to_english(row.names(coefs3))

coefs3 %>%
  arrange(t)
{% endhighlight %}

