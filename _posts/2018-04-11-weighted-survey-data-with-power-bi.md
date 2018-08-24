---
layout: post
title: Weighted survey data with Power BI compared to dplyr, SQL or survey
date: 2018-04-11
tag: 
   - NewZealand
   - OpenData
   - Surveys
   - Tools
description: I show a workaround to make it (relatively) easy to work with weighted survey data in Power BI, and ruminate on how this compares to other approaches of working with weighted data.
image: /img/0121-powerbi.png
socialimage: http://freerangestats.info/img/0121-powerbi.png
category: R
---

## A conundrum for Microsoft Power BI

I've been familiarising myself with Microsoft *Power BI*, which features prominently in any current discussion on data analysis and dissemination tools for organisations.  It's a good tool with some nice features.  I won't try to do a full review here, but just ruminate on one aspect - setting it up for non-specialists to explore weighted survey data.  For this, I want to be able to do appropriately weighted cross-tabs, but I'm not expecting anything that is either more sophisticated or more upstream in the data processing chain.  Actually *creating* the weights, and estimating sampling uncertainty based on them, is something for another tool like R.

Judging from discussion threads like [this one](http://community.powerbi.com/t5/Desktop/case-weighting-in-data/td-p/51599) I'm not the only one who wishes you could just say "apply case weights" in the way that you would with SPSS or a market research cross tab tool.  In fact, there are some tutorials out there on elaborate and painful ways of getting around this problem that seem totally surreal to me, being used to the ease with which R or Stata deal with such problems.  

*Caveat on what follows - my total experience with Power BI can be measured in hours rather than days, so please take the below with a grain of salt.  I may have missed something important.*

I was worried that inability to deal with weighted data could be a deal breaker for the purpose I was thinking of, and when I found out that a recent release [proudly touted the ability to do a weighted average](https://powerbi.microsoft.com/en-us/blog/tag/weighted-average/) (in a way that didn't even help me much) I nearly gave up on it in disgust.  Power BI lets the developer write R code and at one point I was considering the successful workflow was to pass everything through to R and send it back, before realising that this made no sense at all - might as well do the whole thing in R if that's what it takes.

However, a few hours of experimentation and trying to get my head around a different way of thinking, and it turns out the solution wasn't too difficult.  It all comes down to understanding the way Power BI differentiates between static *columns* of data as opposed to *measures* which are calculated on the fly.

Once I'd cracked the problem I made a couple of Power BI reports with weighted microdata from complex surveys to be sure it worked generally.  Here's one that's been made with public data, the New Zealand International Visitor Survey.  It took about 20 minutes to make this, after I'd familiarised myself with the toolkit on another (non-public) dataset.  It's live and interactive, in fact interactive in too many ways to try to describe, so just have a play with it:

<iframe width="740" height="510" src="https://app.powerbi.com/view?r=eyJrIjoiNjNhZDc3MGEtNDkzOS00ZDAwLWE0NTUtNzBmOTViZWU0NDIwIiwidCI6IjBlNzE2NzI4LTA1MDItNDkwZS04YTU2LTk0Y2Q5N2I1YWE2OCJ9" frameborder="0" allowFullScreen="true"></iframe>

*Disclaimer - I've been responsible for that survey in the past, but not for more than a year now.  What follows is very much written as a private citizen.*


## Introducing some data

This example survey is one I've [blogged about before](/blog/2018/02/03/ivs).  It's an on-going survey of 5,000 to 10,000 tourists per year on their departure from New Zealand.  Sample size, questionnaire and mode have varied over time, but the Ministry of Business, Innovation and Employment [publish a backcast set of the microdata](http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs/data-download) that is as comparable across time as is possible.  It's about 24MB to download.  For today's demo, I'm only going to use the simplest part of the data - the `vw_IVSSurveyMainHeader` table which has one row for each of the 125,000 respondents since 1997.  Here's code to download it, including a couple of convenience R functions that MBIE use to help classify countries into groupings (dated I'm afraid - I can criticise them because I wrote them myself in 2011).  I also reduce the dataset to just 8 columns so when I get into Power BI I won't have to deal with the complexity of the full data:

{% highlight R %}
library(readr)
library(survey)
library(dplyr)

source("https://github.com/nz-mbie/mbie-r-package-public/raw/master/pkg/R/CountryManip.R") # for CountryGroup
source("https://github.com/nz-mbie/mbie-r-package-public/raw/master/pkg/R/NZTourism.R")    # for rename.levels

# download survey data from the MBIE (Ministry of Business, Innovation and Employment) website
download.file("http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs/documents-image-library/vw_IVS.zip",
              mode = "wb", destfile = "vw_IVS.zip")

unzip("vw_IVS.zip")
ivs <- read_csv("IVS/vw_IVSSurveyMainHeader.csv")

ivs_sub <- ivs %>%
  mutate(Country = CountryGroup(CORNextYr)) %>%
  select(Country, Year, PopulationWeight, WeightedSpend, POV, Gender, AgeRange, SurveyResponseID) %>%
  rename(Spend = WeightedSpend) %>%
  as_tibble() %>%
  arrange(Year)

# save a copy to use as the data source for Power BI  
write_delim(ivs_sub, path = "ivs-1997-to-2017.txt", delim = "|")
{% endhighlight %}

Of these variables:

- `POV` stands for "purpose of visit", a key concept in tourism data analysis that will be familiar to travellers from many countries' arrival or departure cards.
- `WeightedSpend` actually means "outlier-treated spend"
- `PopulationWeight` is the survey weight, after all sorts of complex post-stratification including for age, gender, airport, country of residence and purpose of visit.

Now, I'm interested in weighted counts of people for various combinations of dimensions (like year and purpose), and also in weighted averages and totals of continuous variables like "spend" and "nights in New Zealand".  If I were using `dplyr`, to get those yearly summary estimates for the years since 2011 I'd do something like:

{% highlight R %}
ivs_sub %>%
  filter(Year > 2010) %>%
  group_by(Year) %>%
  summarise(total_spend = sum(Spend * PopulationWeight),
            people = sum(PopulationWeight),
            mean_spend = total_spend / people) %>%
  arrange(desc(Year))
{% endhighlight %}

Or the exact equivalent operation in SQL:

{% highlight SQL %}
SELECT
  SUM(Spend * PopulationWeight)                         AS total_spend,
  SUM(PopulationWeight)                                 AS people,
  SUM(Spend * PopulationWeight) / SUM(PopulationWeight) AS mean_spend,
  Year
FROM ivs_sub
WHERE Year > 2010
GROUP BY Year
ORDER BY Year DESC
{% endhighlight %}


I love `dplyr` because the code is ordered in the way I think of the operation: take a dataset, filter it, group it by some variable, summarise it in a particular way and then sort the results.  Whereas in SQL you have to look down near the bottom for the `FROM` and `WHERE` statements to see what data you're talking about.  But it really doesn't matter in this sort of case, they both work fine and fast and are pretty readable.

Those are both (to my mind) database-y ways of telling a computer to do something.  A more statistically oriented way is to create a new object that somehow encompasses the survey design and its weights, and abstract the weighting of estimates away from the user.  That's the approach taken (with greatly varying degrees of statistical rigour) by commercial cross-tab tools used by market researchers, statistical packages like SPSS and Stata, and Thomas Lumley's `survey` package in R.  Here's how you'd get mean spend per year this way (I'm ignoring the complexity in the survey design as I'm only interested in the point estimates for today)

{% highlight R %}
ivs_svy <- svydesign(~1, weights = ~PopulationWeight, data = ivs_sub)
mean_spend <- svyby(~Spend, ~Year, design = ivs_svy, FUN = svymean)
tail(round(mean_spend))
{% endhighlight %}

which has these results:

{% highlight R %}
> tail(round(mean_spend))
     Year Spend se
2012 2012  2772 49
2013 2013  2717 48
2014 2014  2879 50
2015 2015  3440 69
2016 2016  3225 63
2017 2017  3187 75
{% endhighlight %}

It's noticeably slower than `dplyr` or SQL, but that's because it's doing a lot more calculation and giving you the appropriate sampling error as well as the point estimates.  And once you've invested in creating the survey design object, it's a lot simpler to forget about the weights and just use `svymean`, `svytotal`, `svyquantile` and so on.

## Weights in Power BI

Power BI is an eco-system rather than a single tool, with three main parts: a desktop application, a web service, and a mobile app.  A typical (but by no means the only) workflow is to do some analysis in the desktop application and create an interactive report or dashboard; and "publish" it to the web service where it can be shared either with other Power BI users, or simply as a web page like my example earlier.

Power BI is an amazing tool with things like natural language queries, but unfortunately there's no simple way to just say "weight the data please, for all subsequent analyses".  So we have to do it old-school, something closer to those original `dplyr` or SQL queries.

For simple counts this is actually easy - we just need to tell it to report the sum of weights for each combination of variables.  This fits in very nicely with how Power BI sees the world, which is basically as a giant pivot table.  So  no problem there.

For totals of spend (or another numeric variable), it's also fairly straightforward.  You need to create a new *column* of weight multiplied by the original value, added to the original data rectangle.  This column is just going to be a bunch of static numbers.  It's defined this way in Power BI:

```
spend_by_weight = 'ivs-1997-to-2017'[PopulationWeight] * 'ivs-1997-to-2017'[Spend]
```

Now this column can be used as the value cell in reporting tables and charts and we're all fine.  It's annoying to have to create a persistent column for each weighted numeric variable rather than do it on the fly (as we did in SQL and `dplyr`) during the grouping and aggregation, but the gain comes with all the automated filtering interactivity of working in Power BI.

The weighted average is more complex.  Imagine we now have a table with a row for each year and a column for total spend and for total visitors.  We just want to divide spend by visitors, right?  That's what happens in `dplyr`, where we took advantage of the fact that variables created first in the `summarise()` statement can then be referred to further down in the same query.  

I wasted a fair bit of time fiddling with how my reporting table was defined before I understood that the problem comes from all the flexibility for the end user such tables have in Power BI, which puts constraints on the developer.  In particular, if the user selects another graph in the same page (or a report-wide filter), the data behind all linked tables gets automatically filtered (go back to my report at the top of the post and try it).  This is like adding new `filter()` functions to our `dplyr` statement (or `WHERE` clauses in SQL).  Power BI won't let you treat the columns in a reporting table as first class objects in their own right; and there's no way to add a column to the original static data that can be just neatly aggregated into a weighted average for any combination of filter, slice and dice that is required once it gets into that reporting table.

It turns out that the way around this is to define a "measure", which is a more powerful concept in Power BI than a simple static column, even though it appears in a data source's column list and looks similar.  We define the measure we want this way, referring to the `spend_by_weight` column we'd already made for use in aggregating totals:

```
mean_spend = sum('ivs-1997-to-2017'[spend_by_weight]) / sum('ivs-1997-to-2017'[PopulationWeight]) 
```

Once defined this way, the new `mean_spend` measure will be calculated dynamically and correctly for whatever combination of variables it is combined with in the visualization.  It's like defining part of `dplyr`'s `summarise()` clause in advance, then whatever the user defines as `group_by` variables (by pointing and clicking) kicks the measure into action.

So, in summary, to work with weighted data in Power BI you need

- a column of weights (obviously)
- for each numeric variable you want to the weighted total of by any particular slice and dice, a new static *column* of the original value multiplied by the weight
- for each numeric variable you want the weighted mean of by any particular slice and dice, a new dynamic *measure* defined in advance of the sum of the column defined in the step above (ie total of that variable), divided by the sum of the aggregated weights (ie population)

Note that this means two extra variables (one column and one measure)  for each existing numeric variable.  This has some implications for the most effective data model to use - more normalization, with long and skinny relational tables covering several "variables" probably better than a wide table with a column for each.  Which makes sense for all sorts of other reasons too.

I have other thoughts about Power BI (or close competitors like Tableau) and its place in the world vis a vis either hand-crafted JavaScript or the Shiny platform, but they can wait for a later post.