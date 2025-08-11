---
layout: post
title: Time use and fertility rates - Part I
date: 2025-08-11
tag: 
   - ModellingStrategy
   - DataFromTheWeb
description: A cross-country panel regression of fertility rate on the gender split of time spent on care and domestic chores. With musings on causality, DAGs, modelling strategy, and sourcing data.
image: /img/0290-dg.svg
socialimage: https:/freerangestats.info/img/0290-dg.png
category: R
---

## Time-use and number of children
Some months ago a post floated across my Bluesky feed making an argument to the effect of "if societies want more children, then men should do more of the housework", accompanied by a chart from a few-years-old paper.  The chart was a scatter plot with something like male share of unpaid domestic chores on the horizontal axis and total fertility rate on the vertical axis&mdash;each point was one of selected OECD countries at a point in time for which data on both variables was available&mdash;and there was a definite positive correlation. 

I can't find the chart now, but it looked something very similar to this one I've made for mysel:

<object type="image/svg+xml" data='/img/0290-highinc-scatter.svg' width='100%'><img src='/img/0290-highinc-scatter.png' width='100%'></object>

... except I think the original had a a stronger positive relationship between the two variables.

The case was being made that if you think the world isn't having enough children (not something I personally subscribe to but let's accept it as a problem for some people), the answer might be more feminism and gender equality, not less. And the obvious context being the various pro-traditionalism, trad-wife, etc arguments to the opposite effect, going around in the altogether rather contemptible (again, obviously just my own view) pro-natalism discourse.

> Might as well get on the record, while it's not relevant statistically, that I'm fully for more feminism and gender equality, and I also think "and then women will have more children" is a bad argument for it.

Unfortunately both the Bluesky post I saw and the original article have now escaped me, but I do remember that the data was a bit old (2010s), and some people commenting 'ah, woke Scandinavian country X where men do lots of housework, but since the time in this chart *they* have stopped having as many children too'. More importantly, I was intrigued by the use of "selected countries" in the title. Selected how and why, I wondered at the time. 

Obviously, limiting the analysis to rich countries gives a narrow view on a bigger relationship. Because one of the strongest empirical relationships in demography, on a historical scale, is the observation that as women and girls get more educational and economic opportunities they tend to have less children, in terms of a society-wide average of a country going through economic development. 

> I'm old enough to remember when everyone I engaged with seemed to agree this was a good thing, both in terms of the extra opportunities and choices for women as a good in itself, and avoiding cramming too many people into an already crowded and under-resourced planet. Apparently this is no longer a consensus, which just leaves me, I don't know, stroking my grey beard and feeling the world's passed me by.

I would expect, world-wide, that women do a higher share of the housework in countries where they have less economic opportunities (would you call these more patriarchal and 'traditional' societies? somewhat difficult to get a non-offensive terminology here).  In fact, what I'd expect is a diagram of causes and effects that looks something like this:

<object type="image/svg+xml" data='/img/0290-dg.svg' width='100%'><img src='/img/0290-dg.png' width='100%'></object>

In this model, economic and education opportunities for women and girls leads to choices to have less children and a decrease in total fertility rate, shown with a red arrow because of the downwards impact. Men doing more housework as a result of a rising culture of gender equality and changing social norms has an impact (probably smaller) in the opposite direction, with a blue arrow. That culture of gender equality itself comes about partly from changing economic conditions (women moving in to visible roles) and partly advocacy. Naturally, this is a gross over-simplification of the reality of these processes.

This isn't a directed acyclic graph (DAG) because it's not acyclic - some of the arrows are two-way, such as economic growth leading to more economic and educational opportunities for women and girls, and economic and educational opportunities for women  and girls leading to economic growth. But you could reduce it to a DAG if you limited it to the three key variables of total fertility rate, men doing housework, and opportunities for women and girls.

<object type="image/svg+xml" data='/img/0290-dg-simplified.svg' width='100%'><img src='/img/0290-dg-simplified.png' width='100%'></object>

This simplified version doesn't make it clear where increased opportunities for women and girls come from or why they lead to men doing more of the housework; the original diagram shows that this was expected to happen via the (difficult to observe and complex to evolve) mediating factor of a general culture of gender equality. 

The simplified diagram helps us think through what would happen if we ignore the confounder of "economic and educational opportunities for women and girls" and just plot male share of unpaid domestic chores against total fertility rate. 

* On a simple two-variable scatter plot, we'd expect a negative correlation, because the time use variable is actually standing in as a proxy for the more important gender equality of opportunities. 
* But if you could get a better indicator of that confounding opportunities variable and control for it, and if there really is an impact from male share of housework on higher fertility decisions, you *might* get a positive effect of male share of domestic work on fertility.


## ... All others must bring data

### First look
OK then let's look at some data. Sustainable Development Goals (SDG) Indicator 5.4.1 is ["the Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%)"](https://metadata.un.org/sdg/SL_DOM_TSPD?lang=en), which is fantastic because it means we have an internationally agreed standard on how this is measured, and what data is available will be in the United Nations Statistical Division's definitive database of the SDG indicators. Data won't be available for all countries, and certainly not for all years in all countries, because it depends on a difficult and expensive time use survey. Very few countries can afford to prioritise one of these regularly and frequently, and many have never had one at all.

For the vertical axis of our first plot, we can get total fertility rate from various sources, but one convenient one that gives an estimate for each country for each year on a standardised, comparable basis is the UN's World Population Prospects.

We have several challenges in using this data:
* The official SDG indicators don't actually include an obvious single dimensional summary of gender share of housework, so we will need to construct it with something like `male_share = male / (male + female)`. Where `male` is the proportion of men's time spent on dometic chres and carework, `female` the equivalent for women. We can make a composite indicator like this because the denominator (total time in the day) for both `male` and `female` is the same.
* Some countries have multiple observations (more than one year with a time use survey) and we'd like to incorporate them somehow. This implies some kind of multilevel model with a country-level random effect as well as residual randomness at the country-year level. On a chart, we can show these multiple observations by connecting points with segments, and visually differentiating the most recent observation from those in earlier surveys. This is much better than just picking one survey per country.
* The years of time use surveys vary significantly over a 20+ year time period, so we should expect a possible time effect to complicate any inference we do. We need to take this into account both in our statistical modelling and our visualisations.
* Not all the age groups are equivalent across countries, so we will have to grit our teeth for some inconsistent definitions of women and men (i.e. when does adulthood start).

My first intent is to get this chart that tries to at least represent the full range of these issues and imply a modelling strategy that would take them into account. So I drew a scatter plot, of all countries, of our two variables
<object type="image/svg+xml" data='/img/0290-simple-scatter.svg' width='100%'><img src='/img/0290-simple-scatter.png' width='100%'></object>

In stark contrast to the plot of just high-icome countres, there's a strongish negative relationship here. This is what I expected and is consistent with my thinking about economic and educational opportunities for women and girls being an important confounding variable as soon as we look at a broader range of countries. 

What about if we introduce some other variables, proxies for the economic opportunities for women and girls? Obvious candidates are income or, failing that,  GDP per capita, appropriately controlled for purchasing power parity in each country and point of time; and some general female empowerment index like relative literacy (say female literacy divided by male literacy, at age 15). 

What I'm after here is drawing some charts like this which will get us started in seeing if the apparent relationship between male share of domestic chores and fertility rate is really an artefact of confounding variables like overall economic development. 

<object type="image/svg+xml" data='/img/0290-facet-scatter.svg' width='100%'><img src='/img/0290-facet-scatter.png' width='100%'></object>

Here we do see, for example, a very interesting result that within the three lower income categories of countries there is a negative relationship between male share of domestic chores and fertility, but in the highest income category that relationship is reversed. In fact, the (lost) scatter plot that started me on this whole journey was basically the bottom right facet of this diagram.

We need to do more though - we can get a better measure of female economic empowerment (and hence choices between motherhood and employment). The best data I could find for my purpose on this was the Gender Inequality Index produced by the UNDP as part of their annual Human Development Report process. Here's what that number looks like for the countries that we have enough data for this overall blog:

<object type="image/svg+xml" data='/img/0290-gii-lollipop.svg' width='100%'><img src='/img/0290-gii-lollipop.png' width='100%'></object>


## Statistical modelling

### Challenges
Right, now I'm ready for a statistical model to try to disentangle these effects, and here I have to say I got into quite a muddle. One thing I was certain of was that I needed a mixed effects model with a random effect for country - when we have repeated observations (because multiple time use surveys) for some countries we want to use that information, but not treat each observation as independent and equally valuable. 

I wanted to also take into account the possibility of some kind of complex interaction between income and male share of unpaid domestic work - as the faceted scatter plot earlier shows, perhaps more male housework means less children in poor countries when this is a proxy for female empowerment, but in wealthier countries the effect reverses. Or perhaps this is just the confounding effect in action.

I also was fretting about the different time at which the observations were happening - is there a global trend in this whole thing (I think probably yes) that we need to let soak up some of the variance which otherwise might be falsely attributed to systematic variation in when the time use surveys happened. And I had the usual concerns with what if any transformations of continuous variables to use, and if I should allow flexible non-linear splines.

On top of all this, we don't have any where near as much data as I'd like - only about 180 observations in total, for about 80 countries. Many of the countries have only a single observation, which is certainly going to make it harder to estimate different levels of random effects (for those countries, the country-level randomness cannot be distinguished from country-time randomness).

To start with, here is a straightforward pairs plot of the main variables, most of them having been log-transformed:

<object type="image/svg+xml" data='/img/0290-pairs.svg' width='100%'><img src='/img/0290-pairs.png' width='100%'></object>


<object type="image/svg+xml" data='/img/0290-final-preds-propmale.svg' width='100%'><img src='/img/0290-final-preds-propmale.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0290-final-preds-gii.svg' width='100%'><img src='/img/0290-final-preds-gii.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0290-final-preds-time.svg' width='100%'><img src='/img/0290-final-preds-time.png' width='100%'></object>


<object type="image/svg+xml" data='/img/0290-final-preds-gdp.svg' width='100%'><img src='/img/0290-final-preds-gdp.png' width='100%'></object>
