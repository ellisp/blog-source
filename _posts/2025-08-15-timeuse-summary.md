---
layout: post
title: Men's domestic chores and fertility rates - Part I
date: 2025-08-15
tag: 
   - ModellingStrategy
   - DataFromTheWeb
description: The time that men spend on domestic chores is positively related to total fertility rate. But only in rich countries. Overall, it's negatively related. And if you model it with both income and gender inequality (generally, more country-level gender inequality means more children), the effect goes away altogether.
image: /img/0290-dg.svg
socialimage: https:/freerangestats.info/img/0290-dg.png
category: R
---

### <i>Author's introduction</i>
<i>
This post grew out of a rambling, sporadically multi-month play with a bunch of data that became far too big for a single post for any plausible audience. So I have broken that work into three artefacts that might be of interest in different ways to different audiences:
</i>

* <i>Men's domestic chores and fertility rates - (this document) - discussion of the substantive issues of the subject matter, with charts and results of statistical models but no code. Main audience is anyone interested in what statistics has to say about the actual (non) relationship of the time men spend on domestic chores and total fertility rate.</i>
* <i>Men's domestic chores and fertility rates - behind the scenes (coming within the next week or so) - discussion of technical issues such as how to draw directed graphs with different coloured edges, how to access the UN SDG indicators database, and the equivalence or not of different ways of fitting mixed effects models. Contains key extracts of code. The main audience is future-me wanting to remember these things, but also anyone else with similar technical interest.</i>
* <i>[The code that produces all the analysis and results](https://github.com/ellisp/blog-source/blob/master/_working/0290-house-work.R) in the two blog posts.</i>

<i>
OK, onto the blog post.
</i>

<hr>

## Time-use and number of children

### An apparent relationship in high income countries
Some months ago a post floated across my Bluesky feed making an argument to the effect of "if societies want more children, then men should do more of the housework", accompanied by a chart from a few-years-old paper.  The chart was a scatter plot with something like male share of unpaid domestic chores on the horizontal axis, and total fertility rate on the vertical axis&mdash;each point was one of selected OECD countries at a point in time for which data on both variables was available&mdash;and there was a definite positive correlation. 

I can't find the chart now, but it looked something very similar to this one I've made for myself:

<object type="image/svg+xml" data='/img/0290-highinc-scatter.svg' width='100%'><img src='/img/0290-highinc-scatter.png' width='100%'></object>

The case was being made that if you think the world isn't having enough children (not something I personally subscribe to but let's accept it as a problem for some people), the answer might be more feminism and gender equality, not less. And the obvious context being the various pro-traditionalism, trad-wife, etc arguments to the opposite effect, going around in the altogether rather contemptible (again, obviously this is just my own view) pro-natalism discourse.

> Might as well get on the record, while it's not relevant statistically, that I'm fully for more feminism and gender equality, and I *also* think "because then women will have more children" is a very bad argument for these things.

Unfortunately both the Bluesky post I saw and the original article have now escaped me, but I do remember that the data was a bit old (2010s), and some people commenting 'ah, woke Scandinavian country X where men do lots of housework, but since the time in this chart *they* have stopped having as many children too'. More importantly, I was intrigued by the use of "selected countries" in the title. Selected how and why, I wondered at the time. 

Obviously, limiting the analysis to rich countries gives a narrow view on a bigger relationship. Because one of the strongest empirical relationships in demography, on a historical scale, is the observation that as women and girls get more educational and economic opportunities, they tend to have less children, in terms of a society-wide average of a country going through economic development. 

> I'm old enough to remember when everyone I engaged with seemed to agree this was a good thing, both in terms of the extra opportunities and choices for women as a good in itself, and avoiding cramming too many people into an already crowded and under-resourced planet. Apparently this is no longer a consensus, which just leaves me, I don't know, stroking my grey beard and feeling the world's passed me by.

### What causes what?

I would expect, world-wide, that women do a higher share of the housework in countries where they have less economic opportunities (would you call these more patriarchal and 'traditional' societies? somewhat difficult to get a non-offensive terminology here). And that in those same countries, they also have more children (see widely known historical empirical fact referred to above). In fact, what I'd expect is a diagram of causes and effects that looks something like this:

<object type="image/svg+xml" data='/img/0290-dg.svg' width='100%'><img src='/img/0290-dg.png' width='100%'></object>

In this model, economic and education opportunities for women and girls leads to choices to have less children and a decrease in total fertility rate, shown with a red arrow because of the downwards impact. Men doing more housework as a result of a rising culture of gender equality and changing social norms has an impact (probably smaller) in the positive direction, with a blue arrow. That culture of gender equality itself comes about partly from changing economic conditions (women moving in to visible roles) and partly from successful advocacy. 

Naturally, this is a gross over-simplification of the reality of these processes.

The diagram above isn't a directed acyclic graph (DAG) because it's not acyclic - that is, some of the arrows are two-way, such as economic growth leading to more economic and educational opportunities for women and girls, and economic and educational opportunities for women  and girls leading to economic growth. But you could reduce it to a DAG if you limited it to the three key variables of total fertility rate, men doing housework, and opportunities for women and girls.

<object type="image/svg+xml" data='/img/0290-dg-simplified.svg' width='100%'><img src='/img/0290-dg-simplified.png' width='100%'></object>

This simplified version doesn't make it clear where increased opportunities for women and girls come from or why they lead to men doing more of the housework. The original, more complex, diagram shows that this was expected to happen via the (difficult to observe and complex to evolve) mediating factor of a general culture of gender equality. 

The simplified diagram *does* help us think through what to expect if we ignore the confounder of "economic and educational opportunities for women and girls" and just plot male share of unpaid domestic chores against total fertility rate.

* On a simple two-variable scatter plot, we'd expect a negative correlation, because the time use variable is actually standing in as a proxy for the more important gender equality of opportunities. 
* But if you could get a better indicator of that confounding opportunities variable and control for it, and if there really is an impact from male share of housework on higher fertility decisions, you *might* get a positive effect of male share of domestic work on fertility.


## ... All others must bring data

### Who measures this stuff?
OK then, let's look at some data. 

Sustainable Development Goals (SDG) Indicator 5.4.1 is ["the Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%)"](https://metadata.un.org/sdg/SL_DOM_TSPD?lang=en), which is fantastic because it means we have an internationally agreed standard on how this is measured. It also means that what data is available will be in the United Nations Statistical Division's definitive database of the SDG indicators. 

Data won't be available for all countries, and certainly not for all years in all countries, because it depends on a difficult and expensive time use survey. Very few countries can afford to prioritise one of these regularly and frequently, and many have never had one at all.

For the vertical axis of our first plot, we can get total fertility rate from various sources, but one convenient one that gives an estimate for each country for each year on a standardised, comparable basis is the UN's World Population Prospects.

We have several challenges in using this data:
* The official SDG indicators don't actually include an obvious single dimensional summary of gender share of housework, so we will need to construct it with something like `male_share = male / (male + female)`. Where `male` is the proportion of men's time spent on dometic chres and carework, `female` the equivalent for women. We can make a composite indicator like this because the denominator (total time in the day) for both `male` and `female` is the same.
* Some countries have multiple observations (more than one year with a time use survey) and we'd like to incorporate them somehow. When we get to statistical modelling, this implies the need for some kind of multilevel model with a country-level random effect as well as residual randomness at the country-year level. On a chart, we can show these multiple observations by connecting points with segments, and visually differentiating the most recent observation from those in earlier surveys. This is much better than just picking one survey per country.
* The years of time use surveys vary significantly over a 20+ year time period, so we should expect a possible time effect to complicate any inference we do. We need to take this into account both in our statistical modelling and our visualisations.
* Not all the age groups are equivalent across countries, so we will have to grit our teeth for some inconsistent definitions of women and men (i.e. when does adulthood start). Not least of the implications of this is it adds an annoying data processing step.

### A relationship reversed

Once I had the data in place, I started with a scatter plot, of all countries, of our two variables.

<object type="image/svg+xml" data='/img/0290-simple-scatter.svg' width='100%'><img src='/img/0290-simple-scatter.png' width='100%'></object>

In stark contrast to the plot of just high-income countries that started me off, there's a strongish *negative* relationship here. The direction of the relationship has reversed! This is what I expected and is consistent with my thinking about economic and educational opportunities for women and girls being an important confounding variable as soon as we look at a broader range of countries. 

What about if we introduce some other variables, proxies for the economic opportunities for women and girls? Obvious candidates are income or, failing that,  GDP per capita, appropriately controlled for purchasing power parity in each country and point of time; and some general female empowerment index like relative literacy (say female literacy divided by male literacy, at age 15). 

What I'm after here is drawing some charts like this which will get us started in seeing if the apparent relationship between male share of domestic chores and fertility rate is really an artefact of confounding variables like overall economic development. 

<object type="image/svg+xml" data='/img/0290-facet-scatter.svg' width='100%'><img src='/img/0290-facet-scatter.png' width='100%'></object>

Here we do see, for example, a very interesting result that within the three lower income categories of countries there is a negative relationship between male share of domestic chores and fertility. But in the highest income category, that relationship is reversed. In fact, the (lost) scatter plot that started me on this whole journey was basically the bottom right facet of this diagram.

### Measuring gender inequality

We need to do more though - we can get a measure of female economic empowerment (and hence choices between motherhood and employment). The best data I could find for my purpose on this was the [Gender Inequality Index](https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII) produced by the UNDP as part of their annual Human Development Report process. Here's what that number looks like for the countries that we have enough data for this overall blog:

<object type="image/svg+xml" data='/img/0290-gii-lollipop.svg' width='100%'><img src='/img/0290-gii-lollipop.png' width='100%'></object>

Finally in this exploratory stage, here is a plot of all the pairwise relationships between the variables we've been discussing:

<object type="image/svg+xml" data='/img/0290-pairs.svg' width='100%'><img src='/img/0290-pairs.png' width='100%'></object>

There's a lot packed in to plots like these, but what we see here is that:

* GDP per capita is strongly negatively correlated with fertility rate (rich countries have less children). 
* Gender inequality is strongly positively correlated with fertility rate (unequal countries have more children).
* Male housework is moderately positively correlated with GDP per capita (rich countries have more male housework).
* Male housework is weakly to moderately negatively correlated with fertility (more male housework countries have less children).
* Each variable has a weak trend over time - downwards for fertility rate and gender inequality, upwards for GDP per capita and male housework. You can actually see in the left column of the plots the chains of dots representing countries like the USA that have the luxury of multiple time-use surveys and a lovely continuous series of comparable observations.

## Statistical modelling

The type of model I want to fit is one that has all these features:

* allows us to include multiple measures for countries that have them, but without making the false assumption that these are independent observations (each extra observation on a country is useful, but not as much extra information as if we had a whole new country)
* allows for an interaction between income and male housework
* allows relationships in general to be non-linear if that's what the data suggests
* allows for a nuisance non-linear trend over time in fertility
* lets the variance of total fertility rate be proportional to its mean, but not identical (so a quasi-poisson family distribution)

To do this I opted to use the `gam` function from Simon Wood's `mgcv` package, fit with this snippet of code:

{% highlight R lineanchors %}
model6b <- gam(tfr ~ s(time_period) + 
                     s(gii, k = 3) + 
                     s(log(gdprppppc), prop_male) + 
                     s(country_fac, bs = 're'), 
                data = model_ready, family = quasipoisson, method = "REML")
{% endhighlight %}

The forthcoming "behind the scenes" follow-up post will have more discussion of some of the modelling choices, diagnoses, and statistical tests.

The end result is that this model is *not* an improvement on a model that drops `prop_male`&mdash;ie the proportion of domestic work that is done by men&mdash;altogether. As seen in this Analysis of Deviance table, with virtually no extra deviance in fertility explained by the more complex model:

```
Analysis of Deviance Table

Model 1: tfr ~ s(time_period) + s(gii, k = 3) + s(log(gdprppppc)) + s(country_fac, 
    bs = "re")
Model 2: tfr ~ s(time_period) + s(gii, k = 3) + s(log(gdprppppc), prop_male) + 
    s(country_fac, bs = "re")
  Resid. Df Resid. Dev     Df Deviance      F Pr(>F)
1    82.463     1.0585                              
2    77.330     1.0154 5.1326 0.043118 0.7491 0.5925
```

This isn't surprising when we reflect on the pairs plot earlier. GDP per capita and the gender inequality index both have strong, obvious relationships with total fertility rate. It makes sense that between them they soak up all the variance that can be explained at the country level.


To see the modelling results visually, here is a plot showing predictions of the average level of fertility rate at varying levels of that male housework variable, created with the incredibly useful `marginaleffects` package by Vincent Arel-Bundock, Noah Greifer and Andrew Heiss. What we see here is no material relationship:

<object type="image/svg+xml" data='/img/0290-final-preds-propmale.svg' width='100%'><img src='/img/0290-final-preds-propmale.png' width='100%'></object>

Contrast that to comparable presentation of the results for gender inequality, and for income per capita:

<object type="image/svg+xml" data='/img/0290-final-preds-gii.svg' width='100%'><img src='/img/0290-final-preds-gii.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0290-final-preds-gdp.svg' width='100%'><img src='/img/0290-final-preds-gdp.png' width='100%'></object>

The time relationship is an interesting one. It looks from the plot below that there is no material relationship, but the statistical evidence is pretty strong that it is worth keeping this variable in the model.

<object type="image/svg+xml" data='/img/0290-final-preds-time.svg' width='100%'><img src='/img/0290-final-preds-time.png' width='100%'></object>

My intuitive explanation for this is that time is more important in explaining trends in fertility rate in the countries that have multiple observations in this sample; and this is not easy to pick up visually in a chart of this sort. Anyway, it doesn't matter, as I'm not interested in the time trend in its own right, just in controlling for it as a possible spoiler of our more important statistical conclusions.

## Conclusions

* If you look at just high income countries, there's an apparent positive relationship between the amount of unpaid domestic chores done by men and total fertility rate, at the country level.
* However, this impact is *reversed* if you look at the full range of countries for which data is available.
* Most importantly, the relationship *vanishes altogether* when we include it in a statistical model that controls for purchasing power parity GDP per capita and for gender inequality more broadly.
* We can conclude that <b>the apparent effect of male housework on total fertility is just a statistical artefact</b> standing in for these two, broader&mdash;and obviously important&mdash;factors.
