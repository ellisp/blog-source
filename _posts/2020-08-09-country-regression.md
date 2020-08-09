---
layout: post
title: Essentially random isn't the same as actually random
date: 2020-08-09
tag: 
   - Health
   - ModellingStrategy
   - Visualisation
description: An observational study claiming to be an RCT might have something to say but there are far too many discretionary researcher choices taken to believe its findings. But I use this as a chance to play with statistical inference post estimating a regression via lasso.
image: /img/0191-gdp-adjusted.svg
socialimage: http://freerangestats.info/img/0191-gdp-adjusted.png
category: R
---

So an article/website is doing the rounds describing some country-level observational analysis of the relationship between hydroxychloroquine usage in the early stages of Covid-19 and deaths from that disease per million population. It purports to show a huge positive impact - reducing the chance of death by about 80% (with a relative risk ratio of about 0.2). 

My post that follows is long, so here's the spoiler - the effect is probably an artefact of some combination of failing to include the right confounders, a poor choice of response variable, choices taken by the researchers to eliminate 90% of the potential data points, and the process of classifying countries by hydroxychloroquine usage. Better statistical methods for such an observational study and small sample size return **a confidence interval for the actual risk ratio of anywhere between 0.25 and 1.0** (where 1.0 means no effect), even if we concede the categorisation of countries' usage is accurate.

## Linguistic kerfuffle over what is an RCT

The website claims that the natural experiment of different countries' policy and regulatory decisions around hydroxychloroquine is actually a randomised control trial (RCT). This is clearly absurd, and an attempt to seize the prestige of RCTs for a particular position. Here's one typically (and justifiably) annoyed [Twitter thread](https://twitter.com/CT_Bergstrom/status/1291837801512595457) on the subject.

I imagine some of the hydroxychloroquine proponents have identified they have been losing the debate recently, because of RCTs failing to find evidence in support of hydroxycholoroquine's benefits (whereas a few weak observational studies *had* found some promising effects). So perhaps this is an attempted jujitsu move to use their critics' strength against themselves. If successful this not only lets some of the prestige of RCTs rub off on their own arguments (they have already gotten coverage on Fox News), it significantly takes the edge off future criticism based on real RCTs. 

This last angle reflects exactly what happened to the analytic category of 'Fake News' - remember this term first came to prominence to categorise overwhelmingly pro-Trump malicious fictional articles in the 2016 US election (eg "Pope endorses Trump"). Now Trump has made the term his own to refer to news he dislikes in the mainstream media, and it is nearly impossible to use in its original context. Let's hope this doesn't happen to RCTs.

That misuse of RCT terminology infuriates me and many others and has been the focus of criticism on Twitter. However, putting aside its propaganda impacts described above, it's essentially a linguistic definitional quibble from a methodological point of view. In fact, there's nothing wrong with cross country comparisons of outcomes, if we put aside what we call them, and if we use the right techniques. On an autobiographical note, wanting to better understand the strengths and limitations of World Bank cross-country regressions was the driver for me studying statistics back in my overseas aid days.

Further, I've no fondness for scientific gate-keeping; I put zero weight on the fact that this 'study' isn't peer reviewed, and might be written by someone without the 'right' qualifications. Pre-publication peer review simply doesn't work, and criticisms should be of the substance of an argument not the author.

## More substantive statistical issues

So, I wanted to see if there were problems with this analysis *beyond* the misuse of RCT terminology for propaganda purposes. Here's what I thought might be going on here, with the ones I think are most likely first:

1. Researcher discretionary choices in the garden of forking paths
2. A confounding factor that correlates with both countries' hydroxychloroquine policies and their Covid-19 death rates
3. Random chance
4. A genuine effect

These aren't mutually exclusive; in fact I think it is quite likely that the first three all apply and possible that all four do. Reasons 1 and 2 are why we can't take any p values at their face value - inference is conditional on these problems not existing (this doesn't mean I am against p values, I am a bit supporter - just warning to be cautious in interpreting them). Reason 3 is basically the bad luck of every now and then one expects to find a false positive. Reason 4 reflects my observation that at least some clear-minded experts (not just full on partisan advocates) concede it is still possible there is some very small positive impact of using HCQ in the early stages of Covid-19 or as a prophylactic, awaiting the right RCT (not this one!) to find it. 

There has to be *something* going on because the claim made in the original website is simply not plausible - around an 80% drop in the probability of dying if you are in a country that uses hydroxychloroquine in the early stages of Covid-19. We know this isn't possible: 

1. because such a huge effect would have shown up in the individual level trials to date even if one accepts they were flawed with regard to doses and timing as claimed by hydroxychloroquine's proponents; and 
2. it simply isn't biologically plausible that a single drug could have such a huge impact, given what we know of the l[ikely mechansim](https://twitter.com/jpogue1/status/1288871272261328902).

The sample size for this study is a very thin 19 countries. It is worth stressing up front that the sample size here is the *19* countries, not their billions of inhabitants, because the variation we have in our data is all at the country level, not individual.

Lets look at the first two of my four potential explanations in a bit of detail.

## 1. Research discretion in the garden of forking paths

Many of my readers will be familiar with this piece by Andrew Gelman and Eric Loken on [The garden of forking paths: Why multiple comparisons can be a problem, even when there is no “fishing expedition” or “p-hacking” and the research hypothesis was posited ahead of time∗](http://www.stat.columbia.edu/~gelman/research/unpublished/p_hacking.pdf). The essence of the argument is that consciously or not, researchers take a range of discretionary decisions in their analysis process that have big impacts for inference.

Here are the key decisions that the researchers made here that might have an impact:

The **allocation of countries** into the "widespread", "mixed" or "limited" use of HCQ in early stages of Covid-19. This is a huge driver of the analysis and obviously of critical importance. However, the classification is done in a way that seems to leave a fair bit of room for judgement if not error. See [this Twitter thread for a better-informed discussion than I could give](https://twitter.com/GidMK/status/1292305985868935168).

The choice of **deaths per population as the response variable rather than deaths per case**. The rationale for this is that deaths are more reliably measured; I think this is fairly generally conceded. However, it's a major limitation with the study because the claimed effect of HCQ is on patients who receive the treatment early in the course of the disease. The better measure here would be deaths per case. There are simply too many steps in the process population -> cases -> deaths that might be down to things other than HCQ. 

Crucially, given the study is about the impact of a *treatment*, surely it is fair to assume that cases that are potentially exposed to the treatment are counted in the official statistics? While it is almost certainly correct that many cases are undiagnosed and reported, particularly in poorer countries, surely these are also people who were not given HCQ in the early stages. 

As it happens, the relationship between deaths per case and deaths per population is fairly straightforward as seen in the next plot. Of course, simple arithmetic shows that the slope of the line in this scatter plot is effectively the cases per population. Countries such as Sweden, USA, Chile and Brazil that are above the trend are there because, while their deaths per case are unremarkable, their cases per population (and hence deaths per population) are high.

<object type="image/svg+xml" data='/img/0191-per-case-per-pop.svg' width='100%'><img src='/img/0191-per-case-per-pop.png' width='100%'></object>

If we plug the better response variable into the classical model that is probably similar to that of the original authors, the apparent HCQ effect persists but is weaker. In trying to replicate their results, I can get an estimated point estimate of the relative risk ratio with the model that is closest of 0.12 and confidence interval of (0.05, 0.25) - similar to the unadjusted 0.13 reported in the original website. If I change the response variable to deaths per case my new point estimate is 0.22 and confidence interval of (0.10, 0.43). This is still a very big effect (too big to be plausible), but wider, and further from zero.

I am very confident that deaths per case is a better response variable than deaths per population for what is being measured here, even with reporting problems with case, and this is an important example of a researcher decision impacting on the result.

**33 countries with population smaller than 1 million were excluded**. This seems a mistake. A *much* better solution would have been to include them, and to weight the analysis by population (if deaths per population stays the response variable) or cases (if it changes to deaths per case, as it should).

**29 countries with early adoption of masks were excluded**. This seems odd, and on the face of it suspicious. On similar logic to the discussion of why deaths per case is the better measure, it is worth noting that masks are meant to stop cases, not progression of the disease. If the better response variable of deaths per case had been chosen, the people who never get infected are dropped out of both the numerator and denominator of the response, and we could have included the sample size substantially.

**51 countries with very few people aged over 80 were excluded**. This doesn't make sense, given the authors are adjusting the death count for age factor.
 
**14 countries with very little spread to date of Covid-19 were excluded**. This decision is understandable, although again a better approach would have been to include these countries, use deaths per cases as response, and weight the data by number of cases.

As can be seen, many the above challenges follow from the decision to use deaths per population as the response variable rather than deaths per case. This cascade of decisions reduces the sample size from 176 possible countries to 19. 

The other big use of researcher discretion here was in **choice of variables** to include as controls. That is the focus for my second likely explanation of the problems in the study.

## 2. Confounding factors

The second thing I noticed after reading the original paper was the absence of any control for economic, political and institutional variables (the first, like everyone else, was the misuse of the 'RCT' terminology).  Surely, whatever is causing different performance in number of deaths from Covid-19, diversity in these variables is more important than use of this one drug.  

Here's a chart that the researchers didn't include but should have:

<object type="image/svg+xml" data='/img/0191-gdp-adjusted.svg' width='100%'><img src='/img/0191-gdp-adjusted.png' width='100%'></object>

We immediately see that the limited users of hydroxychloroquine, other than Mexico, are all relatively high income countries. This also draws attention to just how much these countries have in common, other than their HCQ policies. It's not obvious why higher incomes and a common cultural background might be leading to higher death rates, but it *is* obvious that there are a bunch of variables here that aren't included in the model but should be. This chart alone is the compelling proof that "essentially random" (as one defender of this paper described it in the Twitter Wars) is *not* the same as "actually random".

For completion, here's the same chart but with case fatality rate (deaths per case) on the vertical axis rather than deaths per population. We see the relationships are a little weaker all round - between GDP and case fatality rate, and between reported HCQ usage and case fatality rate. For example, USA drops vertically and Indonesia rises, weakening both relationships, and in a way that I think is more accurate a picture answering the core research question here.

<object type="image/svg+xml" data='/img/0191-gdp-per-case.svg' width='100%'><img src='/img/0191-gdp-pre-case.png' width='100%'></object>


This also made me think about another obvious confounder - from my casual observation, I thought some of those poor, widespread-HCQ using countries were fairly early in their outbreaks. So I added a "how long since the outbreak got really serious" variable, based on the number of days since the country passed its first death per million. This shows some slight relationship with total deaths per million (as of course it would) and with the treatment variable, so it's another confounding variable we should control for in any serious analysis,

<object type="image/svg+xml" data='/img/0191-time-since-serious.svg' width='100%'><img src='/img/0191-time-since-serious.png' width='100%'></object>

As it happens, the hydroxychloroquine effect is robust to controlling for these confounders. Using slightly different methods for adjusting for the confounders that the authors *had* identified (eg diabetes and hypertension prevalence, stringency of other measures, etc), and using a variety of fairly basic traditional methods, I get a relative risk ratio for the impact of "widespread" use of early hydroxychloroquine of around 0.1 or 0.2 in my various models whether or not I am controlling for GDP per capita and for the days since the country passed one death per million. I used straightforward generalized linear models for this (trying both quasibinomial and quasipoisson responses, both with logarithm link functions). I didn't think it worth while trying propensity score matching or any two step estimation process because I don't have a plausible mental model for what is behind the decision of countries to adopt the 'treatment' in this case.

See later in this post for the results of more sophisticated modelling that takes better account of uncertainty, but again the two obvious confounders that I thought of and had easy access to data for did not make a difference.

I remain pretty confident that there is a problem of some important confounding variables here, even if I (and perhaps no-one) knows exactly what they are. In a sense, this is the bottom line problem with this analysis - because there wasn't any genuine random assignment, we're never going to know what actually is driving the countries' decision to use or not hydroxychloroquine, and what other variables that usage statistic is standing as a proxy for.


## Chance

So, how would one do this analysis? First by acknowledging that we only have 19 data points, not the billions claimed in the original website. Because we don't have access to the individual level variation, we have to treat the country level as the source of variance here. We don't have many degrees of freedom to use, yet we have a bunch of variables we want to control for.

I think this is a difficult problem, and one that's probably not worth solving fully - 19 observationsis simply too few to draw firm conclusions, so the best we can hope for is some exploratory hypotheses to follow up with real (individual level) RCTs. But it's worth investigating the data a *bit* with modern methods. With all these candidate variables, how can we judge which ones are important, making the most efficient use of the data we have?

I've been reading [Statistical Learning with Sparsity](https://web.stanford.edu/~hastie/StatLearnSparsity/) by legends Hastie, Tibshirani, Wainwright which prompted me to use the lasso as a feature selection method in this toy example (toy because the sample is so small, the causality so complex and the expected effect size so small compared to other variables no serious conclusions could ever be drawn).

So I used a lasso with penalty factor chosen by cross-validation for best predictive value. This lets us  with a bootstrap to ....




There really is a positive benefit for use of hydroxychloroquine when used at the right time, and the (actual) RCTs to date have not correctly investigated its use at the right dose at the right time


{% highlight R lineanchors %}


{% endhighlight %}

## Other supplementary material



