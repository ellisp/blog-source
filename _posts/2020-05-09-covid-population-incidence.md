---
layout: post
title: Test-positivity rates and actual incidence and growth of diseases
date: 2020-05-09
tag: 
   - ModellingStrategy
   - Health
   - R
description: I look at several different ways of accounting for the information given us by high positive testing rates for COVID-19 and look at the impact on estimates of effective reproduction number at a point in time
image: /img/0178-smoothed-test-rates.svg
socialimage: http://freerangestats.info/img/0178-smoothed-test-rates.svg
category: R
---

## Test positivity rates and disease incidence

OK, lets talk about test positivity rates ie the proportion of administered tests that conclude the subject has the disease. These are in the news with COVID-19 test positivity being used as an informal indicator of COVID-19 incidence and the quality of the confirmed case data, for example in [Nate Silver's daily commentaries](https://twitter.com/NateSilver538/status/1258870023545860101). There's a good [recent piece in The Atlantic](https://www.theatlantic.com/technology/archive/2020/04/us-coronavirus-outbreak-out-control-test-positivity-rate/610132/) on why high test positivity rates are a particular problem for understanding incidence in the US where tests have been scarce. With a lack of random samples from populations, and very partial coverage of the symptom-based testing, it's really hard to estimate population incidence, prevalence or even its growth and [effective reproduction number](https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/epidemic-theory). 

> Terminology point - incidence is the rate of new (or newly diagnosed) cases of a disease, and prevalence is the total cases at a point in time. Obviously the difficulties in measuring either are closely related.

We know from other situations, particularly malaria in developing countries with limited health surveillance capabilities, that test positivity rate itself can be used as an indicator of incidence. In fact, it's one of the [official metrics](https://www.who.int/data/gho/indicator-metadata-registry/imr-details/3151) from the World Health Organisation for monitoring malaria. The rationale is that there is an ongoing background rate of people presenting for tests with fever symptoms, and the proportion of these people who turn out to have malaria is an imperfect but useful indicator of the overall prevalence and incidence. If it gets low enough, you can even start thinking about elimination.

There's a small recent academic literature on the relationship between malaria test positivity rates and actual incidence. For example, a nice 2016 piece by Boyce and others in open access PLoS One about [Practical Implications of the Non-Linear Relationship between the Test Positivity Rate and Malaria Incidence](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4809590/). Because the data is included in a supplementary table (albeit as a 265 page long PDF!) I was able to reproduce and enhance one of their key charts:

<object type="image/svg+xml" data='/img/0178-malaria.svg' width='100%'><img src='/img/0178-malaria.png' width='100%'></object>

*All R code is at the bottom of the post today. Any sources that aren't clear from the text and graphics are explicit in the code.*

One implication of this is that if tests are the only indicator you have of a disease's incidence, if the number of positives in the tests *double* but the number of tests hasn't doubled, it is probable that the number of actual infections has *more than doubled*. This is relevant for how we interpret the reported COVID-19 cases. In fact, it just confirms the common sense that higher test positivity rates probably mean not just that we've got more cases from those observed in the test, but we're more likely to be missing cases through rationing or other reasons of tests not being available. Bottom line - if you were able to do more tests, you'd find more cases.

## Could we adjust incidence estimates based on test positivity rates?

In late March and early April of this year, fully 50% of tests in New Jersey and New York were positive, according to data from covidtracking.com:

<object type="image/svg+xml" data='/img/0178-smoothed-test-rates.svg' width='100%'><img src='/img/0178-smoothed-test-rates.png' width='100%'></object>

The statistical conundrum here can be thought of in a simple numeric example. Suppose we have two locations, A and B. Both have 10 confirmed cases, but for A these are based on 20 tests (test-positivity of 50%) and in B they are based on 1,000 tests (1%, approximately the rate [here in Australia](https://www.health.gov.au/news/health-alerts/novel-coronavirus-2019-ncov-health-alert/coronavirus-covid-19-current-situation-and-case-numbers) at the time of writing). We can imagine several unrealistic extreme approaches in how we treat the inference from these confirmed case numbers to the number of people in the population with the disease:

<ol type = "A">
<li>We can accept them on face value and say each location has 10 cases.</li>
<li>We can multiply them both by some common factor ("10" is commonly thrown around) to represent undiagnosed cases (whether these are unsymptomatic, not-yet-symptomatic, or symptomatic and can't get a test) and say each location has 100 cases.</li>
<li>We can treat the tests as a sample from the population and multiply the cases in A by 50 to bring its cases up to "what they would be if they'd done as many tests as B", giving us 500 cases in A and 10 in B. In situations with more diverse numbers, this can be generalised by multiplying confirmed cases by the test-positivity rate and also by some arbitrary constant that is at least as large as the inverse of the lowest test-positivity of any location.</li>
</ol>

I should stress at this point that as far as I am aware, no serious commentator proposes any of these methods to be satisfactory. However, the revealed preference is either to use method A and simply write a caveat ("only confirmed cases are shown"), or method B as a crude "this is the best adjustment for now" pragmatic alternative.

Method C has more intuitive appeal to me than either of the others. For example, it seems extraordinary to suggest that when New York had a test-positivity rate of 50% that there weren't many, many more people who would have been confirmed if tests were as widely available per sick person as is the case in Australia today. However, method C has problems too. In particular, there is an implicit assumption that increasing the testing 'sample' would see a steady test-positivity rate; whereas surely as more people were tested, the testing would be extended eventually to marginal cases and the rate of positives would start to decline.

All of this is a consequence of the difficulties of inference to an unseen population without a random sample, or at least a sample generated by a known process. Because the selection of the sample in this case is inextricably tied up with the variable we want to measure, we would need a very very good model indeed of the testing process to be able to meaningfully draw inferences from confirmed cases to population prevalence.

For some purposes - including forecasting so long as total cases are well below herd immunity levels (which they certainly are) - growth rates are more important than the absolute level. If we had to choose between methods A, B and C in converting our confirmed cases to population estimates, what is the impact on estimates of the effective reproduction number (R) at a point in time? The excellent [Oz COVID 19 Visualisations](https://cbdrh.github.io/ozcoviz/) site (a joint effort of researchers at four different universities in New South Wales and Victoria and built as an RMarkdown dashboard) estimates effective R for NSW with both methods A (reported case numbers) and B (multiplying by 10), observing that it makes very little difference. But what about my proposed method C, which would inflate case numbers at times of less testing, relative to times of plentiful testing?

We can actually generalise all three methods into a model

$$y = mxq^k\epsilon $$

where

- `y` is the population prevalence
- `x` is the confirmed cases
- `m` is an unknown constant multiplier of `x`
- `q` is the test-positivity rate
- `k` is an unknown constant exponent of `q`, between zero and one.
- \\(\epsilon\\) is a random error term of unknown distribution

When `k` is zero we get method B - just multiply the confirmed cases by a constant to get your estimated population cases. If `m` is one and `k` is zero, then you are right down to method A. 

On the other hand, when `k` is 1 we get method C, ie assuming the number of confirmed cases would increase poportionately if tests were hypothetically increased. 

There's something nice about this generalisation - which often happens when we can identify a general equation that are current methods are all special cases of - which is that a new alternative emerges. Any value of `k` between the extremes of zero and one is a nice compromise between the two. This compromise concedes the commonsense notion that the higher the test-positivity, the more cases this suggests are out there, without going to the extreme of full-on method 3.

The problem of course is that we don't know what either `m` or `k` are, nor is there any obvious means of estimating them. To make things worse, there is no reason at all for them to be constant across different locations or even the same location at different times. They will be driven by factors such as availability and criteria for testing, amongst others.

If we had an independent estimate of population incidence we could do this, but we don't yet have such a measure of any reliability, certainly not a time series by location. Random samples for testing from the population is the way to get prevalence rates (and if you did it often enough you could estimate incidence too), but you need [large samples and tests with very low false-positive rates to do this reliably](https://www.statschat.org.nz/2020/04/25/why-new-york-is-different/) if the disease is rare.

One alternative method is to take the number of deaths "later" as an indicator of the number of actual cases now. Deaths from COVID-19 are more reliably counted than cases, although there is still [almost certainly a significant undercount](https://www.newscientist.com/article/mg24632804-100-how-many-people-have-really-died-from-covid-19-so-far/). There is also the problem that we don't know the infection fatality rate to any degree of precision (although expert opinion, not without dissent, is converging on a range of of around 0.5% to 1%). Anyway, it turns out we can get a face-plausible estimate of actual cases in New York by multiplying deaths seven days later by 100.

## Adjusting confirmed cases for test positivity using New York as an example

The chart below shows estimates of total cases in New York based on my three methods enumerated above, a "generalized" case where I use `k = 0.5`, and the "deaths seven days later multiplied by 100" methods.

<object type="image/svg+xml" data='/img/0178-diff-methods.svg' width='100%'><img src='/img/0178-diff-methods.png' width='100%'></object>

... and here is a smoothed version:

<object type="image/svg+xml" data='/img/0178-diff-methods-smoothed.svg' width='100%'><img src='/img/0178-diff-methods-smoothed.png' width='100%'></object>

I chose 7 days (rather than 14 or 4, for example) just by eyeballing variants of this chart. I chose values of `m` specifically so totals from all methods (other than simply "confirmed cases") delivered similar totals to the around 1.9 million total cases in New York delivered by the crude "multiply deaths by 100" method. Please note that I choose the word "crude" very deliberately here - I am not saying that the number of cases in New York is 1.9 million - I am just going to be using this as ballpark figure to compare the impact on analysis based on growth-rate of different methods of estimating population prevalence that give similar totals.

## Implications for growth rates and effective reproduction number

So what analysis do I mean in particular? One example of analysis we would be able to do if we knew the actual numbers of cases in the population is estimation of the effective reproduction number ie the average number of people infected by someone with the disease, taking into account any immunity or preventative measures. When the number of cases is small compared to the population these estimates depend on growth rates rather than absolute size, so arbitrary multipliers make little difference. But adjusting figures upwards when test-positivity is high (typically early in the outbreak) would lead to higher values of R earlier, and lower later, relative to methods that just take the confirmed cases as proportionate to the total cases.

So using the methods in Thompson et al's [Improved inference of time-varying reproduction numbers during infectious disease outbreaks](https://www.sciencedirect.com/science/article/pii/S1755436519300350?via%3Dihub) as implemented in the excellent EpiEstim R package, I estimated effective reproduction numbers based on each of my five sets of case numbers. These estimates draw on data on the distribution of the serial interval (ie the time between infections in a chain of infections) in [this article by Nishiura et al](https://www.medrxiv.org/content/10.1101/2020.02.03.20019497v2) and I have copied my code to do this almost exactly from the [Oz COVID 19 Visualizations' source code](https://github.com/CBDRH/ozcoviz/blob/master/nsw_eff_R_data_and_plot_prep.R) for similar estimates for NSW. Here's the result:

<object type="image/svg+xml" data='/img/0178-r-methods.svg' width='100%'><img src='/img/0178-r-methods.png' width='100%'></object>

Pleasingly, my methods for correcting for test-positivity rate (the bottom row of two plots) make a small improvement to the shape of the effective reproduction number over time, although the improvement isn't enough to make the first few weeks of estimates realistic. Basically, there just wasn't enough testing (less than 50 per day being reported) for reliable estimates at that point, and I doubt any statistical wizadry could help much.

By 'improvement' I mean

{% highlight R lineanchors %}

{% endhighlight %}


