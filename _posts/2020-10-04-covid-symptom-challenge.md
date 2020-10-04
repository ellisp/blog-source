---
layout: post
title: Facebook survey data for the Covid-19 Symptom Data Challenge
date: 2020-10-04
tag: 
   - Health
   - Surveys
description: Two huge surveys of Facebook users seem to provide valuable new information on how the world is responding to Covid-19, but I am very unsure about whether they have potential to enable earlier detection of outbreaks.
image: /img/0195-states-masks.svg
socialimage: http://freerangestats.info/img/0195-states-masks.png
category: R
---

The Covid-19 Symptom Data Challenge (under way at the time of writing this blog post) makes available two large sets of survey data. These are provided in granular but aggregate form ie no response-level microdata, at least for this Challenge. The two surveys cover the USA and the rest of the world respectively, and involve hundreds of thousands of Facebook users aged 18 and over, in 119 countries. The actual challenge is to use this data for this purpose:

> "Can you develop a novel analytic approach that uses the CMU/UMD COVID-19 Symptom Survey data to enable earlier detection and improved situational awareness of the outbreak by public health authorities and the general public?"

The Challenge is a a partnership organized by Catalyst@Health 2.0 with Facebook Data for Good, the Delphi Group at Carnegie Mellon University (CMU), the Joint Program on Survey Methodology at the University of Maryland (UMD), the Duke Margolis Center for Health Policy, and Resolve to Save Lives, an initiative of Vital Strategies. I am not affiliated in any way with the Challenge or any of its partners.

Entries are due on [6 October 2020](https://www.symptomchallenge.org/eligibility-and-requirements#timeline). I would have liked to put in an entry, but lacked time. Also, on my first glance at the data, I wasn't sure that I *could* develop a novel analytic approach "to enable earlier detection and improved situational awareness"; at least not without a very broad definition of "improved situational awareness" that goes well beyond that implied by the example analytical approaches provided. I will use this post to jot down the few things that occured to me in the time I did have available to look at this data. 

My summary conclusion is that these two huge surveys of Facebook users seem to provide valuable new information on how the world is responding to Covid-19, but I am very unsure about whether they have potential to enable earlier detection of outbreaks. So I will look with interest at what people come up with in respond to the challenge.

My code to download, tidy and analyse this data is in [this GitHub repository](https://github.com/ellisp/covid-symptom-challenge).

## Variation by time and state in Australia

### Disappointing for symptoms as an early-warning indicator

In looking at this data, I started with what I knew best, the situation in Australia. The UMD survey has enough of a sample size to talk meaningfully about trends over time by states and territories. Here is what is surely the headline statistic, the percentage of the population over 18 reporting "Covid-like illness symptoms":

<object type="image/svg+xml" data='/img/0195-states-cli.svg' width='100%'><img src='/img/0195-states-cli.png' width='100%'></object>

These estimates are the weighted ones provided by Facebook, so they have been corrected for age and sex in the sample to allow unbiased estimates of population values. There are more detailed individual questions available in the CSV data (but not the API version of the data), relating to coughs, fevers, etc., but these have been combined by relevant experts into a single proportion of the population with Covid-like symptoms. 

Knowing what's been happening in Australia in the past six months, I was pretty disappointed by this result. There have been virtually no Covid cases in most of the country, whereas in July and August we were up to 100 cases per million per day in Victoria (population 6.7 million). That meant for a short while at the end of July Victoria was up at the peak level of new cases the UK went through six weeks earlier, comparable to the level the US as a whole experienced in September or half the US peak of 200 cases per million per day during July. So, for a couple of weeks, Victoria really felt like part of the pandemic, while for the rest of Australia it was effectively suppressed or (eg in Western Australia) eliminated.

However, there is no sign of this in the chart of survey-reported Covid-like symptoms. There is no way you could judge by looking at the chart above either that Victoria was hit by Covid worse than the other states; or that it was 10-100 times worse in July and August in Victoria than before and after.

Of course, this is just one country, one with relatively low Covid rates, and there might be much more to glean from the full data. There's a very promising-looking example with Florida data on the Challenge website which shows a much stronger relationship between symptoms and actual Covid cases. And I haven't even tried any statistical modelling with the Australian data. But just eyeballing the data is enough to show that there isn't a spectacular, early warning, reliable indicator here. 

There might be some signal through the noise, but I'd be pretty nervous about advising governments on action based on it. Would you warn the Western Australian government of that upwards trend in August and September - does it mean an undetected Covid outbreak, in a period of complacency and apparent elimination? Seems unlikely (not impossible of course! - in which case this would retro-actively become a spectacular vindication of this use of the data). We'll see what comes, both in Western Australia, and in analysis in response to the Challenge.

### But the survey is certainly reflecting reality on other variables

With new survey data when we're disappointed in a key variable, we often wonder "have we scrambled the whole thing? Or perhaps people are completely making up their answers?" An essential reality check is to look at other variables that we can validate. I chose the percentage of Australians not wearing masks (I chose 'not' because of the way the original question was structured - options for "never", "Some", "half", "most" and "all" the time). This time we see an impressively tight pattern, in fact as clear a signal as I've ever seen from survey data:

<object type="image/svg+xml" data='/img/0195-states-masks.svg' width='100%'><img src='/img/0195-states-masks.png' width='100%'></object>

Rarely do we see anything in survey data as tight as that. In both spatial and temporal comparisons, what we see here is exactly what we'd expect. I was surprised at the number of NSW and Queensland residents who were wearing masks at least sometimes, but a bit of googling showed me I should have expected that. I certainly *wasn't* surprised that zero Victorians report never wearing a mask in the last month or so. They are mandatory when leaving the home, and the compliance I see is pretty high. I think we can be sure that the survey hasn't been scrambled during processing, and that people really are responding meaningfully to the questions they're given.

### The survey tells us *something* about health

So what about some of those other variables? Here's where I think this data really adds some valuable insight. For example - there's a lot of interest in the mental health impact of Covid-19 and of the responses to it. This is an area where survey data has a longish history of use in estimates of prevalence. There are a few high level questions in the survey that are of interest, including one on how often the respondent feels depressed. Here are the results by state:

<object type="image/svg+xml" data='/img/0195-states-dep.svg' width='100%'><img src='/img/0195-states-dep.png' width='100%'></object>

This looks like something that is both meaningful and (I think) new. In particular, that downwards trend in Victoria validates a lot of people's lived experience I am sure, but I haven't seen such specific evidence as this yet. 

Surveys are difficult and expensive, particularly if you want to run them day after day and get time trends. I'm not aware of any other data source that gets anything like this sort of picture, certainly not at this frequency and timeliness. I'd be surprised if there's not potential for this data to be used for some kind of meaningful monitoring and situational awareness here; so even if it turns out not to be great for early warning of Covid-19, it should still have something to contribute.

## Six countries

Here are three similar charts to those above, but for six arbitrarily chosen countries. I don't have much to say on these; it's similar to the Australian situation but writ large. That is:

- not sure whether the monitoring of symptoms tells us much in terms of *advance warning*, but there might be something if we probe harder
- the questions on *masks certainly* reveal national and temporal patterns
- the question on *depression probably* tells us something useful, certainly in terms of trends across time, possibly across countries.

<object type="image/svg+xml" data='/img/0195-countries-cli.svg' width='100%'><img src='/img/0195-countries-cli.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0195-countries-masks.svg' width='100%'><img src='/img/0195-countries-masks.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0195-countries-dep.svg' width='100%'><img src='/img/0195-countries-dep.png' width='100%'></object>

That's all for today. Great data source; will be challenging to use to understand Covid incidence; might have more to say on other, indirect issues. 

Code is [on GitHub](https://github.com/ellisp/covid-symptom-challenge/blob/main/analysis/0195-covid-symptoms-blogpost.R)


