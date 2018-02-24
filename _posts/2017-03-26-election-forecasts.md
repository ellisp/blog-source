---
layout: post
title: New Zealand election forecasts
date: 2017-03-26
tag: 
   - VotingBehaviour
   - Timeseries
   - R
description: My New Zealand Election Forecasts web page is up; and I have some reflections on election day randomness, and on quality control.
image: /img/0085-gam-vote-predictions.svg
socialimage: http://ellisp.github.io/img/0085-gam-vote-predictions.png
category: R
---

Over the weekend I released a new webpage, connected to this blog, with [forecasts for the New Zealand 2017 General Election](/elections/elections.html).  The aim is to go beyond poll aggregation to something that takes the uncertainty of the future into account, as well as relatively minor issues such as the success (or not) of different polling firms predicting results in the past.  The [full source code is available in its own repository](https://github.com/ellisp/nz-election-forecast) (and under active development) and I won't try and discuss it all here, but I did have a couple of reflections.

## Randomness on election day

The guts of the model currently being used in that site is a generalized additive model with a multivariate Normal response, which is party vote on the logit scale.  Effectively, I have a latent unobserved party support variable, and I treat the various polling data as observations somehow imperfectly manifested by that latent variable.  The whole approach is a quick and dirty alternative to a Bayesian state space model which stays on my "to do one day" list.  

When transformed back onto the original scale (proportion of party vote), that model looks like this:

<img src='/img/0085-gam-vote-predictions.svg' width='100%'>

To turn this into a prediction for election day, I need to deal with two sources of randomness:

- Because predicting the future is hard, I don't know where the latent support for each party will be on election day (at the time of writing, six months away).  The shaded ribbons in the image above are 95% credibility intervals for where this will end up, and hence can be treated as a random variable.
- Even after taking into account the uncertainty in the underlying latent level of support, there will be some randomness in how it manifests on election day.  Will people turn up; will we find out the polls have systematically missed something; and so on.

The aim is to create simulations that take both those forms of randomness into account, including the fact that the random variables in question are negatively correlated (because support for one party increases at the expense of the others).  Those simulations look like this, when done:

<img src='/img/0085-gam-vote-predictions-density.svg' width='100%'>

The first form of variance is easy, and comes straight out of the `mgcv` function that fits the generalized additive model; as does an estimate of the covariance between the party support.  But I had three goes at identifying the second form of randomness, election day randomness:

1. At first I ignored it altogether.  An election is (sort of) a census compared to an opinion poll which is a sample survey, so one might expect the election to come out bang on where the "real" latent support is, so the only uncertainty to worry about is estimating the latent support.
2. That seemed to create distributions of party vote that were unrealistically narrow and I worried I would be making the classic electoral modelling mistake of underestimating the randomness of the electorate (yes, this has been prominent over the past year).  I went in the other direction and said, perhaps an election is just as random as an opinion poll; I estimated the variance of individual opinion polls around the latent party support, and simulated election day outcomes with the same variance.  This is actually the first version I published; I liked it because it had great dollops of uncertainty in its suggested outcomes, which is nearly always a good thing that helps compensate for the problem of the real world being messier and more complex than our models, so confidence or credibility intervals based purely on models nearly always fail more than advertised.
3. Further reflection during the day made me think I'd gone too far.  Method #2, even when applied to data one week out from the 2014 election, had an implausibly large range of outcomes; reflecting the fact that individual polls are pretty small and highly variable, much more so than elections.  I opted instead to compare the election results for parties in 2005, 2008, 2011 and 2014 to the latent party support estimated by my generalized additive model, and use the mean squared error from that to scale the variance of my simulated 2017 election day.

Here's where the mean squared error from my model compares to actual election results:

<img src='/img/0085-variance.svg' width='100%'>

This suggested a simple pragmatic linear model of `E(log(Var(logit(vote))) ~ E(logit(vote))`.  So now in my forecasting simulation, I take that expected variance, add it to the variance in the estimate of the latent party support itself, and combine with the correlation of party support from the GAM to estimate a variance-covariance matrix for use with `mvrnorm` to generate party votes (on logit scale still).  This is the method currently live on the website, having replaced my too-conservative method #2.  For the curious, here are the probabilistic predictions for the 2017 New Zealand election:

<img src='/img/0085-gam-final-chances-bar.svg' width='100%'>
<img src='/img/0085-gam-results-density.svg' width='100%'>

## Quality control

I had a little annoyance this morning, with a version of the page containing an error (although only for about 15 minutes) in the part that tested the forecasting methods on the 2014 election data, pretending to have polls only up to six months before the election.  A bit of code that was meant to turn "no polling information available" on minor parties into 0% support had slipped through a change in method from the raw proportion scale to the logit scale.  Inverse logit of 0 is 0.5 so effectively I was saying that the Conservative party, ACT, and United Future each had a bunch of polls indicating 50% support (for non-New Zealanders, these are all small parties which in fact polled less than 5% support in 2014).  I'd noticed some funny graphics with trend lines implausibly high in some dates, but forgotten to look into it.  Once I looked into it I quickly found the bug, but not until after the site had been live for 15 minutes.  

That experience, and the experience of changing my mind on how to simulate election day randomness, made me reflect on quality control.  Electoral predictions are important things, the more so given that in New Zealand there's few competing models (and only a couple of poll aggregators).  It would be embarrassing, and generally bad, to make many errors in a page like the one I'm writing about here, no matter what caveats and disclaimers I add.  Erring on the side of caution helps; but this is the sort of area where sometimes one doesn't even notice one is making rash assumptions.  Full transparency is part of the solution (hence [all my code is open to scrutiny](https://github.com/ellisp/nz-election-forecast)) but isn't really a substitute for professional teamwork.  

So some lessons (from this and similar experiences):

- get a peer reviewer if you can.
- publish code in full if you can, and take care to make it as reproducible as possible.  Working through the situation of "could someone pick up this repository and run it" can be a good way of surfacing some types of problems that otherwise stay hidden.
- don't just review code, review thinking processes.  Being challenged on the source of individual election variance would have got me to a better outcome earlier; but just reviewing my code wouldn't have done it (the code worked fine, and did what it was intended - the problem was the sub-optimal method).
- pay attention to warning signs.  Even if they don't look important themselves, they might indicate something else.

All fairly obvious.  Some other day I'll do a more thorough blog post on quality control.
