---
layout: post
title: Time-varying house effects in New Zealand political polls
date: 2017-09-16
tag: 
   - NewZealand
   - VotingBehaviour
   - R
description: I adjust my state-space model of New Zealand voting behaviour to allow for the house effect of one of the pollsters to change from the time they started including an on-line sample, and get some interesting results.
image: /img/0111-reid-methodology-change-impact.svg
socialimage: http://ellisp.github.io/img/0111-reid-methodology-change-impact.png
category: R
---

## House effects in state space models of voting intention

"House effects" is the term commonly used in models of voting intention to refer to the different statistical biases of different pollsters.  In my [Model B state space model of the New Zealand 2017 election](/elections/state-space.html), the estimated house effects look like this:

<img src='/img/state-space-house-effects.svg' width='100%'>

What these mean, to pick the largest example, is that Bauer Media Insights are estimated to over-estimate the party vote for Greens by about four percentage points.  This is taken into account when the model simultaneously estimates the unobserved latent voting intention for every day in the observed period - and the house effects for all the other pollsters.  Only one type of observation has no house effect, and that's the elections on the day.  Those particular results are definitive!

For those wondering about the presence of six pollsters, including some that are no longer active in New Zealand - my model starts with the 2011 election and any pollster with two or more public polls since then is included.  Even though Digipoll and Ipsos are not publishing polls for the 2017 election, they did leading up to 2014, and hence they go into the mix for estimating the latent voting intention every day leading up to the 2014 election day, which is useful for understanding the house effects of those pollsters that are still publishing polls.

## What happens when the pollster changes their method?

Peter Green drew my attention ([on Twitter](https://twitter.com/ellis2013nz/status/908463168581599232)) to a recent change in the Newshub - Reid Research poll - in 2017, 25% of the sample has been online, rather than the full sample being recruited and questioned over "land" telephone lines.  With a lot of scrutiny recently on volatility and disagreement between the polls as we head to election day in a week's time, Dr Green wondered if the change in method could have led to a detectable change in the Reid Research house effect.

Trying to detect this is a fairly straightforward addition to my state space model; I just added to the data a dummy variable that was 1 for Reid Research polls in 2017 and 0 otherwise, and a parameter "additional house effect" for each party to apply when that indicator is 1.  Seven more parameters to estimate in a model that already has more than 20,000 (because there is a parameter for the voting intent for each party, every day since September 2011) doesn't add too much to the estimation difficulty.  

We get this result:

<img src='/img/0111-reid-methodology-change-impact.svg' width='100%'>

We can interpret this as the change in Reid Research's house effect in 2017, compared to their house effect in 2016 and earlier.  Remember that all this is estimated in the light of how the other pollsters are estimating the unobservable voting intention; those other house effects are assumed to stay the same over this period.  So it looks like since the change in method is leading to the reported estimated vote for National going *up* by about 1.5 percentage points (but really, anwhere between 0 and 3%) and Labour *down* by about 1 percentage point; Greens and Māori Party vote down by a little less.    

These effects are additional to the "2016 and earlier" effects shown in the first chart of this post.  So the *net* Reid Research house effect for the Greens is that Reid Research polls are probably still *over*estimating the vote but only by about one percentage point (less than the two points in 2016 and earlier); and for the National Party, Reid's reported values should probably be discounted by about 1.5 percentage points (as the 2016 and earlier effect is basically zero).  The house effect for New Zealand First doesn't change - our best estimate is that Reid Research continue to understate the vote for New Zealand First by about 1.5 percentage points.

The operative line of code, written in Stan is this:

{% highlight Stan %}
for(j in 1:n_parties){
    
    ...
                              
    for(t in 1:y5_n)
        y5_values[t, j] ~ normal(mu[y5_days[t], j] + 
					d[5, j] + 
					reid_impact[j] * reid_method[t], 
				 y5_se[j] * inflator);
        
    ...
        
  }


{% endhighlight %}

I'm not terribly proud of my Stan coding (everyone's learning, right) and think this could be optimised in various ways, but it's fairly transparent.  The `y5_n` number of random values of polls by the 5th pollster (Reid Research - 5th in alphabetical order), for the `j`th party on the `t`th day since the election are normally distributed.  The mean of that distribution is the appropriate value of `mu` for that day (`mu` is the unobserved, latent voting intention), plus the appropriate value of `d` (ie the standard house effect), plus the `reid_impact` adjustment if and only if the dummy variable `reid_method` is 1 for that particular day.

The impact on my prediction for election day is pretty marginal; it's only one pollster among several, and we nudge *down* National's chances a bit compared to my last publication, and *up* Labour and Greens.  The overwhelming probability at the time of writing is forecast to be with a coalition led by either Labour or National but needing New Zealand First as one of its members.

Check out:

- the [latest forecast with Model B](/elections/state-space.html), or my [combined forecast](/elections/combined.html) (recommended) that averages my two models
- [source code](https://github.com/ellisp/nz-election-forecast) for the New Zealand election forecasting project
- Latest version of my [nzelect R package](https://github.com/ellisp/nzelect), with poll data from 2002 to the current day and detailed results for the 2014 election.

