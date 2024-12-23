---
layout: post
title: Depression incidence by county and vote for Trump
date: 2024-12-23
tag: 
   - VotingBehaviour
   - Economics
description: I look at county level data on incidence of depression and voting for Trump in the 2024 US Presidential election
image: /img/0285-us-1719-lim.png
socialimage: https:/freerangestats.info/img/0285-us-1719-lim.png
category: R
---
A skeet floated across my Bluesky feed that looked at the cross-sectional relationship between incidence of depression in 2020 and voting for Trump in the 2024 Presidential election. The data was at state level, but the hypothesis of interest was an individual one (are depressed people voting for Trump). I don't have the individual level microdata that might help explore the actual hypothesis, but I was surprised to see that the state-level regression had a significant evidence of an effect, and wondered if this would continue at the county level, which still has relatively accessible data.

Well, it turns out it does, as we can see with this first, simple chart:

<object type="image/svg+xml" data='/img/0285-life-exp-scatter.svg' width='100%'><img src='/img/0285-life-exp-scatter.png' width='100%'></object>

I'll be fitting some more fancy models and getting better charts further down, but the basic message is the same as in this one - counties with higher incidence of depression had a higher proportion of their vote going to Trump than was the case with counties with lower levels of depression.

Before I say anything else or show one line of code, let's get straight that this is very possibly not a causal link. In fact there are at least three things that are plausibly happening here:

1. People who are more depressed were more likely to vote for Trump
2. People (who may themselves be not depressed) who are in areas with lots of depressed people around them were more likely to vote for Trump (eg because voters think "Trump will be able to do something about all the depressed people around here")
3. Some underlying factor (eg economic, social or cultural conditions) that leads to some areas having higher rates of depression also leads to higher votes for Trump, through some other mechanism 

My instinct is that #3 is the more likely explanation, but I personally don't actually have evidence to choose between them. Nor are these hypotheses mutually exclusive; two or all of them might be true at once.

OK here's the code that gets that data and produces the first chart and a simple model with a statistically significant effect:

{% highlight R lineanchors %}

{% endhighlight %}




{% highlight R lineanchors %}

{% endhighlight %}




{% highlight R lineanchors %}

{% endhighlight %}
