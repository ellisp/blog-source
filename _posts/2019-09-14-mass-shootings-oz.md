---
layout: post
title: Poisson point processes, mass shootings and clumping
date: 2019-09-14
tag: 
   - Distributions
   - Times	eries
   - Australia
   - Crime
   - Reproducibility
   - Simulations
   - R
description: I show how modelling the distribution of an underlying continuous variable that has been clouded by binning is much better way of understanding the data than crude methods dealing directly with the binned counts.
image: /img/0158-events.svg
socialimage: http://freerangestats.info/img/0158-events.png
category: R
---

## Did the averagee rate of Australian mass shooting decline after 1996, or was the drop just chance?

I recently came across this letter to the Annals of Internal Medicine by Simon Chapman, Michael Stewart, Philip Alpers and Michael Jones :[Fatal Firearm Incidents Before and After Australia's 1996 National Firearms Agreement Banning Semiautomatic Rifles](https://annals.org/aim/fullarticle/2675234/fatal-firearm-incidents-before-after-australia-s-1996-national-firearms), via [this piece by Gun Control NZ](https://www.guncontrol.nz/media/myths-propaganda-statistics-why-dr-samara-mcphedran-cant-be-belie). 

The question under investigation is whether the drop in mass-shooting events in Australia since the change in the firearm regulatory environment in 1996 could be a result of chance or not. "Mass shootings" are defined as homicides in which at least five persons died, not including the perpetrator. There were 13 of these events in the 18 years from 1979 up to the time of the National Firearms Agreement, and none afterwards. Chapman et al model the events with a [Poisson point process](https://en.wikipedia.org/wiki/Poisson_point_process) and [provide all of their R code](https://acp.silverchair-cdn.com/acp/content_public/journal/aim/937339/m18-0503_supplement.pdf?Expires=1567900673&Signature=E6-Z~KyoSN8w5Q0vyXU1ypYoOOr4g05Vu5a3AHV~PbBj0ewSl7-cgJfvye7BV9DhonioJvK2SFb747-XVpGeuheaBHN0BRQLxPemmQIWyB7eXqyovfTmn6Kfa9Quh5FsLLgWPx-Syv7laz0RICZ9BVdb4bJzQkphRsrq1RMZm6bFWutH2Uoy-E772jI19KUJU1TGIW6AmvI5d1PU1TWHZF-wGwOYqSn-uBtaCVifpzL3MQ0EStdFlrJ55~We-K11wYPl~noH~lrFxDs0YuiM~MmuJzZQewEiAIpPNlfyCox4wrTdpSlmTMBEb1ArmED~JpgLxe7V29OKQV~MrVKIog__&Key-Pair-Id=APKAIE5G5CRDK6RD3PGA) to replicate their findings. However, the code they provide is somewhat compact and tersely (if at all) commented, and having familiarised myself with how it works I thought it worth while blogging about in a somewhat verbose manner. 

I am putting aside [controversies](http://www.hoplofobia.info/wp-content/uploads/2015/08/2018-Kleck_Chapman_NFA_comments.pdf) about whether five persons is the correct threshold to consider, whether these events should be restricted only to deaths from rapid fire weapons, and analysis of general trends and possible confounding factors, and for the purpose of this blog just approaching this as an illustration of the modelling of Poisson point processes.

## Data familiarisation

<object type="image/svg+xml" data='/img/0158-events.svg' width='100%'><img src='/img/0158-events.png'></object>


*Post continues after R code*

{% highlight R lineanchors %}

{% endhighlight %}

## Changes I made in the original code

If you compare my eventual R script I used for this blog with [the original](),

* Brought the definiti

<object type="image/svg+xml" data='/img/0157-res2.svg' width='100%'><img src='/img/0157-res2.png'></object>

<object type="image/svg+xml" data='/img/0157-res3.svg' width='100%'><img src='/img/0157-res3.png'></object>

