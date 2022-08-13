---
layout: post
title: Pacific island population pyramids
date: 2022-08-13
tag: 
   - DataVisualization
   - WorkRelated
   - Demography
   - Pacific
description: I show how to access data from the Pacific Data Hub to draw population pyramids of Pacific Island countries and territories
image: /img/0240-pop-pyramids-wide.png
socialimage: http://freerangestats.info/img/0240-pop-pyramids-wide.png
category: R
---

## New job!

I have a new job! I am the Director of the [Statistics for Development Division](https://sdd.spc.int/) at the Pacific Community, which is commonly known by its now-old acronym the SPC (this [used to stand for](https://www.spc.int/about-us/history) the South Pacific Commission, then the Secretariat of the Pacific Community, and is now just an orphan  or [pseudo-acronym](https://en.wikipedia.org/wiki/Acronym#Pseudo-acronym) without specific meaning for the 'S').

The new job is going to have less hands-on coding than my last role at Nous, which will probably mean I reinvigorate this blog. One of my original purposes of starting this after all was to give me a coding outlet and motivation to keep my skills from rusting, several roles ago.

I'm not going to blog about work here, or only briefly in passing, but I might occasionally drop some work-related stuff in when it fits with the overall mood here (and there are definitely a lot of synergies). Also, because I've moved to New Caledonia and I'm naturally going to be thinking a lot more about the Pacific, there's likely to be more Pacific data popping up in my examples.

## Some Pacific data

Today I'm showcasing the [Pacific Data Hub](https://pacificdata.org/about-us), which is a New Zealand-funded central repository of data about the Pacific and from the Pacific. It's led by the Pacific Community, specifically by the Statistics for Development and Information Services divisions in partnership. It's new and so far most of its data is from the official statistics systems of our members, but we have hopes and plans for a lot more.

One of the more complete parts of the Pacific Data Hub is PDH.Stat, our implementation of the OECD-originated tool for disseminating aggregate indicator data. The .Stat technology, which is based on the SDMX standard for data and metadata, is commonly used by national statistics offices (including for example [NZ.Stat](https://nzdotstat.stats.govt.nz/wbos/Index.aspx) and the [ABS' .Stat Data Explorer](https://explore.data.abs.gov.au/)), but it is very non-trivial tech to set up and maintain. The median number of staff at Pacific Island country and territory national statistics offices is 14 (compared to just over one thousand at Stats NZ, for example), and it's not feasible for most individual countries to manage their own .Stat implementation. So we at the Pacific Community host it collectively and aim to provide a one-stop shop for all the important aggregate information on the Pacific.

Here's the image I'm producing today. It's population estimates (for 2020) and projections (for 2050) for 21 Pacific Island countries and territories - all of the Pacific Community Pacific Island members except tiny Pitcairn Island.

<picture  > 
    <source srcset="/img/0240-pop-pyramids-wide.png" width = '100%' media="(min-width: 1200px)">
    <img src="/img/0240-pop-pyramids-tall.png" width = '100%' alt="Population pyramids with 5 year age bands and sex for 20 Pacific Island country and territories">
</picture>

Depending on your screen size that should be either 7 countries across and 3 down, or 3 across and 7 down.

There's some very interesting things here. 

In PNG, Solomon Islands, Vanuatu and (to a lesser extent) Kiribati, we see the classic wide base population pyramids familiar in poor, rapidly growing countries where death rates have dropped over the past century but birth rates have not done the same. These shapes are projected to stay similar in the future, just much wider - population growing at around 2% per year, which is fast (the global growth rate right now is about 1% and dropping steadily - the projected average growth from 2020 to 2050 would be much less).

In contrast, we see in Fiji a pattern common in middle income countries further along the pathway sometimes called the demographic transition. People have fewer children, the pyramid's walls are steeper in 2020 and nearly vertical in 2050, and population growth rates are accordingly pretty low (0.2%). New Caledonia is even further along that path. 

But in some other countries and territories though there are unusual patterns. Examples of one pattern can be seen in Samoa, Marshall Islands, Federated States of Micronesia, Tuvalu and Tonga. These have a wide-base population pyramid showing lots of young people, relatively speaking, in 2020. But projected growth rates are very low. What's happened to the young people from 2020? As the age bands are 5 years and the difference between the 2020 bars and the 2050 blue lines is 30 years, we should see the bulge move up vertically by 6 slots, and stay the same horizontal size barring deaths and migration. But the people who are aged 0 to 20 in 2020 (bottom 4 bars) are simply not projected to be in these countries aged 30 to 50 in 2050.

This isn't because of a catastrophic early adult death rate, but large scale migration. It's interesting to see this feature so prominent in the projections, which of course are based in part on a model of net migration based on previous history in each country.

## Getting and presenting data from PDH.Stat

Because [PDH.Stat](https://stats.pacificdata.org/) is part of the bigger international community of .Stat implementers, we can leverage development at the OECD and elsewhere. For example, we have made sure that all the data in PDH.Stat is [accessible by RESTful API](https://docs.pacificdata.org/dotstat/api) in the SDMX format; and made PDH.Stat accessible by [plugins for Excel, Power BI, Stata, Python and of course R](https://docs.pacificdata.org/dotstat/plugins). 

For R, we use the rsdmx package which interacts with many of the official sites around the world providing data via SDMX.

*Post continues below R code*
{% highlight R lineanchors %}


{% endhighlight %}





{% highlight R lineanchors %}


{% endhighlight %}


Too choose between the wide or the tall versions of the image I used the `<picture>` HTML tag, which I'd borrowed from [this StackOverflow exchange]()https://stackoverflow.com/questions/23414817/load-images-based-on-screen-sizehttps://stackoverflow.com/questions/23414817/load-images-based-on-screen-size)

{% highlight HTML lineanchors %}
<picture  > 
    <source srcset="/img/0240-pop-pyramids-wide.png" width = '100%' media="(min-width: 1000px)">
    <img src="/img/0240-pop-pyramids-tall.png" width = '100%' alt="Population pyramids with 5 year age bands and sex for 20 Pacific Island country and territories">
</picture>
{% endhighlight %}