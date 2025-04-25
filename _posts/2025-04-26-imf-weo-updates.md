---
layout: post
title: World Economic Updates
date: 2025-04-26
tag: 
   - Economics
   - DataFromTheWeb
   - Pacific
   - OpenData
   - WorkRelated
description: I have a quick look at the latest World Economic Outlook released by the IMF, with a particular eye on the economic growth forecasts for Pacific island countries. The Pacific countries that have had the biggest revision downwards in their growth prospects over the six months since the last Outlook are the three in the Compact of Free Association with USA (Palau, Marshall Islands, and Federated States of Micronesia), plus Fiji.
image: /img/0288-pict-growth.png
socialimage: https:/freerangestats.info/img/0288-pict-growth.png
category: R
---

This week the World Bank and International Monetary Fund "Spring Meetings" have been in progress, and a highlight is always the publication of the latest World Economic Outlook. This never fails to get a bit of press attention around the world. This year much of it was like [this BBC piece](https://www.bbc.com/news/articles/czx415erwkwo), focusing on the revision in growth forecasts for 2025 (ie this year) compared to forecasts made for the same year in the last (January 2025) update of the Outlook. Of course, the current context is dramatic policy changes in the USA since January - the three months since the last update coincide with the new US administration of Donald Trump.

### Revised growth estimates for 2025

I had several motivations for exploring this data. In fact, the first one was I wanted to see exactly what "economic growth" is being highlighted by the BBC and others in this situation - is it real or nominal, per capita or just the absolute size of the economy? But I also wanted to see what the IMF had to say about the economic growth prospects for the Pacific island countries that I work with in my day job. 

So the first plot I polished up was this one, using a similar approach to the main graphic in the BBC article linked above, but highlighting the Pacific rather than G7 members:

<object type="image/svg+xml" data='/img/0288-pict-growth.svg' width='100%'><img src='/img/0288-pict-growth.png' width='100%'></object>

Unlike the BBC, I'm comparing the April 2025 World Economic Outlook to the edition that came out in October 2024 - the last time the database was published for all countries that the IMF studies rather than just an important subset. All the code to download and wrangle the data and polish the plots is at the bottom of the post.

We see an interesting story in the chart above - a bit more mixed than the case for the richer countries, who uniformly had a revision downwards of growth estimates. The three Pacific countries in a "Compact of Free Association" with the USA (Palau, Marshall Islands, and Federated States of Micronesia) all see a material revision downwards in their expected GDP growth for 2025, as does Fiji (which stands out as the largest economy of the region). For other Pacific countries, however, the revision is small and/or in the other direction. Samoa, for example, has seen an increase in its expected growth (on IMF estimates) for 2025 from 4.2% to a pretty healthy 5.4%.

Also we can see in this chart the answer to my earlier question - the economic growth in question is real (constant prices), but not adjusted for population size.

To be sure of that last point, I re-created the BBC's chart for the G7 countries, adding in the four BRIC (Brazil, Russia, India and China) countries and again using the October 2024 edition as my comparison point. That gets us this next chart below. If you compare to the BBC original you will see the April 2025 estimates are identical, but of course the October 2024 estimates in my chart differ from the counterpart light blue bars in the BBC chart, which were based on January 2025 estimates.

<object type="image/svg+xml" data='/img/0288-selected-growth.svg' width='100%'><img src='/img/0288-selected-growth.png' width='100%'></object>

 OK, that's all very well for looking at a small number of countries at once, but what were the 2025 growth revisions for all countries in the world? And how does the Pacific compare to that? For this task we have too many countries for a bar chart and the best approach is a scatter plot, with the old forecast on the horizontal axis and the new one on the vertical axis. That gets us this chart:

<object type="image/svg+xml" data='/img/0288-pict-growth-scatter.svg' width='100%'><img src='/img/0288-pict-growth-scatter.png' width='100%'></object>

The diagonal line represents equal forecasts in both editions; it's not a line of best fit, which would be somewhat to the right of the line actually drawn. Consistent with all the public commentary, most countries have seen a revision downwards in their economic growth prospects. This has been most dramatic for Sudan and Southern Sudan, for [reasons not related to tariffs](https://www.bbc.com/news/world-africa-59035053).

### Overall growth tragectories

Revisions in growth forecasts are good for people looking for the very latest "market-moving" news, but probably more important is the big picture of economic growth trends. I doubt many of my readers knew that Palau's forecast economic growth for 2025 six months ago was 8.8%, so perhaps the news that it is downgraded to the (still pretty positive) 5.6% leaves you cold! 

More interesting for big picture people is perhaps some line charts showing GDP as a whole. For this purpose, I have chosen to emphasise GDP that is corrected for both changes in local prices (so constant prices) and cross-country differences in what can be purchased (purchasing power parity); and per capita. This is the best measure in this data of average economic situation of these countries.

To start with, here's the global picture:

<object type="image/svg+xml" data='/img/0288-all-growth-line.svg' width='100%'><img src='/img/0288-all-growth-line.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0288-pict-growth-line.svg' width='100%'><img src='/img/0288-pict-growth-line.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0288-pict-growth-comp-line.svg' width='100%'><img src='/img/0288-pict-growth-comp-line.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0288-pac-ratios.svg' width='100%'><img src='/img/0288-pac-ratios.png' width='100%'></object>




## Code

### Accessing data

{% highlight R lineanchors %}



{% endhighlight %}



### Polishing plots

{% highlight R lineanchors %}



{% endhighlight %}

