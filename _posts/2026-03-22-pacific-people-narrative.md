---
layout: post
title: Pacific island demograpy, the narrative
date: 2026-03-22
tag: 
   - Pacific
   - Economics
   - Demography
   - WorkRelated
description: Pacific island populations are shaped by people <i>movements</i>, more than is the case for most countries in the world. Ease of migration&mdash;because of history, culture or international agreements&mdash;has a critical impact on population size, growth and shape; and the fundamentals of the economy. 
image: /img/0305-population-line.svg
socialimage: https:/freerangestats.info/img/0305-population-line.png
category: R
---

This post is the last in a series of seven on population and people movement issues in the Pacific. The first six posts featured code re-generating the charts I used in a keynote speech before the November 2025 meeting of the Pacific Heads of Planning and Statistics in Wellington, New Zealand. Today's post is simply a narrative drawing on all those charts. There's no R code today; links to the previous posts are at the bottom.

## Population growth is varied, but the larger Pacific island countries are growing pretty fast

The first point to make is that the Pacific is very varied in terms of its population dynamics. We can see this a bit in the most straightforward and intuitive chart of population historical growth and future projections:

<object type="image/svg+xml" data='/img/0305-population-line.svg' width='100%'><img src='/img/0305-population-line.png' width='100%'></object>

The data this comes from is the United Nations' 2024 population projections, which are currently used as-is in the Pacific Data Hub. Some of these figures are known to be wrong, either because there have been censuses that reported too late to be taken into account (e.g. Federated States of Micronesia) or for other reasons (e.g. Tokelau). But they're a good starting point.

The vertical scale on that chart is "free" meaning each country-facet is on a different scale, so it's not easy to tell visually which countries are larger or smaller. To help with this, they are organised from smallest (Niue) to largest (Papua New Guinea). Pitcairn Islands, although a member of the Pacific Community, is not included because of its tiny size (around 50 people) even by Pacific standards. 

A bit of familiarity with the region though, helped by the sequencing on the chart, identifies that the larger countries&mdash;those in the bottom row from Samoa to Papua New Guinea, up to say Kiribati and Guam in the row above&mdash;are growing faster as well as being larger. Well that's hardly surprising is it; faster growing countries will of course become larger, so we'd expect these things to be related? Except that the causality is not all in one direction like that. At its extreme, there's more to the fact that Niue is only one or two thousand people and Papua New Guinea is 10,000 times its size (about 11 million) than that Papua New Guinea has grown faster recently. Niue's small physical land mass, and other characteristics, are one of the reasons it doesn't grow as fast.

The next chart is an attempt to illustrate this further. First, consider a version where we just put size on the horizontal axis and growth on the vertical. Both axes are logarithmically transformed, or else the chart would be all blank space with Papua New Guinea out on the right and all other countries a cluster of dots on top of eachother on the left.

<object type="image/svg+xml" data='/img/0305-population-scatter.svg' width='100%'><img src='/img/0305-population-scatter.png' width='100%'></object>

I quite like this chart, for audiences with enough numeracy to cope with a scatter plot and log axes. The pink shading makes clear which countries are shrinking, a point of definite interest. The colour-coding of points by sub-region is very useful, making clear how the Melanesian countries cluster together in the top right "large and growing fast" quadrant; most Polynesian countries (not all) are shrinking; and Micronesia has a real mix. The next version of this chart adds a particularly interesting element to this (in a slide show, you can move from one chart to the next and it looks like these circles appear by magic):

<object type="image/svg+xml" data='/img/0305-population-scatter-highlighted.svg' width='100%'><img src='/img/0305-population-scatter-highlighted.png' width='100%'></object>

Those circles represent the countries or territories that have some kind of easy migration access to a larger, richer country. This includes (conveniently, mostly in sets of three):

* three French territories (French Polynesia, Wallis and Futuna, New Caledonia); 
* three Realm of New Zealand territories (Cook Islands, Tokelau, Niue); 
* Pitcairn Island whose residents (mostly) have right of abode in the UK; 
* three USA territories (Guam, Commonwealth of the Northern Mariana Islands, American Samoa)
* three independent countries with a Compact of Free Association with the USA (Palau, Marshall Islands, Federated States of Micronesia)

Now we have an interesting feature. All of the countries with negative growth apart from Tonga and Tuvalu are highlighted this way. Perhaps in future we would include Tuvalu in the list above because it does now have special arrangements with Australia and New Zealand that allow a certain amount of people movement, most importantly the Falepili Mobility Pathway scheme that allows 280 permanent residence visas per year with Australia. But this is too recent to show up in the chart above.

Samoa and Tonga do not have automatic right of residence to New Zealand but they have strong cultural and historical ties, large communities already living there (more on this later), and the visa obstacles are largely surmountable.

In my view the distinction between the countries that have easy mobility to a larger richer countries, and those that don't, is the most important single marker to use when considering population issues in the Pacific.

## Net migration is a critical factor in different population growth rates

OK, let's look at net migration. The United Nations has to estimate this as part of its projection process so we can see it for all the countries in their dataset. In the below, I've organised countries from those least impacted by migration (top left) to most (bottom right).

<object type="image/svg+xml" data='/img/0309-all-picts-net-migration.svg' width='100%'><img src='/img/0309-all-picts-net-migration.png' width='100%'></object>

There's a few remarkable things here. One is that the chart is mostly red&mdash;migration is nearly all outwards. Another is that it leaps around a bit. We can suspect data problems for some of this eg in the case of Tokelau's recent years, I just think we have an error (it's being looked at). 

As a way of drilling into the impact of migration on population, for just a subset of countries because it's getting complicated, is comparing the natural rate of increase (ie births minus deaths) with the net migration&mdash;which between them, add up to total change in population for each country. Here's a chart that does that, for just six countries:

<object type="image/svg+xml" data='/img/0309-six-picts-natural-immmigration-line.svg' width='100%'><img src='/img/0309-six-picts-natural-immmigration-line.png' width='100%'></object>

In each of these charts, we can start with the green dashed line, which is where natural increase would be with no migration. Then we add (or subtract, in most cases) the red dotted line which is net migration. The sum of these two is the blue solid line, total population change. The sort of things we see here are:

* For Kiribati and Papua New Guinea, the solid blue line is fairly close to the dashed green line, indicating that natural increase is what is driving population change
* For Samoa and Marshall Islands, the green dashed natural increase line is very positive and the red dotted migration line is very negative. These two drivers mostly cancel eachother out, but when all is added up Marshall Islands has rapid recent population decline and Samoa still has some remnant slow population growth
* For Niue and Marianas, the story is more complicated but in recent years has stabilised at "not much change" 

What we're seeing here is that migration&mdash;or the lack of it in significant numbers, for Kiribati and Papua New Guinea&mdash;is what is driving the population story.

## Migration (short and long term) changes the <i>shape</i> of the origin country's demographics

What does this mean for the structure of who is left? To illustrate this, I like to compare Kiribati and Marshall Islands. Both are entirely or mostly coral atolls; they are only around an hour's flight from eachother; and they have few natural resources other than their people, the ocean and its fish, and location. 

Location is a critical asset or curse for the Marshall Islands. Kwajalein Atoll in the Marshalls was a major Japanese base in World War II and the site of a bloody battle in 1944; now it is a key US base forming a bridging zone and operational depth between the so-called second (Guam, Palau, Saipan, etc.) and third (Aleutians, Hawaii, Samoa, New Zealand) [island chains](https://en.wikipedia.org/wiki/Island_chain_strategy) in preparation for the next Pacific war, against whichever eastwards-facing Asian land power that might be. Bikini Atoll in the Marshalls was the site of US nuclear weapon testing during the cold war. For our purposes, all this matters because Marshall Islands has a Compact of Free Association with the USA which provides large amounts of funding plus free people movement to the USA. Even with recent crackdowns in the USA this persists, although dealing with Marshallese who have been forcibly returned from the USA due to low level criminal behaviour is becoming a policy challenge. 

Kiribati has no such arrangement with its own large regional partner, Australia (former-British Kiribati uses the Australian dollar and drives on the left, just as Marshall Islands uses the US dollar and drives on the right hand side of the road).

<object type="image/svg+xml" data='/img/0314-kiribati-marshalls.svg' width='100%'><img src='/img/0314-kiribati-marshalls.png' width='100%'></object>

To repeat some text from the blog post where I introduced this chart: Kiribati today has about four times the population of Marshall Islands but in 1980 was only about double. The significant thing here is the wasp waist of the Marshall Islands pyramid in 2025—while it had a similar shape to Kiribati in 1980. People at peak working and reproductive age are literally absent from today’s Marshall Islands&mdash;in this case, primarily in the USA.

The result of this is that Marshall Islands not only benefits from its individuals having more freedom of movement and opportunity, and sending back remittances from relatively high paying lives in the USA; but also having a pressure valve for what would otherwise be a rapidly (too fast?) growing population. To put it bluntly, Kiribati has a problem of too many people (particularly on crowded southern Tarawa); Marshall Islands, if it has a population problem, is one of too few. The contrast of crowded, relatively poor Tarawa and less-crowded, relatively well-off Majuro is an obvious and stark one to anyone travelling to them both in quick succession.

## Pacific people are overseas in very considerable numbers

OK, so people have been moving from the Pacific islands to elsewhere for decades or longer. Proportionately speaking, does this matter? Have large numbers of Pacific islanders cumulatively ended up elsewhere? The following chart answers this with a resounding "yes", for at least nine countries:

<object type="image/svg+xml" data='/img/0313-diaspora-bar.svg' width='100%'><img src='/img/0313-diaspora-bar.png' width='100%'></object>

For seven countries, there are more people ethnically associated with that country living in the USA, New Zealand and Australia than in the origin country. For the countries in the bottom row - the three New Zealand Realm countries plus Pitcairn - there are many multiples more people living overseas. Around 40,000 Niueans live overseas (mostly in New Zealand) and less than 2,000 in Niue. For Tonga, Samoa and Marshall Islands the situation is not as extreme but still very substantial.

Illustrating this further, consider this chart of the world's largest Pacific Islander cities. Thanks to comments on LinkedIn I have improved this from previous versions&mdash;we now have a better estimate of people with Pacific Islander as one 'race' of several in Hawaii, and a more comparable definition of Greater Wellington. But what we see is broadly the same message as before&mdash;of the top ten Pacific Islander cities in the world, two are in Australia (Sydney and Brisbane) and one each in New Zealand (Auckland) and the USA (Honolulu).

<object type="image/svg+xml" data='/img/0312-pacific-cities-revised.svg' width='100%'><img src='/img/0312-pacific-cities-revised.png' width='100%'></object>

All this has substantial implications for cultural and national identity, for economics and for politics. What political rights and expectations are there for these overseas people? For the home country, is there a minimum size that is viable? Or even a minimum proportion of your people? What happens legally, politically and diplomatically (for example, to votes in the United Nations General Assembly) if&mdash;as is clearly possible under climate change&mdash;a low-lying coral atoll country like Tokelau, Marshall Islands or Tuvalu loses all of its land to sea-level rise and *all* of its people are living overseas? 

## Remittances are a critical, even dominant, part of many (but not all) Pacific island economies 

Without going to these extreme scenarios we have enormous economic implications in the here and now. One of the strongest indicators of this is the level of remittances. Remittances are payments from family or other contacts overseas, typically in a higher income country. The source of remittances can be people on relatively short trips overseas—in the Pacific, examples include people in the Pacific Australia Labour Mobility scheme or the New Zealand Recognised Seasonal Employer scheme—or from long term migrants who have made the other country their indefinite home.

We see from this, final, chart that some Pacific island countries have extraordinarily high levels of remittances compared to averages of comparable countries, including other small states:

<object type="image/svg+xml" data='/img/0315-remittances-bar.svg' width='100%'><img src='/img/0315-remittances-bar.png' width='100%'></object>

Interestingly, the three highest countries on this measure are *not* on my list of countries with special access to permanent residency in a large rich country. But as previously mentioned, Samoa and Tonga have particularly strong ties to New Zealand that act partway to such special access. Vanuatu does not, but it is the beneficiary of short term labour schemes. It's also possible that remittances are under-reported in some countries.

For countries like Tonga, it seems likely or at least possible that remittances are coming from long term migrants in New Zealand. After a generation or three, will they stop sending money back to Tonga? Possibly, but as we have seen there is a continual ongoing refreshment in the form of new migrants and it is likely the remittances will continue for the forseeable future. 

One thing is clear&mdash;labour mobility is an enduring feature of the Pacific region that meets multiple groups' needs and leaves a decisive mark on both sending and receiving economies. Remittances are just one most direct part of the economic impact; others include investment ties and human capability development. But I won't go into a literature review on this area.

## There are important policy implications

Why do we care? There are important implications of all this. The context of my original talk was a meeting bringing together heads of National Statistical Offices with heads of national planning, and I wanted to highlight population issues as one area where the links between official statistics and national planning and policy are (or should be) particularly strong. Some of the key economic and planning issues in this regard as I see them include:

* Population projections (including at sub-national level) and the obvious implications for infrastructure and related planning (if and where to build roads, medical centres, schools, etc.)
* Impacts of migration (short and long term) on the working-age population back at home and what planning or policy levers are needed to deal with this, not just lament it
* Social impacts of many working-age people, perhaps disproportionately of one gender, being overseas
* Implications for taxation policy of a large proportion of national income coming in remittances 
* Impact of migration (in either direction) on the net fiscal position (is it working age people coming to our country, paying more tax than they extract in benefits&mdash; or vice versa)
* Specifically, where do retirees end up, and who pays for them
* Impact of returnees from overseas work experiences&mdash;including raised skills and experiences, higher taxation payments, and general impact on national capabilities and capacity
* Impacts on equality back home from different types of people in the diaspora
* What labour market and cultural context should education be preparing young people for?

This aims to be an indicative, rather than comprehensive, list. After all, my aim here is to highlight some issues, not venture into policy advice that I have no mandate for and which would require much more systematic evaluation of options and root causes of the problems.

## Other posts in this series

The seven blog posts in total in this series are set out below. The first six contain R code and data sources for each chart:
* [Visual summaries of population size and growth](/blog/2025/11/30/pacific-population)
* [Net migration](/blog/2025/12/04/pacific-net-migration)
* [Population pyramids](/blog/2026/03/01/pacific-pyramids)
* [World cities with the most Pacific Islanders](/blog/2026/02/16/pacific-cities) 
* [Pacific diaspora](/blog/2026/02/18/pacific-diaspora) 
* [Remittances](/blog/2026/03/08/remittances)
* Tying it all together (this post today)
