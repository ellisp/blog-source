---
layout: narrowwithcomments
title: Tracking Covid-19 in Australia
image: /img/covid-tracking/victoria-latest.svg
socialimage: /img/covid-tracking/victoria-latest.png
---


## The latest Covid-19 situation in Victoria, Australia

<object type="image/svg+xml" data='/img/covid-tracking/victoria-latest.svg' width='100%'><img src='/img/covid-tracking/victoria-latest.png' width='100%'></object>

This analysis is done with the EpiNow2 R package. It takes into account estimated delays from infection, incubation and reporting. The approach is described in [this blog post](/blog/2020/07/18/victoria-r-convolution) from July 2020. 

For each of the three time series shown, the green segment on the left shows a period of time that is substantively settled. The brown/orange segment is a "nowcast" - still heavily impacted by delays in incubation and reporting, but with at least some partial data. The final blue segment is a forecast.

The darker part of each ribbon reflects the estimated 50% credibility interval - the range within which a particular variable (infections, cases, or effective reproduction number) is believed to have a 50% chance of occurring, were the truth to be known. The wider, paler part of each ribbon is a 90% credibility interval.

The code for this particular regular analysis is [available on GitHub](https://github.com/ellisp/blog-source/blob/master/_working/covid-tracking/victoria-tracker.R).




