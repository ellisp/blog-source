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

The vertical pale grey bars represent confirmed cases, but with a small adjustment for test positivity (ie the proportion of tests for Covid-19 that return positive on any one day). The approach to adjusting for positivity is as described in [this blog post](/blog/2020/05/09/covid-population-incidence), with a power parameter of 0.1. Victoria's test positivity over the last few months is shown in this chart, which makes clear that the proportion of positive tests is low (ie good) by international standards:

<object type="image/svg+xml" data='/img/covid-tracking/victoria-positivity.svg' width='100%'><img src='/img/covid-tracking/victoria-positivity.png' width='100%'></object>

The case numbers used for analysis are collated by [The Guardian](https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505). I use the daily increase in cumulative cases as my initial estimates of incidence. Note that these are sometimes slightly less than the daily figures announced by press release, [because of adjustments to previous days' case numbers](https://twitter.com/NickEvershed/status/1285709049489760256) which cannot be allocated to specific dates.

The code for this analysis of the situation in Victoria is [available on GitHub](https://github.com/ellisp/blog-source/blob/master/_working/covid-tracking/victoria-tracker.R).




