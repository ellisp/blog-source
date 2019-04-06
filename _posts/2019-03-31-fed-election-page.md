---
layout: post
title: Website with Australian federal election forecasts
date: 2019-03-31
tag: 
   - Australia
   - VotingBehaviour
   - R
description: I've put up a page with my current Australian federal election forecasts for the House of Representatives, which I'll keep up to date until the election comes.
image: /img/ozpolls/seat-sims.svg
socialimage: http://freerangestats.info/img/ozpolls/seat-sims.png
category: R
---

## The election forecasts

Building on my recent blog posts, I've put up a [page dedicated to forecasts of the coming Australian federal election](/elections/oz/index.html). It takes the state space model of two-party-preferred vote from my [first blog on polls leading up to this election](/blog/2019/03/02/aust-election-1), and combines it with a more nuanced understanding of the seats actually up for grabs in this election than I'd been able to develop for [my second blog, on swings](/blog/2019/03/11/aust-election-2-swings). For the current margins at divisional level after the various electoral distributions, I'm drawing on this [excellent summary by the ABC](https://www.abc.net.au/news/elections/federal-redistribution-2018/).

To cut to the chase, here's my prediction - 76 percent chance of ALP being able to form government by themselves:

<img src='/img/ozpolls/seat-sims.svg' width='100%'>

Because the forecast is built on division-level simulations of what will happen when local randomness adds to an uncertain prediction of two-party-preferred swing, I also get probabilistic forecasts for each individual seat. This lets me produce charts like this one:

<img src='/img/ozpolls/SA.svg' width='100%'>

...which shows what is likely to happen seat by seat when we get the actual election.

## The `ozfedelect` R package

The `ozfedelect` R package continues to grow. Just today, I've added to it:

- a vector of colours for the Australian political parties involved in my forecasting
- a useful data frame `oz_pendulum_2019` of the margins of the various House of Representatives seats going in to the 2019 election.
- update of polling data.

All this is in addition to the historical polling and division-level election results it already contains.

## Code for these forecasts

The code for conducting the forecasts or just installing `ozfedelect` for other purposes is [available in GitHub](https://github.com/ellisp/ozfedelect). The `ozfedelect` GitHub repository not only will build the `ozfedelect` R package from scratch (ie downloading polling data from Wikipedia and election results from the Australian Electoral Commission) it also has the R and Stan code for fitting the model of two-party-preferred vote and turning it into division-level simulated results.

It should be regarded as a work in progress. Comments and suggestions are warmly welcomed!