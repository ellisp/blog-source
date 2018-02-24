---
layout: narrowwithcomments
title: New Zealand general election forecasts
image: /img/gam-vote-predictions.png
socialimage: /img/gam-vote-predictions.png
---
<p></p>

This page shows the results of *combining* the simulations of the two different models I use for predicting the New Zealand election results:

- [Model A](/elections/elections.html), which is based on a generalized additive model with smoothing splines, fit after correcting for polling house effects, to forecast election day party vote - which allows recent trends in changing voting intention (eg the systematic growth or decline in support for a party) to continue towards election day
- a ["state space" model, Model B](/elections/state-space.html), which simultaneously estimates house effects and latent voting intention for each party and assumes that voting intention is a random walk, so the level will not (on average) change in any structural way between now and election day although it will wander around at random.

See the page on [Model B](/elections/state-space.html) for more detail on the differences between the two approaches.

## High level results - combined models
<img src='/img/combined-final-chances-bar.svg' width='100%'>
<img src='/img/combined-final-chances-histogram.svg' width='100%'>
<img src='/img/combined-vote-predictions-density.svg' width='100%'>

## How we get there

### Model A - splines that assume recent trends will continue
<img src='/img/gam-vote-predictions.svg' width='100%'>

### Model B - state space model with random walk latent voting intention
<img src='/img/state-space-ribbons.svg' width='100%'>