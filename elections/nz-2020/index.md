---
layout: narrowwithcomments
title: NZ 2020 - general election forecasts
image: /img/nz-elections-2020/state-space-final-chances-bar.svg
socialimage: https:/freerangestats.info/img/nz-elections-2020/state-space-final-chances-bar.png
---

**Last updated 16 October 2020**

## Near-certainty of a Labour-led government after 2020 election

This chart shows the estimated probability of the key possible governing combinations after the coming New Zealand general election, which will be held on Saturday 17 October 2020.

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-final-chances-bar.svg' width='100%'><img src='/img/nz-elections-2020/state-space-final-chances-bar.png' width='100%'></object>

Barring a major polling error or a historically rapid change of public mood, the new government will be Labour-led and not need New Zealand First as part of their coalition (of course, they may negotiate a broader coalition than needed anyway). There is a modest chance that Labour will not need the Greens in a coalition.

Those chances are based on the possibility of a combination of parties having a majority in Parliament. There will probably be 120 seats in Parliament, but there is a possibility of an additional seat under certain combinations of circumstances, which are dealt with in the model. The chart below shows the full distribution of results for selected possible combinations of parties:

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-results-density.svg' width='100%'><img src='/img/nz-elections-2020/state-space-results-density.png' width='100%'></object>

## Trends in latent voting intention

The forecast starts with a loose prior (t distribution with four degrees of freedom) for the vote for the Labour Party. My starting position, before any polling data is in, is that the swing against the party with the incumbent Prime Minister will be drawn from the historical distribution of that swing: about 1.3% drop in vote for their party with a standard deviation of about 3.2%. The fat-tailed t distribution means we should expect outliers up to three standard deviations from this mean swing without being terribly surprised.

When that prior is combined with a latent state space model based on polling data, we see this trend, and an expected range of voting possibilities  by election day:

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-ribbons.svg' width='100%'><img src='/img/nz-elections-2020/state-space-ribbons.png' width='100%'></object>

Our model delivers a probability distribution for the actual party vote on election day, shown in the next chart. This is basically a slice through the above ribbon chart, showing the range of expected party vote on just that one final day:

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-vote-predictions-density.svg' width='100%'><img src='/img/nz-elections-2020/state-space-vote-predictions-density.png' width='100%'></object>

These party votes are converted to numbers of seats in Parliament via methods that allow for the possibility of small parties winning electorate seats (eg ACT in Epsom; Maori Party in the Maori Electorates).

## House effects

The Bayesian method used to estimate this model allows the "house effects" of each pollster-party combination to be estimated. These are not interesting in themselves (unless you are a pollster) but are a useful nuisance-factor to control for. The point estimates of each pollster's house effects are shown in this chart:

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-house-effects.svg' width='100%'><img src='/img/nz-elections-2020/state-space-house-effects.png' width='100%'></object>

One of the strongest trends in house effects is that opinion polls tend to overestimate the actual vote for the Green Party.

## More detailed seat distribution for individual parties and combinations

This final chart shows the distribution of seats expected for a wider range of parties and combinations than the first few visualisations presnted earlier.

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-final-chances-histogram.svg' width='100%'><img src='/img/nz-elections-2020/state-space-final-chances-histogram.png' width='100%'></object>


## Source code and thanks

These forecasts are written in open source programming languages: [R](https://www.r-project.org/) and [Stan](https://mc-stan.org/). It wouldn't be possible without the efforts of the enormous and positive community of developers for each of those tools.

Full references for the underlying core environment:
 
- R Core Team (2020). *R: A language and environment for statistical computing.* R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
- Bob Carpenter, Andrew Gelman, Matthew D. Hoffman, Daniel Lee, Ben Goodrich, Michael Betancourt, Marcus Brubaker, Jiqiang Guo, Peter Li, and Allen Riddell. 2017. "Stan: A probabilistic programming language." *Journal of Statistical Software* 76(1). DOI 10.18637/jss.v076.i01
- Stan Development Team (2020). *RStan: the R interface to Stan.* R package version 2.19.3.  http://mc-stan.org/.
- Wickham et al., (2019). Welcome to the tidyverse. *Journal of Open Source Software*, 4(43),1686, https://doi.org/10.21105/joss.01686

My source code for these forecasts:

- Source code for [the forecasting preparation and model](https://github.com/ellisp/nz-election-forecast)
- Source code for [the `nzelect` R package](https://github.com/ellisp/nzelect) which holds the polling and election results data (there's also a version on CRAN but it's out of date at the moment)

I also depend on some anonymous contributor/s to Wikipedia who update/s [this page with polling data](https://en.wikipedia.org/wiki/Opinion_polling_for_the_2020_New_Zealand_general_election).

## Reflections

I strongly recommend reading Nate Silver's reflections on [The Real Story of 2016](https://fivethirtyeight.com/tag/the-real-story-of-2016/) about polls, forecasts and political analysis in the 2016 US Presidential election, much of which is relevant in other electoral situations.  It includes this gem:

> "...there are real shortcomings in how American politics are covered, including pervasive groupthink among media elites, an unhealthy obsession with the insider’s view of politics, a lack of analytical rigor, a failure to appreciate uncertainty, a sluggishness to self-correct when new evidence contradicts pre-existing beliefs, and a narrow viewpoint that lacks perspective from the longer arc of American history."

## Caveat and disclaimer

This page is not associated with any political party, media or commentator.  I have made every effort to provide a transparent, technical probabilistic forecast of the election results and limit any subjective judgement to technical matters relating to model building.  No political judgement or interpretation is to be inferred from these forecasts.  Even more than is always the case, this page has no connection whatsoever to my day job.

Non-politicised corrections, reactions or suggestions are welcomed - use the comments section below (or in the sidebar depending how you are reading this) or [log an issue with the source code repository on GitHub](https://github.com/ellisp/nz-election-forecast/issues).  
