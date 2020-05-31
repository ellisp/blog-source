---
layout: narrowwithcomments
title: General election forecasts
image: /img/nz-elections-2020/state-space-final-chances-bar.svg
socialimage: /img/nz-elections-2020/state-space-final-chances-bar.png
---

## Welcome

This page is simply an index to some election forecasts I have done as a side project. After trying several different avenues, my preferred method of forecasting elections is to develop a weak prior based on a "political science model" (for example, estimating a distribution for the government party's vote given economic conditions) and updating that prior with a Baysian state space model which allows for different pollsters' tendency to under- or over-estimate each parties' vote. This let's me generate a forecast of vote like this:

<object type="image/svg+xml" data='/img/nz-elections-2020/state-space-ribbons.svg' width='100%'><img src='/img/nz-elections-2020/state-space-ribbons.png' width='100%'></object>

... which then gets translated into expected seats by whatever the mechanism of the electoral system in question.

## Forecasts

- [New Zealand 2020](/elections/nz-2020/index.html) (updating regularly)
- [Australia 2019](/elections/oz-2019/index.html)
- [New Zealand 2016](/elections/nz-2016/elections.html)

## Reflections

I strongly recommend reading Nate Silver's reflections on [The Real Story of 2016](https://fivethirtyeight.com/tag/the-real-story-of-2016/) about polls, forecasts and political analysis in the 2016 US Presidential election, much of which is relevant in other electoral situations.  It includes this gem:

> "...there are real shortcomings in how American politics are covered, including pervasive groupthink among media elites, an unhealthy obsession with the insider’s view of politics, a lack of analytical rigor, a failure to appreciate uncertainty, a sluggishness to self-correct when new evidence contradicts pre-existing beliefs, and a narrow viewpoint that lacks perspective from the longer arc of American history."

## Caveat and disclaimer

This page is not associated with any political party, media or commentator.  I have made every effort to provide a transparent, technical probabilistic forecast of the election results and limit any subjective judgement to technical matters relating to model building.  No political judgement or interpretation is to be inferred from these forecasts.  Even more than is always the case, this page has no connection whatsoever to my day job.

Non-politicised corrections, reactions or suggestions are welcomed - use the comments section below (or in the sidebar depending how you are reading this) or [log an issue with the source code repository on GitHub](https://github.com/ellisp/nz-election-forecast/issues).  
