---
layout: post
title: Bayesian state space modelling of the Australian 2019 election
date: 2019-03-02
tag: 
   - Surveys
   - Australia
   - VotingBehaviour
   - Stan
   - R
description: I tidy up Australian polling data back to 2007 and produce a statistical model of two-party-preferred vote for the coming election.
image: /img/0145-model.svg
socialimage: http://freerangestats.info/img/0145-model.png
category: R
---

So I've been back in Australia for five months now. While things have been very busy in my new role at [Nous Group](http://nousgroup.com.au/), it's not so busy that I've failed to notice there's a Federal election due some time by November this year. I'm keen to apply some of the techniques I used in New Zealand in the richer data landscape (more polls, for one) and complex environment of Australian voting systems.

## Polling data

The Australian Electoral Commission has wonderful, highly detailed data on actual results, which I'll doubtless be coming to at some point. However, I thought for today I'd start with the currency and perpetual conversation-making power (at least in the media) of polling data. 

There's no convenient analysis-ready collection of Australian polling data that I'm aware of. I used similar methods to what's behind my `nzelect` package to grab these survey results from Wikipedia where it is compiled by some anonymous benefactor, from the time of the 2007 election campaign until today. 

Thanks are owed to Emily Kothe who did a bunch of this scraping herself for 2010 and onwards and put the results on GitHub (and on the way motivated me to [develop a date parser](https://stackoverflow.com/questions/54782162/parsing-complicated-date-column/54782945#54782945) for the horror that is Wikipedia's dates), but in the end I started from scratch so I had all my own code convenient for doing updates, as I'm sure I'll be wanting. 

All the [code behind this post](https://github.com/ellisp/ozfedelect) is in its own GitHub repository. It covers grabbing the data, fitting the model I'll be talking about soon, and the graphics for this post. That repo is likely to grow as I do more things with Australian voting behaviopur data.

Here's how that polling data looks when you put it together:

<img src='/img/0145-first-pref.svg' width='100%'>

Notes on the abbreviations of Australian political parties in that chart:

- **ONP** ~ "Pauline Hanson's One Nation" - nationalist, socially conservative, right-wing populism
- **Oth** ~ Other parties
- **Grn** ~ "Australian Greens" ~ left wing, environment and social justice focus
- **ALP** ~ "Australian Labor Party" ~ historically the party of the working person, now the general party of the centre left
- **Lib/Nat** ~ "Liberal Party" or "National Party" ~ centre and further right wing, long history of governing in coalition (and often conflated in opinion polling, hence the aggregation into one in this chart)

I'm a huge believer in looking at polling data in the longer term, not just focusing on the current term of government and certainly not just today's survey release. The chart above certainly tells some of the story of the last decade or so; even a casual observer of Australian politics will recognise some of the key events, and particularly the comings and goings of Prime Ministers, in this chart.

Prior to 2007 there's polling data available in Simon Jackman's `pscl` package which has functionality and data relating to political science, but it only covers the first preference of voters so I haven't incorporated it into my cleaned up data. I need both the first preference and the estimated two-party-preference of voters. 

*(Note to non-Australian readers - Australia has a Westminster-based political system, with government recommended to the Governor General by whomever has the confidence of the lower house, the House of Representatives; which is electorate based with a single-transferrable-vote aka "Australian vote" system. And if the USA could just adopt something as sensible as some kind of preferential voting system, half my Twitter feed would probably go quiet).*

## Two-party-preferred vote

For my introduction today to analysis with this polling data, I decided to focus on the minimal simple variable for which a forecast could be credibly seen as a forecast of the outcome on election day, whenever it is. I chose the two-party-preferred voting intention for the Australian Labor Party or ALP.  We can see that this is pretty closely related to how many seats they win in Parliament:

<img src='/img/0145-2pp-seats.svg' width='100%'>

The vertical and horizontal blue lines mark 50% of the vote and of the seats respectively.

US-style gerrymanders generally don't occur in Australia any more, because of the existence of an independent electoral commission that draws the boundaries. So winning on the two-party-preferred national vote generally means gaining a majority in the House of Representatives. 

Of course there are no guarantees; and with a electoral preference that is generally balanced between the two main parties even a few accidents of voter concentration in the key electorates can make a difference. This possibility is enhanced in recent years with a few more seats captured by smaller parties and independents:

<img src='/img/0145-other-parties.svg' width='100%'>

All very interesting context.

## State space modelling

My preferred model of the two I used for the last New Zealand election was a Bayesian state space model. These are a standard tool in political science now, and I've written about them in both the [Australian](blog/2017/06/24/oz-polls-statespace) and [New Zealand](/blog/2017/09/16/time-varying-house-effects) context.

To my knowledge, the seminal paper on state space modelling of voting intention based on an observed variety of polling data is [Jackman's "Pooling the Polls Over an Election Campaign"](http://eppsac.utdallas.edu/files/jackman/CAJP%2040-4%20Jackman.pdf). I may be wrong; happy to be corrected. I've made a couple of blog posts out of [replicating some of Jackman's work](/blog/2017/07/09/oz-polls-revisited) with first preference intention for the ALP  in the 2007 election. In fact, this was one of my self-imposed homework tasks in learning to use [Stan](https://mc-stan.org/), the wonderfully expressive statistcal modelling and high-performance statistical computation tool and probability programming language.

My state space model of the New Zealand electorate was considerably more complex than I need today, because in New Zealand I needed to model (under proportional representation) the voting intention for multiple parties at once. Whereas today I can focus on just two-party-preferred vote for either of the main blocs. Obviously a better model is possible, but not today!

The essence of this modelling approach is that we theorise the existence of an unobserved latent voting intention, which is measured imperfectly and irregularly by opinion poll surveys. These surveys have sampling error and other sources of "total survey error", including "house effects" or statistical tendencies to over- or under-estimate vote in particular ways. Every few years, the true voting intention manifests itself in an actual election.

Using modern computational estimation methods we can estimate the daily latent voting intention of the public based on our imperfect observations, and also model the process of change in that voting intention over time and get a sense of the plausibility of different outcomes in the future. Here's what it looks like for the 2019 election:

<img src='/img/0145-model.svg' width='100%'>

This all seems plausible and I'm pretty happy with the way the model works. The [model specification written in Stan](https://github.com/ellisp/ozfedelect/blob/master/model-2pp/model-2pp.stan) and the [data management in R](https://github.com/ellisp/ozfedelect/blob/master/model-2pp/model-2pp.R) are both available on GitHub.

An important use for a statistical model in my opinion is to reinforce how uncertain we should be about the world. I like the representation above because it makes clear, in the final few months of modelled voting intention out to October or November 2019, how much change is plausible and consistent with past behaviour. So anyone who feels certain of the election outcome should have a look at the widening cone of uncertainty on this chart and have another think.

A particularly useful side effect of this type of model is statistical estimates of the over- or under-estimation of different survey types or sources. Because I've confronted the data with four successive elections we can get a real sense of what is going on here. This is nicely shown in this chart:

<img src='/img/0145-density-d.svg' width='100%'>

We see the tendency of Roy Morgan polls to overestimate the ALP vote by one or two percentage points, and of YouGov to underestimate it. These are interesting and important findings (not new to this blog post though). Simple aggregations of polls can't incorporate feedback from election results in this way (although of course experienced people routinely make more ad hoc adjustments).

A more sophisticated model would factor in change over time in polling firms methods and results, but again that would take me well beyond the scope of this blog post.

Looking forward to some more analysis of election issues, including of other data sources and of other aspects, over the next few months.

Here's a list of the contributors to R that made today's analysis possible:

{% highlight R lineanchors %}
thankr::shoulders() %>% knitr::kable() %>% clipr::write_clip()
{% endhighlight %}

|maintainer                                      | no_packages|packages                                                                                                                             |
|:-----------------------------------------------|-----------:|:------------------------------------------------------------------------------------------------------------------------------------|
|Hadley Wickham <hadley@rstudio.com>             |          16|assertthat, dplyr, ellipsis, forcats, ggplot2, gtable, haven, httr, lazyeval, modelr, plyr, rvest, scales, stringr, tidyr, tidyverse |
|R Core Team <R-core@r-project.org>              |          12|base, compiler, datasets, graphics, grDevices, grid, methods, parallel, stats, stats4, tools, utils                                  |
|Gábor Csárdi <csardi.gabor@gmail.com>           |           6|callr, cli, crayon, pkgconfig, processx, ps                                                                                          |
|Winston Chang <winston@stdout.org>              |           4|extrafont, extrafontdb, R6, Rttf2pt1                                                                                                 |
|Yihui Xie <xie@yihui.name>                      |           4|evaluate, knitr, rmarkdown, xfun                                                                                                     |
|Kirill Müller <krlmlr+r@mailbox.org>            |           4|DBI, hms, pillar, tibble                                                                                                             |
|Dirk Eddelbuettel <edd@debian.org>              |           3|digest, inline, Rcpp                                                                                                                 |
|Lionel Henry <lionel@rstudio.com>               |           3|purrr, rlang, tidyselect                                                                                                             |
|Jeroen Ooms <jeroen@berkeley.edu>               |           2|curl, jsonlite                                                                                                                       |
|Jim Hester <james.hester@rstudio.com>           |           2|pkgbuild, readr                                                                                                                      |
|Ben Goodrich <benjamin.goodrich@columbia.edu>   |           2|rstan, StanHeaders                                                                                                                   |
|Jim Hester <james.f.hester@gmail.com>           |           2|glue, withr                                                                                                                          |
|Vitalie Spinu <spinuvit@gmail.com>              |           1|lubridate                                                                                                                            |
|Deepayan Sarkar <deepayan.sarkar@r-project.org> |           1|lattice                                                                                                                              |
|Gabor Csardi <csardi.gabor@gmail.com>           |           1|prettyunits                                                                                                                          |
|Patrick O. Perry <patperry@gmail.com>           |           1|utf8                                                                                                                                 |
|Jennifer Bryan <jenny@stat.ubc.ca>              |           1|cellranger                                                                                                                           |
|Michel Lang <michellang@gmail.com>              |           1|backports                                                                                                                            |
|Simon Jackman <simon.jackman@sydney.edu.au>     |           1|pscl                                                                                                                                 |
|Jennifer Bryan <jenny@rstudio.com>              |           1|readxl                                                                                                                               |
|Kevin Ushey <kevin@rstudio.com>                 |           1|rstudioapi                                                                                                                           |
|Justin Talbot <justintalbot@gmail.com>          |           1|labeling                                                                                                                             |
|Simon Potter <simon@sjp.co.nz>                  |           1|selectr                                                                                                                              |
|Jonah Gabry <jsg2201@columbia.edu>              |           1|loo                                                                                                                                  |
|Charlotte Wickham <cwickham@gmail.com>          |           1|munsell                                                                                                                              |
|Alex Hayes <alexpghayes@gmail.com>              |           1|broom                                                                                                                                |
|Joe Cheng <joe@rstudio.com>                     |           1|htmltools                                                                                                                            |
|Baptiste Auguie <baptiste.auguie@gmail.com>     |           1|gridExtra                                                                                                                            |
|Luke Tierney <luke-tierney@uiowa.edu>           |           1|codetools                                                                                                                            |
|Henrik Bengtsson <henrikb@braju.com>            |           1|matrixStats                                                                                                                          |
|Peter Ellis <peter.ellis2013nz@gmail.com>       |           1|frs                                                                                                                                  |
|Simon Garnier <garnier@njit.edu>                |           1|viridisLite                                                                                                                          |
|Brodie Gaslam <brodie.gaslam@yahoo.com>         |           1|fansi                                                                                                                                |
|Brian Ripley <ripley@stats.ox.ac.uk>            |           1|MASS                                                                                                                                 |
|R-core <R-core@R-project.org>                   |           1|nlme                                                                                                                                 |
|Stefan Milton Bache <stefan@stefanbache.dk>     |           1|magrittr                                                                                                                             |
|Marek Gagolewski <marek@gagolewski.com>         |           1|stringi                                                                                                                              |
|James Hester <james.hester@rstudio.com>         |           1|xml2                                                                                                                                 |
|Max Kuhn <max@rstudio.com>                      |           1|generics                                                                                                                             |
|Simon Urbanek <Simon.Urbanek@r-project.org>     |           1|Cairo                                                                                                                                |
|Jeremy Stephens <jeremy.f.stephens@vumc.org>    |           1|yaml                                                                                                                                 |
|Achim Zeileis <Achim.Zeileis@R-project.org>     |           1|colorspace                                                                                                                           |


