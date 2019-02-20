---
layout: post
title: Seasonality in NZ voting preference?
date: 2019-02-20
tag: 
   - Surveys
   - NewZealand
   - VotingBehaviour
   - R
description: I update the nzelect R package with the latest New Zealand polling data, and use a generalized additive model to look for a seasonal impact on support for the current government.
image: /img/0144-pro-pm.svg
socialimage: http://freerangestats.info/img/0144-pro-pm.png
category: R
---

There was a flurry of activity in the last couple of days on [Twitter](https://twitter.com/Thoughtfulnz/status/1097734918115799040) and the blogosphere, most notably [Thomas Lumley's excellent Stats Chat](https://www.statschat.org.nz/2019/02/19/summer-polling/), relating to whether there is a pro-government bias in surveys of New Zealand voting intention in the summer. As the analysis I've seen used my `nzelect` R package, this motivated me to update it for recent polls. 

## The `nzelect` update

`nzelect` hasn't been updated on CRAN for some time, because about a year ago I made some major changes to the data model for the historical election results by voting place and I haven't been able to complete testing and stabilisation of the result. I do hope to do this some time in the next few months. In the meantime, the version on GitHub has the current polling data, and I intend to keep it current. Political polls are very thin on the ground these days for New Zealand, so that's not too big an ask! I've now spent a bit of time tidying it up and adding the three most recent polls, which I'd previously neglected.

Let's start with the basics. One of the reasons I first put the package together several years ago was to help facilitate analysis of the relatively long run in political opinion. I wanted to lift up analysis from over-interpretation of the last few noisy data points. Here's the expressed voting intention of New Zealanders for the four currently largest parties in Parliament over time:

<img src='/img/0144-four-parties.svg' width='100%'>

It's striking, but unsurprising, how the Greens and New Zealand First support has collapsed since coming into government with Labour (or just before, in the case of the Greens and their disappointing 2017 election campaign). The junior party in a coalition often suffers, as attention and kudos goes to the leaders of the large party (in this case, Prime Minister Jacinda Ardern) and the smaller parties' own base deals with the realities and compromises of being in government.

The other interesting (and disappointing, for statisticians and political scientists) observation from this chart  is how obviously the number of polls has decreased. Topic for another day (or more likely, someone else to write about).

Here's the R code for that chart:

{% highlight R lineanchors %}
# install latest version of nzelect:
devtools::install_github("ellisp/nzelect/pkg1")

# load packages needed including for rest of session
library(nzelect)
library(tidyverse)
library(scales)
library(lubridate)
library(gridExtra)
library(broom)
library(mgcv)

#-----------------------------polling overview---------------------

p1 <- polls %>%
  filter(MidDate > as.Date("2014-11-20") & !is.na(VotingIntention)) %>%
  filter(Party %in% c("National", "Labour")) %>%
  mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
         Party = fct_drop(Party)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention, colour = Party, linetype = Pollster)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(shape = Pollster)) +
  geom_smooth(aes(group = Party), se = FALSE, colour = "grey15", span = .4) +
  scale_colour_manual(values = parties_v, guide = "none") +
  scale_y_continuous("Voting intention", label = percent) +
  scale_x_date("") +
  facet_wrap(~Party, scales = "fixed") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") 

p2 <- polls %>%
  filter(MidDate > as.Date("2014-11-20") & !is.na(VotingIntention)) %>%
  filter(Party %in% c("Green", "NZ First")) %>%
  mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
         Party = fct_drop(Party)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention, colour = Party, linetype = Pollster)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(shape = Pollster)) +
  geom_smooth(aes(group = Party), se = FALSE, colour = "grey15", span = .4) +
  scale_colour_manual(values = parties_v, guide = "none") +
  scale_y_continuous("Voting intention", label = percent) +
  scale_x_date("") +
  facet_wrap(~Party, scales = "fixed") +
  theme(panel.grid.minor = element_blank()) 

grid.arrange(p1, p2)
{% endhighlight %}

There's obvious interest for supporters of the left-of-centre parties in the combined vote for Labour and the Greens. That suggests the importance of this chart:

<img src='/img/0144-lab-green-comb.svg' width='100%'>

(Apologies for red-green colour blind people in the use of these party colours; the Greens are the lowest of the three lines and Labour the middle.)

It's clear that to a significant degree electoral support for the two is substitutable, with the green and red lines moving in scissors-like counter directions at several key times since 2010, with the last 24 months just the most dramatic example.

It's also clear that the combined support for the centre-left in New Zealand is pretty strong, recovered to a point it hasn't been since several years before the end of the government led by Helen Clarke in the 2000s.

Here's the code for that chart. Note how I use the dates of elections to make a simple data frame of who is in power when, for the background rectangles; and leverage the `parties_v` vector of colours in `nzelect` to allocate the official party colours to both the parties' lines and to the background fill.

{% highlight R lineanchors %}
#----------------long run-------------
elections <- polls %>%
  filter(Pollster == "Election result") %>%
  distinct(MidDate) %>%
  rename(start_date = MidDate) %>%
  mutate(end_date = lead(start_date, default = as.Date("2020-10-01")),
         pm = c("Labour", "Labour", "National", "National", "National", "Labour"))

p3 <- polls %>%
  as_tibble %>%
  filter(!is.na(VotingIntention)) %>%
  filter(Party %in% c("Green", "Labour")) %>%
  select(Party, MidDate, VotingIntention, Pollster) %>%
  spread(Party, VotingIntention) %>%
  mutate(Combined = Labour + Green) %>%
  gather(Party, VotingIntention, -MidDate, -Pollster) %>%
  mutate(Party = fct_relevel(Party, c("Combined", "Labour"))) %>%
  ggplot() +
  geom_rect(data = elections, ymin = -Inf, ymax = Inf, alpha = 0.1, 
            aes(xmin = start_date, xmax = end_date, fill = pm)) +
  geom_line(aes(x = MidDate, y = VotingIntention, colour = Party)) +
  scale_colour_manual(values = c(parties_v, "Combined" = "black")) +
  scale_fill_manual(values = parties_v) +
  labs(x = "Survey date", y = "Voting intention", 
       colour = "Surveyed voting intention:", fill = "Prime Minister's party:") +
  scale_y_continuous(label = percent_format(accuracy = 2)) +
  ggtitle("Voting intention in New Zealand over a longer period than usually presented",
          "Labour, Greens, and combined")
{% endhighlight %}

## Seasonality

Now, on to the question of seasonality. I don't have much to add to the analysis of David Hood on Twitter and Thomas Lumley on StatsChat; I basically agree with their conclusions. I have the advantage of a few more polls because of the update to `nzelect` this morning.

Here is the expressed intended vote for the party of the Prime Minister over time:

<img src='/img/0144-pro-pm.svg' width='100%'>

The blue line for the National Party is higher than the equivalent for Labour Prime Ministers because National has tended to form a larger proportion of its governing coalitions than Labour in this time period. I could (and probably should) have added the intended vote for all parties currently in coalition government, but this is actually a pretty complicated thing to do so I've gone for the simpler approach for now. So long as we don't make simplistic comparisons forgetting that New Zealand has a proportional representation system, that is ok for our purposes.
		  
We obviously can't tell anything from this chart about the seasonality; there are two many data points and too much noise. Actually, one thing we can say for sure is that seasonality isn't strong. For very seasonal data such as tourist numbers, the seasonality would be obvious even in a chart like this.

To see if there is a subtle seasonality effect, I tried modelling voting intention for the Prime Minister's party on the month of the year, controlling for the party in power, a smooth trend over time, and whether or not it is an election month (otherwise September and November, with five of the six elections in this period, would certainly cloud the data). I used (as I nearly always do in this situation) Simon Woods excellent `mgcv` R package.

Having done that, we can approximate confidence intervals for the impact on voting preference of the month of the year. The next chart shows those estimates:

<img src='/img/0144-seasonality.svg' width='100%'>

As the title says, it's weak evidence of a weak effect, which might be around half a percentage point more positive for the Prime Minister's party in the summer months than it is in June. Or it might be more than that, or even negative.

Here's the code for that model and the last two charts:
		  
{% highlight R lineanchors %}
#------------------------seasonality of govt support------------------
# data frame of voting intention for the lead party of the government 
d <- polls %>%
  as_tibble() %>%
  left_join(elections, by = c("MidDate" = "start_date")) %>%
  select(-(WikipediaDates:EndDate)) %>%
  fill(pm, end_date) %>%
  # limit to the party that has the Prime Minister:
  filter(Party == pm) %>%
  mutate(survey_month = as.character(month(MidDate, label = TRUE)),
         survey_month = fct_relevel(survey_month, "Jun"),
         election_month = (month(MidDate) == month(end_date) & year(MidDate) == year(end_date)))

ggplot(d) +
  geom_rect(data = elections, ymin = -Inf, ymax = Inf, alpha = 0.1, 
           aes(xmin = start_date, xmax = end_date, fill = pm)) +
  geom_line(aes(x = MidDate, y = VotingIntention, colour = Party, group = end_date)) +
  scale_colour_manual(values = c(parties_v, "Combined" = "black")) +
  scale_fill_manual(values = parties_v) +
  labs(x = "Survey date", y = "Voting intention for PM's party", 
       colour = "Surveyed voting intention:", fill = "Prime Minister's party:") +
  scale_y_continuous(label = percent_format(accuracy = 2)) +
  ggtitle("Voting intention for the Prime Minister's party")

# model of vote for govt's lead party, controlling for trend (the smooth term) and for which
# party is in government, looking for effects from month of year.
mod <- gam(VotingIntention ~ election_month + survey_month +  Party + s(as.numeric(MidDate)), data = d)
anova(mod) # yes, the survey_month is 'statistically significant'

tibble(variable = names(coef(mod)),
       estimate = coef(mod),
       se = summary(mod)$se) %>%
  # very approximate confidence intervals:
  mutate(lower = estimate - se * 1.96,
         upper = estimate + se * 1.96) %>%
  filter(grepl("month", variable)) %>%
  mutate(survey_month = gsub("survey_month", "", variable)) %>%
  mutate(survey_month = factor(survey_month, 
                               levels = c("election_monthTRUE", as.character(month(c(7:12, 1:6), label = TRUE))))) %>%
  ggplot(aes(x = lower, xend = upper, y = survey_month, yend = survey_month)) +
  geom_vline(xintercept = 0, colour = "red") +
  geom_segment(size = 3, colour = "steelblue", alpha = 0.7) +
  geom_point(aes(x = estimate)) +
  scale_x_continuous(label = percent) +
  labs(x = "Estimated impact (in percentage points) on intended vote for PM's party, relative to June",
       y = "Survey month") +
  ggtitle("Seasonality of voting preference for the New Zealand PM's party?",
          "Weak evidence of a very weak pro-government effect from late Spring to early Autumn")
{% endhighlight %}

It's very motivating to see others using the `nzelect` package. Please tag me in Twitter, or let me know some other way, if you use this and it will encourage me for further enhancements, and to get the new version with better historical data onto CRAN!

## Thanksgiving

I'm going to try to get into the habit of this at the end of each blog post. Without the hard work, innovation and sheer smarts of the open source community my blog (and many much more important things!) wouldn't be possible. Here are just those in the R world whose code I used in this session (not all of it made it into the excerpt above, but that's all the more reason to give thanks below).

{% highlight R lineanchors %}
thankr::shoulders() %>% knitr::kable() %>% clipr::write_clip()
{% endhighlight %}

|maintainer                                      | no_packages|packages                                                                                                                                      |
|:-----------------------------------------------|-----------:|:---------------------------------------------------------------------------------------------------------------------------------------------|
|Hadley Wickham <hadley@rstudio.com>             |          17|assertthat, dplyr, forcats, ggplot2, gtable, haven, httr, lazyeval, modelr, plyr, rvest, scales, stringr, testthat, tidyr, tidyverse, usethis |
|R Core Team <R-core@r-project.org>              |          10|base, compiler, datasets, graphics, grDevices, grid, methods, stats, tools, utils                                                             |
|Gábor Csárdi <csardi.gabor@gmail.com>           |           9|callr, cli, crayon, desc, pkgconfig, processx, ps, remotes, sessioninfo                                                                       |
|Kirill Müller <krlmlr+r@mailbox.org>            |           6|bindr, bindrcpp, hms, pillar, rprojroot, tibble                                                                                               |
|Jim Hester <james.hester@rstudio.com>           |           4|devtools, pkgbuild, pkgload, readr                                                                                                            |
|Winston Chang <winston@stdout.org>              |           4|extrafont, extrafontdb, R6, Rttf2pt1                                                                                                          |
|Jim Hester <james.f.hester@gmail.com>           |           3|fs, glue, withr                                                                                                                               |
|Lionel Henry <lionel@rstudio.com>               |           3|purrr, rlang, tidyselect                                                                                                                      |
|Dirk Eddelbuettel <edd@debian.org>              |           3|digest, Rcpp, x13binary                                                                                                                       |
|Yixuan Qiu <yixuan.qiu@cos.name>                |           3|showtext, showtextdb, sysfonts                                                                                                                |
|Jeroen Ooms <jeroen@berkeley.edu>               |           2|curl, jsonlite                                                                                                                                |
|Yihui Xie <xie@yihui.name>                      |           2|knitr, xfun                                                                                                                                   |
|R-core <R-core@R-project.org>                   |           1|nlme                                                                                                                                          |
|Vitalie Spinu <spinuvit@gmail.com>              |           1|lubridate                                                                                                                                     |
|Michel Lang <michellang@gmail.com>              |           1|backports                                                                                                                                     |
|Patrick O. Perry <patperry@gmail.com>           |           1|utf8                                                                                                                                          |
|Simon Wood <simon.wood@r-project.org>           |           1|mgcv                                                                                                                                          |
|Achim Zeileis <Achim.Zeileis@R-project.org>     |           1|colorspace                                                                                                                                    |
|Baptiste Auguie <baptiste.auguie@gmail.com>     |           1|gridExtra                                                                                                                                     |
|Gabor Csardi <csardi.gabor@gmail.com>           |           1|prettyunits                                                                                                                                   |
|Peter Ellis <peter.ellis2013nz@gmail.com>       |           1|nzelect                                                                                                                                       |
|Simon Urbanek <Simon.Urbanek@r-project.org>     |           1|Cairo                                                                                                                                         |
|James Hester <james.hester@rstudio.com>         |           1|xml2                                                                                                                                          |
|Justin Talbot <justintalbot@gmail.com>          |           1|labeling                                                                                                                                      |
|Torsten Hothorn <Torsten.Hothorn@R-project.org> |           1|mvtnorm                                                                                                                                       |
|Christoph Sax <christoph.sax@gmail.com>         |           1|seasonal                                                                                                                                      |
|Jennifer Bryan <jenny@rstudio.com>              |           1|readxl                                                                                                                                        |
|Kevin Ushey <kevin@rstudio.com>                 |           1|rstudioapi                                                                                                                                    |
|Max Kuhn <max@rstudio.com>                      |           1|generics                                                                                                                                      |
|Stefan Milton Bache <stefan@stefanbache.dk>     |           1|magrittr                                                                                                                                      |
|Martin Maechler <mmaechler+Matrix@gmail.com>    |           1|Matrix                                                                                                                                        |
|Charlotte Wickham <cwickham@gmail.com>          |           1|munsell                                                                                                                                       |
|Brodie Gaslam <brodie.gaslam@yahoo.com>         |           1|fansi                                                                                                                                         |
|Matthew Lincoln <matthew.d.lincoln@gmail.com>   |           1|clipr                                                                                                                                         |
|Gavin L. Simpson <ucfagls@gmail.com>            |           1|gratia                                                                                                                                        |
|Marek Gagolewski <gagolews@rexamine.com>        |           1|stringi                                                                                                                                       |
|Jeremy Stephens <jeremy.f.stephens@vumc.org>    |           1|yaml                                                                                                                                          |
|Brian Ripley <ripley@stats.ox.ac.uk>            |           1|MASS                                                                                                                                          |
|Deepayan Sarkar <deepayan.sarkar@r-project.org> |           1|lattice                                                                                                                                       |
|Claus O. Wilke <wilke@austin.utexas.edu>        |           1|cowplot                                                                                                                                       |
|Rasmus Bååth <rasmus.baath@gmail.com>           |           1|beepr                                                                                                                                         |
|Jennifer Bryan <jenny@stat.ubc.ca>              |           1|cellranger                                                                                                                                    |
|Alex Hayes <alexpghayes@gmail.com>              |           1|broom                                                                                                                                         |
|Simon Urbanek <simon.urbanek@r-project.org>     |           1|audio                                                                                                                                         |
|Jim Hester <jim.hester@rstudio.com>             |           1|memoise                                                                                                                                       |
