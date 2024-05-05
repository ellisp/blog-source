---
layout: post
title: House effects, herding, and the last few days before the election
date: 2019-05-15
tag: 
   - Surveys
   - VotingBehaviour
   - Australia
   - R
description: A few small explorations of idiosyncracies with polls, a couple of days before the Australian federal election.
image: /img/0153-herding.svg
socialimage: https:/freerangestats.info/img/0153-herding.png
category: R
---

So, we're down to the last few days before the Australian federal election, the first one that I've been tracking polls and making forecasts for. I thought I'd address a couple of points raised on Twitter about my forecasts. My forecasts are generally a bit more sceptical of a clean ALP win (ie 76 or more seats in the House of Representatives) than most of the published punditry (like [this "Labor will win this election... virtually unquestionable" piece](https://theconversation.com/labor-will-win-this-election-i-think-thats-virtually-unquestionable-political-scientist-andy-marks-on-ausvotes2019-and-the-key-issues-in-nsw-116273), for instance). However, my forecasts are mostly about uncertainty - I'm not even forecasting a close election as such, as much as saying that we have fairly low level knowledge about what is going to happen, which happens to translate into a 38% chance of a clean ALP win. It might be a biggish win; very little will surprise me. That's a difficult thing to sell to the public.

## House effects

[Kevin Bonham pointed out on Twitter](https://twitter.com/kevinbonham/status/1128081029275586560) that my forecasts assume that house effects (ie the structural under- or over-estimates of voting behaviour by individual polling firms) are the same over time. As surveying firms try to improve their methods, and as public behaviour changes, we would expect to see differences in the "biases" (speaking statistically) of each firm over time. Indeed, the same firm can publishe data simultaneously from surveys that collect data from different modes (eg phone versus online). I haven't attempted to capture those nuances, never mind changes over time as the firms try to fix issues.

This is a definite limitation of my method, and one I'd like to fix for future elections. I also reflected on this issue in my forecasts of the New Zealand election, and even did a [blog post on it](/blog/2017/09/16/time-varying-house-effects); I concluded from the minimal effects seen there that the house effects changed surprisingly slowly so I could ignore the phenomenon and judge each firm on its overall results. That might not be the case for Australian polls, so I should at least try building in changing house effects over time. Unfortunately this will have the impact of increasing even further the uncertainty in translating polling numbers to election results.

While I don't have time for that today, I do have time to at least explore the most likely problem candidate, Roy Morgan. My Bayesian state space model finds a strong over-estimate of the ALP vote in Roy Morgan, comparing it to the other polling firms and to actual election outcomes as far back as I have data. Bonham suggests that more recently, Roy Morgan polls if anything now under-estimate the ALP vote. 

Here's the house effects shown by my current model:

<img src='/img/0153-latest-polling-firm-density.svg' width='100%'>

Let's look at the voting intention reported by the four current main pollsters by election year. Just looking at these density curves certainly does suggest that this time around, Roy Morgan has reversed and is now underestimating ALP vote; the purple density is noticeably to the left of the others:

<img src='/img/0153-morgan-density.svg' width='100%'>

Compare to how far the yellow is to the right of the crowd in the first plot. Sorry for not matching the colours; tempus fugit.

However, I'm not going to draw firm conclusions from this. We can see that Roy Morgan have published relatively few polls this election cycle, and they have mostly been relatively late:

<img src='/img/0153-morgan-2019.svg' width='100%'>

The ALP support now is less than six months ago, so the fact that Roy Morgan's average reported intended vote for ALP is less than the other firms is not as convincing as it might be thought. There are just too few data points to be sure. In effect, my model judges Roy Morgan on their performance over the full period data are available, and overall that data shows an overestimate of ALP vote. It's possible they've fixed the problem, but they'll need to get quite a few runs on the board to overcome their bad history on this.

Having said that, I think there *is* enough data (and other more theoretical considerations) to be confident that house effects in general change over time, so when I can I will address it. I can also say that I think this issue is not important in my current forecasts; excluding Roy Morgan from my data altogether does not lead to a noticeably different result. The real driver of my less-certain-than-normal-pundits' outlook for an ALP win is from the uncertainty my model exists, particularly in translating an uncertain two-party-preferred swing into an even more uncertain set of seat changes.

Here's the R code for the above examination of Roy Morgan's house effect:

*Post continues after code extract*

{% highlight R lineanchors %}
library(ozfedelect)
library(tidyverse)
library(lubridate)

#-------------Has Roy Morgan fixed the pro-ALP overestimate----------------

d <- ozpolls %>%
  filter(firm %in% c("Election result", "Roy Morgan", "Newspoll", "Galaxy", "Ipsos", "Essential")) %>%
  filter(preference_type == "Two-party-preferred") %>%
  filter(party == "ALP") 

d %>%
  filter(firm != "Election result") %>%
  ggplot(aes(x = intended_vote, colour = firm)) +
  geom_density() +
  facet_wrap(~election_year) +
  labs(x = "Intended two-party-preferred vote for the ALP") +
  ggtitle("Differences in polling firms' estimates of ALP vote by election year")

d %>%
  filter(firm != "Election result") %>%
  ggplot(aes(x = mid_date, y = intended_vote, colour = firm)) +
  geom_point() +
  facet_wrap(~firm) +
  ggtitle("Roy Morgan polls for the 2019 election are few and late",
          "We can't conclude the ALP overestimate is fixed from these data points.") +
  labs(x = "Survey mid date",
       y = "Intended two-party-preferred vote for the ALP")
{% endhighlight %}

## Herding

Another issue I and others have been pondering is the low level of variation between polls. Of the last 16 published national polls, 7 have placed the ALP two-party-preferred vote at 51%, 7 at 52%, and one each at 52.5% and 53%. As Mark the Ballot points out, that's less variation than you'd expect if they really were based on random samples. The most likely explanation for this underdispersion is that polling firms are introducing errors in their processing that push their polls towards a consensus view. This is a known phenomenon around the world, as this [2014 article by Nate Silver on FiveThirtyEight](https://fivethirtyeight.com/features/heres-proof-some-pollsters-are-putting-a-thumb-on-the-scale/) demonstrates. The net effect is to reduce the usefulness of polls, by bringing them towards a pundit-based folly of the crowds consensus.

It's not clear if the errors are being introduced accidentally (for example, by unconsciously giving more scrutiny and checks to "suspicious" points that break with the consensus view) or more deliberately through some kind of smoothing or regularisation. Either way, it means that the polling information is worth less than it would be with a more straightforward production process. But it's something we're going to have to live with, and just note yet another reason for being less confident in what we think is going to happen on Saturday - whatever it is that we think!

Here's a comparison of what we've seen in the reported polls versus what we'd expect in actual random samples. I've used Mark the Ballot's assumption of sample sizes of 1,000; in practice, some samples are higher and some smaller, but as we are ignoring non-sampling error I am happy to err a little on the small side for sampling. Also, note that the reported results are (usually) rounded in ways that impact on the distribution of overall results in complicated but unimportant ways. To derive the results below I've mimicked that rounding process in 100,000 simulated samples of 25 polls, all of them rounded to the nearest percent:

<img src='/img/0153-herding.svg' width='100%'>

Here's the code for that:

*Post continues after code extract*

{% highlight R lineanchors %}
#----------How bad is the underdispersion---------
set.seed(123)

d2 <- d %>%
  filter(year(mid_date) == 2019) %>%
  select(mid_date, 
         observed_polls = intended_vote) 

n <- nrow(d2)
reps <- 100000

sims <- matrix(round(rbinom(n * reps, 1000, mean(d2$observed_polls) / 100) / 10),
               ncol = n, nrow = reps)

d4 <- tibble(standard_deviations = apply(sims, 1, sd)) 

d4 %>%
  ggplot(aes(x = standard_deviations)) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = sd(d2$observed_polls), colour = "red") +
  annotate("text", x = 0.45, y = 0.5, hjust = 0, label = paste0(
    "Only ", round(mean(d4 < sd(d2$observed_polls)), 3),
    " of simulated poll\nsequences have a standard\ndeviation less than has been\nobserved in the ",
    n, " polls in 2019."
  )) +
  annotate("text", x = 1, y = 1.5, colour = "red", label = "Observed", hjust = 1) +
  labs(x = paste0("Standard deviations of simulated sequences of ", 
                  n, 
                  " surveys with 1,000 respondents, with survey results rounded")) +
  ggtitle("The polls published in 2019 vary somewhat less than they should if random",
          "The amount of variance is surprisingly low, but not impossibly so.")
{% endhighlight %}

## For the record - forecasts as at 15 May 2019

I find it interesting that some people react to forecasts as though they are normative. I got a number of retweets from rightists - including Australians who proudly wear their Make America Great Again caps to early voting and tweet about, thereby donating cultural scientists all sorts of interesting material - for my most recent update on my election predictions. And a few left-leaning sceptics. Let me state my own intent to keep my own political preferences (which are strong) out of this; I want to avoid the problems that lead to herding of opinion pollsters, for one thing. While I can understand people who don't like to see forecasts that their side is less likely to win they thought (or do like to see forecasts that their own party may have a chance after all), I think we should endeavour to avoid our preferences clouding our judgement.

Here's my forecasts - with big chunks of uncertainty - as at 15 May 2019:

My forecast for the Australian 2019 federal election on 18 May 2019 (with the latest polling data available at 14 May 2019) is a narrow ALP win in their own right (36% chance) or with the presumed support of one or two Green members (47% chance in total). However, a range of other options are very much in play. An 80% prediction interval for the ALP in the House of Representatives is 68 to 80 seats.

The Liberal-National Coalition is likely to end up with between 67 and 78 seats. An outright win is a distinct possibility, and a win with the help of independents or minor parties more so. The Greens are most likely to end up retaining their single seat, but have a fair chance of picking up a second. In total, parties other than the ALP and Coalition are likely to end up with three to six seats.

The range of possibilities with regard to numbers of seats:

<img src='/img/0153-seat-sims.svg' width='100%'>

The predicted two-party-preferred vote:

<img src='/img/0153-latest-model-results.svg' width='100%'>



## The R packages that make this possible

{% highlight R lineanchors %}
thankr::shoulders() %>% 
  mutate(maintainer = str_squish(gsub("<.+>", "", maintainer)),
         maintainer = ifelse(maintainer == "R-core", "R Core Team", maintainer)) %>%
  group_by(maintainer) %>%
  summarise(`Number packages` = sum(no_packages),
            packages = paste(packages, collapse = ", ")) %>%
  arrange(desc(`Number packages`)) %>%
  knitr::kable() %>% 
  clipr::write_clip()
{% endhighlight %}


|maintainer          | Number packages|packages                                                                                                                   |
|:-------------------|---------------:|:--------------------------------------------------------------------------------------------------------------------------|
|Hadley Wickham      |              15|assertthat, dplyr, forcats, ggplot2, gtable, haven, httr, lazyeval, modelr, plyr, rvest, scales, stringr, tidyr, tidyverse |
|R Core Team         |              11|base, compiler, datasets, graphics, grDevices, grid, methods, stats, tools, utils, nlme                                    |
|Yihui Xie           |               5|evaluate, highr, knitr, rmarkdown, xfun                                                                                    |
|Kirill Müller       |               4|DBI, hms, pillar, tibble                                                                                                   |
|Lionel Henry        |               4|purrr, rlang, svglite, tidyselect                                                                                          |
|Winston Chang       |               4|extrafont, extrafontdb, R6, Rttf2pt1                                                                                       |
|Gábor Csárdi        |               3|cli, crayon, pkgconfig                                                                                                     |
|Jim Hester          |               3|glue, withr, readr                                                                                                         |
|Yixuan Qiu          |               3|showtext, showtextdb, sysfonts                                                                                             |
|Dirk Eddelbuettel   |               2|digest, Rcpp                                                                                                               |
|Edzer Pebesma       |               2|sf, units                                                                                                                  |
|Jennifer Bryan      |               2|cellranger, readxl                                                                                                         |
|Peter Ellis         |               2|frs, ozfedelect                                                                                                            |
|Simon Urbanek       |               2|audio, Cairo                                                                                                               |
|Achim Zeileis       |               1|colorspace                                                                                                                 |
|Alex Hayes          |               1|broom                                                                                                                      |
|Brian Ripley        |               1|class                                                                                                                      |
|Brodie Gaslam       |               1|fansi                                                                                                                      |
|Charlotte Wickham   |               1|munsell                                                                                                                    |
|Claus O. Wilke      |               1|cowplot                                                                                                                    |
|David Gohel         |               1|gdtools                                                                                                                    |
|David Meyer         |               1|e1071                                                                                                                      |
|Deepayan Sarkar     |               1|lattice                                                                                                                    |
|James Hester        |               1|xml2                                                                                                                       |
|Jeremy Stephens     |               1|yaml                                                                                                                       |
|Jeroen Ooms         |               1|jsonlite                                                                                                                   |
|Joe Cheng           |               1|htmltools                                                                                                                  |
|Justin Talbot       |               1|labeling                                                                                                                   |
|Kamil Slowikowski   |               1|ggrepel                                                                                                                    |
|Kevin Ushey         |               1|rstudioapi                                                                                                                 |
|Marek Gagolewski    |               1|stringi                                                                                                                    |
|Matthew Lincoln     |               1|clipr                                                                                                                      |
|Max Kuhn            |               1|generics                                                                                                                   |
|Michel Lang         |               1|backports                                                                                                                  |
|Patrick O. Perry    |               1|utf8                                                                                                                       |
|Rasmus Bååth        |               1|beepr                                                                                                                      |
|Roger Bivand        |               1|classInt                                                                                                                   |
|Stefan Milton Bache |               1|magrittr                                                                                                                   |
|Vitalie Spinu       |               1|lubridate                                                                                                                  |
