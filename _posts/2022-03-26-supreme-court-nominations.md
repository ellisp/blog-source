---
layout: post
title: Smoothing charts of Supreme Court Justice nomination results
date: 2022-03-26
tag: 
   - DataVisualization
description: Sometimes a Twitter storm of chart-shaming is unfair, mean, and frankly misguided. I reproduce and defend a chart originally produced by FiveThirtyEight to illustrate changes over time in how nominations for US Supreme Court Justices have been voted on in the Senate.
image: /img/0230-gam-alone-with-withdrawn.svg
socialimage: http://freerangestats.info/img/0230-gam-alone-with-withdrawn.png
category: R
---

## Sad about Twitter pile-ons

So this is a blog post about smoothing data that has been generated over time, and a bit about Twitter being mean. 

To cut to the chase, here's my own version of the data in question. This is the Senate vote for nominations to the US Supreme Court, from 1789 onwards:

<object type="image/svg+xml" data='/img/0230-gam-alone-with-withdrawn.svg' width='100%'><img src='/img/0230-gam-alone-with-withdrawn.png' width='100%'></object>

The source data is [published by the US senate](https://www.senate.gov/legislative/nominations/SupremeCourtNominations1789present.htm) as a webpage (code to scrape this is below). The plot above isn't my idea though, I've stolen it from political / sports / data website [FiveThirtyEight](https://fivethirtyeight.com/features/americans-broadly-want-the-senate-to-confirm-ketanji-brown-jackson-to-the-supreme-court/).  The FiveThirtyEight version came to my attention via Twitter, in particular this tweet from AlecStapp:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">An odd choice to put a 240-year trend line on a chart when the underlying data looks like that... <a href="https://t.co/HaF4Pa5muK">pic.twitter.com/HaF4Pa5muK</a></p>&mdash; Alec Stapp (@AlecStapp) <a href="https://twitter.com/AlecStapp/status/1507542987563323393?ref_src=twsrc%5Etfw">March 26, 2022</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

...which got pretty widely taken up via retweets, quote tweets and 'likes' with a lot of scoffing about the 'chart crime', how Nate Silver should read his own books to learn about over-fitting, how he needs more education, etc. etc. 

Gosh I hate that aspect of Twitter - the way it leads otherwise decent (I presume) people to join a pile-on.

A lot of the criticisms were based on the supposition - wrong, I think - that the chart used a trendline from Excel or similar software that just fit a polynomial regression. This reminded a lot of people of the infamous use in 2020 by the Trump administration of a polynomial trendline to extrapolate Covid case numbers. *That* chart I was happy at the time to endorse a pile-on; using such a method for forecasting the future in that way is unforgiveable, particularly when much better models were available from all manner of experts. 

Some people seem to have learned the wrong lesson from that Trump cubic spline-to-forecast-covid episode.. The **wrong** lesson would be "never use the Excel add trendline function" or "never use a polynomial regression for any purpose whatsoever". The lesson should have been "Don't use a polynomial regression for forecasting stuff when there is a better model available."

But FiveThirtyEight weren't using their chart to make important forecasts. They were using it to make the point, highlighted in their title, that the confirmation process has gotten more contentious recently. Secondarily, it was highlighting historical dips and rises. For a purpose like this, any kind of empirical smoother - even a polynomial regression, if chosen judiciously - is perfectly adequate.

There was also some other even more incoherent criticisms:

- that you shouldn't put trend lines through historical data (what? how would we do economic history? or even just track unemployment?)
- that you shouldn't put trendlines through data that exhibits lots of points at the natural limit of a scale (ie 100%). This simply makes no sense, I don't even know how to engage with this... just to say that it makes perfect sense to take an average or similar summary of such data. We routinely for example talk about unemployment over time, which is just the average of the zeroes and ones at a point in time for people in the workforce who have jobs or not. Nothing wrong with that"average.
- that you shouldn't use trendlines with a low "R-squared" or explained variation from the regression implied by the trendline. This is the sort of thing that's been learnt by someone in a cookbook statistics course, it's just wrong. You don't need a high R-squared for a regression to be useful or indeed statistically significant. Often a noisy dataset with a subtle signal within it is still worth modelling, even when the model leaves a lot of unexplained noise.
- that it's not a real time series, presumably because the observations aren't equally spaced. This is another mistake, presumably from someone who has done some study with simple time series and never got to the more complex stuff when the observations aren't happening at equal intervals (it does get *much* more complicated, but irregularly spaced time series are still time series - opinion polls and their analysis being one example I have blogged about many times)
- that you should draw lines separately through the rejected, confirmed by vote, and confirmed by voice subsets of the data. Frankly, I don't know why you would do this; but basically it commits the statistical sin of breaking your data into subgroups based on the response variable you are trying to explain (vote), which is always going to get you one of no results, misleading results, or completely meaningless results.

None of these criticisms stand up. An annotation like this line is a really valuable addition to the chart. It helps show some strong features of the data that otherwise simply don't leap out to the viewer. Those are:

- a dip in the average vote for nominations around the 1850s - coinciding with the USA's descent in civil war and its most challenging time politically yet.
- a rise in the average vote around the 1940s and 1950s
- a decline in the last couple of decades.

These are real phenomena, and the line really makes them obvious to the viewer, and a good start for discussion about why. Frankly, I think this is a good chart. Not perfect, but a good, fit for purpose chart.

> Frankly, I think this is a good chart from FiveThirtyEight, and the trendline is a good feature that helps the viewer.

The criticism that I think *does* stand up is that they shouldn't have extrapolated the line to today or the near future. I agree, I think that's unhelpful; they could have just stopped the line with the final data point (Trump's last nomination) and still gotten these very useful substantive insights from it.

I guess I'd probably also prefer if it was a bit wider and shorter ie had a longer aspect ratio. But that's not very important.

So all the meanness on Twitter motivated me to come out of my self-imposed blog-free sabbatical and look at the actual data. The thing I was most curious about was whether in fact this curve *was* generated by a polynomial trendline a la Excel, or came from something else. I also wanted to confirm my intuition that, however the line was drawn, it was right to draw the above three observations from the data. That is, that the shape shown by the line is data-driven, not an artefact of the smoothing method chosen.

Spoiler - it might have been a polynomial line but probably wasn't; and it wouldn't matter anyway, because the shape of the line is genuinely data-driven.

## Importing code

First I needed to find the data. Thanks to Kostya Medvedovsky who [pointed me to the source](https://twitter.com/kmedved/status/1507596750865018882). It's a table in a web page so a bit of mucking about is needed. Some of the code below could be brittle if they make small changes in the web page, but it works ok today.

A couple of key data processing questions were what to do with the large number of nominations that were passed by "voice vote" (ie without going to a vote); and what to do with nominations that were withdrawn before getting to a vote. I think FiveThirtyEight turned the former into 100% votes, and I followed that. The latter they dropped from the data; I arranged my data so I could either do that, or turn them into 0% votes (which I think I prefer as a closer match to what would have actually happened if they had proceeded to put themselves to the vote - they wouldn't really have got 0% of course, but nor would they have got the average vote of other nominations which is what dropping them from the data altogether implies, at least in terms of producing trend lines).

*Post continues below R code*
{% highlight R lineanchors %}
library(tidyverse)
library(rvest)
library(lubridate)
library(scales)
library(glue)
library(patchwork)
library(kableExtra)
library(mgcv)

url <- "https://www.senate.gov/legislative/nominations/SupremeCourtNominations1789present.htm"

the_caption = glue("\n\n\nAnalysis by freerangestats.info copying analysis from fivethirtyeight; data from {url}")

d <- read_html(url)

d2 <- html_table(d)[[1]]

d3 <- d2[-(1:5), ] %>%
  filter(X2 == "" & X1 != "") %>%
  select(nominee = X1,
         to_replace = X3,
         nominated_date = X5,
         vote = X7,
         result = X9) %>%
  # add result labels:
  mutate(result = case_when(
    result == "C" ~ "Confirmed",
    result == "D" ~ "Declined",
    result == "N" ~ "No action",
    result == "P" ~ "Postponed",
    result == "R" ~ "Rejected",
    result == "W" ~ "Withdrawn",
    TRUE ~ "Not yet decided"
  )) %>%
  # remove footnotes:
  mutate(nominee = gsub("[0-9]", "", nominee)) %>%
  # fix dates
  mutate(nominated_date = mdy(nominated_date)) %>%
  # clean up vote count
  separate(vote, sep = "-", into = c("vote_for", "vote_against"), remove = FALSE, fill = "right") %>%
  mutate(vote_for = as.numeric(str_extract(vote_for, "[0-9]+")),
         vote_against = as.numeric(str_extract(vote_against, "[0-9]+"))) %>%
  # if the original vote was by voice, we call that 1-0
  mutate(vote_for = if_else(vote == "V", 1, vote_for),
         method = case_when(
           vote == "V" ~ "Voice", 
           result == "Withdrawn" ~ "Withdrawn",
           TRUE ~ "Vote"),
         result_lumped = if_else(result == "Confirmed", "Confirmed", "Other"),
         denominator = replace_na(vote_for, 0) + replace_na(vote_against, 0),
         prop_for = vote_for / denominator,
         prop_including_withdrawn = ifelse(is.na(prop_for) & result == "Withdrawn", 0, prop_for))

# Table of counts by result to check against the Senate page - all checks ok
count(d3, result) 
{% endhighlight %}

Here's that table of results, which matches the summary on the Senate's original webpage:

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> result </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Confirmed </td>
   <td style="text-align:right;"> 120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Declined </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No action </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not yet decided </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Postponed </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rejected </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Withdrawn </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
</tbody>
</table>

## Drawing different smoothing lines

Some of the discussion on Twitter had been about splines as part of a Generalized Additive Model or GAM, versus locally estimated scatterplot smoothing or LOESS, versus the infamous polynomial regression. So I tried all three of these methods. This got me the result below.

This might be a bit small for your screen, but you get the idea - all three methods come up with similar results.

<object type="image/svg+xml" data='/img/0230-as-original.svg' width='100%'><img src='/img/0230-as-original.png' width='100%'></object>

I tried the same thing with data that included the "withdrawn" nominees as zero percent votes. That got me these charts:

<object type="image/svg+xml" data='/img/0230-with-withdrawn.svg' width='100%'><img src='/img/0230-with-withdrawn.png' width='100%'></object>

Again, not much to choose from between the three different approaches.

Which is how I concluded:

- FiveThirtyEight probably didn't use a cubic regression, but this doesn't really matter.
- The key trends the line shows really are there in the data.

That's all folks. Here's the R code that drew the charts:

{% highlight R lineanchors %}
# Various variants of charts
p0 <- d3  %>%
  ggplot(aes(x = nominated_date, y = prop_for)) +
  geom_point(aes(colour = result_lumped, shape = method), size = 2) +
  labs(colour = "Result:",
       shape = "Method:",
       x = "Nomination date",
       y = "Proportion voting for confirmation",
       title = "Changing average vote for Supreme Court justice nominations") +
  scale_y_continuous(label = percent) 

p1 <- p0 +
  geom_smooth(method = "gam", se = FALSE, colour = "grey50", size = 2) +
  labs(subtitle = "Generalized additive model")
  
# Same chart but including the 'withdrawn' as zeroes
p1a <- p1 + aes(y = prop_including_withdrawn) + labs(y = "Proportion voting for confirmation (withdrawn = 0%)") 

p2 <- p0 +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3)", se = FALSE, colour = "grey50", size = 2) +
  labs(subtitle = "Cubic polynomial regression")

p2a <- p2 + aes(y = prop_including_withdrawn) + labs(y = "Proportion voting for confirmation (withdrawn = 0%)")

p3 <- p0 +
  geom_smooth(method = "loess", se = FALSE, colour = "grey50", size = 2) +
  labs(subtitle = "LOESS")

p3a <- p3 + aes(y = prop_including_withdrawn) + labs(y = "Proportion voting for confirmation (withdrawn = 0%)")

# Three charts:
  p1 + 
    p3 + labs(title = "") + theme(legend.position = "none")  + 
    p2 + labs(title = "", caption = the_caption) + theme(legend.position = "none")

# Three charts, including the 'withdrawn' as zeroes:
  p1a + 
    p3a + labs(title = "Things don't change much if we include 'withdrawn' as 0%") + theme(legend.position = "none")  + 
    p2a + labs(title = "", caption = the_caption) + theme(legend.position = "none")

{% endhighlight %}
