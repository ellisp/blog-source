---
layout: post
title: Minor updates for ggseas and Tcomp R packages
date: 2018-06-15
tag: 
   - Tools
   - Timeseries
   - Forecasting
   - R
description: Minor updates available on CRAN for the ggseas (seasonal adjustment on the fly) and Tcomp (tourism forecasting competition data) R packages
image: /img/0124-stat_seas.svg
socialimage: http://freerangestats.info/img/0124-stat_seas.png
category: R
---

## Updates

I've made small updates to two of my R packages on CRAN: `ggseas` (seasonal adjustment on the fly for `ggplot2` graphics) and `Tcomp` (tourism forecasting competition data).  Neither of the packages changes in a noticeable way for most users.

- The `ggseas` update is to get it ready for the [coming release of `ggplot2` 2.3.0](https://www.tidyverse.org/articles/2018/05/ggplot2-2-3-0/) scheduled for the end of June 2018.  It makes changes under the hood to the [mapping of variables to support "tidy evaluation"](https://community.rstudio.com/t/my-package-will-be-broken-on-cran-when-should-i-submit-the-fix/8708/7) but does not change anything for users.  It will still work with older versions of `ggplot2`.
- The `Tcomp` update fixes a [problem](https://github.com/ellisp/Tcomp-r-package/issues/12) where the reported length of time series was incorrect.

I've written more about [`ggseas`](/blog/2016/10/12/ggsdc-rents) and [`Tcomp`](/blog/2016/10/19/Tcomp) elsewhere.  Given they're both being updated at once, here's a brief demo of them in action together.

There are 1,311 series in the list `tourism` in `Tcomp`.  Each element of `tourism` contains some limited metadata about the series (for example its length, and how long the forecast is to be for), the original training data, and the "answer" in the form of the actual observations over the forecast period.  These objects are of class `Mdata`, introduced in Rob Hyndmans' `Mcomp` package, which comes with a convenient plotting method.


{% highlight R lineanchors %}
library(tidyverse)
library(scales)
library(ggseas)
library(Tcomp)

# default plot method for forecasting competition datasets of class Mdata
par(bty = "l", font.main = 1)
plot(tourism[["M4"]], main = "Series M4 from the tourism forecasting competition")
{% endhighlight %}

<img src='/img/0124-plot.svg' width='100%'>

`ggseas` makes it easy to take a seasonal time series object (ie an object of class `ts` with frequency > 1), convert it into a data frame, and produce exploratory graphics with it.  For example, here's code to take the training data from that same forecasting competition object and compare the original with a seasonally adjusted version (using X13-SEATS-ARIMA for the seasonal adjustment), and a 12 month rolling average:

{% highlight R lineanchors %}
# convert a time series to a data frame
the_data <- ggseas::tsdf(tourism[["M4"]]$x)

# draw a graphic
ggplot(the_data, aes(x = x, y = y, colour = 1)) +
  geom_line(aes(colour = "Original")) +
  stat_seas(aes(colour = "Seasonally adjusted")) +
  stat_rollapplyr(aes(colour = "12 month rolling average"), width = 12) +
  scale_colour_manual(values = c("Original" = "grey50", "Seasonally adjusted" = "red", 
                                 "12 month rolling average" = "blue")) +
  theme(legend.position = c(0.2, 0.8)) +
  scale_y_continuous("Unknown units\n", label = comma) +
  labs(x = "", colour = "") +
  ggtitle("Comparison of statistical summary methods in plotting a time series",
          "Monthly series 4 from the Tourism forecasting competition")
{% endhighlight %}


<img src='/img/0124-stat_seas.svg' width='100%'>


Then the `ggsdc` function makes it easy to look at a decomposition of a time series.  This really comes into its own when we want to look at two or more time series, mapped to colour (there's an example in the helpfile), but we can use it with a univariate time series too: 

{% highlight R lineanchors %}
ggsdc(the_data, aes(x = x, y = y), method = "seas") +
  geom_line() +
  ggtitle("Seasonal decomposition with `X13-SEATS-ARIMA`, `seasonal` and `ggseas`",
          "Monthly series 4 from the Tourism forecasting competition") +
  scale_y_continuous("Unknown units\n", label = comma) +
  labs(x = "")
{% endhighlight %}


<img src='/img/0124-ggsdc.svg' width='100%'>

I wrote the first version of the functions that later became `ggseas` in 2012 when working for New Zealand's Ministry of Business, Innovation and Employment.  We had new access to large amounts of monthly electronic transactions data and needed an efficient way to explore it on our way to developing what eventually became the [Monthly Regional Tourism Estimates](http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/monthly-regional-tourism-estimates).  


## Reflections on CRAN

This experience and recent twittering on Twitter have led me to reflect on the CRAN package update process.  Both these updates went through really smoothly; I'm always impressed at the professionalism of the volunteers behind CRAN.

I think CRAN is hugely important and an amazing asset for the R community.  It's really important that packages be published on CRAN.  I think the mapping of dependencies, and the process for checking that they all keep working together, is the key aspect here.  

`ggplot2` apparently has more than 2,000 reverse dependencies (ie packages that import some functionality from `ggplot2`), all of them maintained more or less voluntarily.  When major changes are made I can't think of any way other than CRAN (or something like it) for helping the system react and keep everything working.  For instance, I found out that the new `ggplot2` v2.3.0 would break `ggseas` when I got an automated email advising me of this, from the tests Hadley Wickham and others working on `ggplot2` ran to identify problems for their reverse dependencies.  The RStudio community site was useful for pointing me (and others with similar issues) in the direction of a fix, and of course one uses GitHub or similar to manage issues and code version control, but in the end we need the centralised discipline of something like CRAN as the publication point and the definitive map of the dependencies.  So, a big shout out to the volunteers who maintain CRAN and do an awesome job.

## Reflections on testing

When a user pointed out a bug in the `Tcomp` package by raising an issue on GitHub, I was pleased with myself for using genuine test-driven development.  That is, I first wrote a test that failed because of the bug:

{% highlight R lineanchors %}
test_that("series lengths are correct", {
  expect_equal(sum(sapply(tourism, function(s){length(s$x) - s$n})), 0)
  
})
{% endhighlight %}

...and *then* worked on finding the problem and fixing it upstream.  It's such an effective way to work, and it means that you have a growing body of tests.  Packages like `Tcomp` and `ggseas` go for years without me doing anything to them, so when I do have to go back and make a change it's very reassuring to have a bunch of tests (plus running all the examples, and performing the checks for CRAN) to be sure that everything still works the way it's meant to.  

There's not enough tests in the build process of those packages at the moment; the more experienced I get, the more I think "build more tests, and build them earlier" is one of the keys to success.  This is just in code-driven projects either; I think there's broad applicability in any professional situation, although the development and automation of tests is harder when you're not just dealing with code.

