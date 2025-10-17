---
layout: post
title: Tripping over group level effects
date: 2025-11-11
tag: 
   - ModellingStrategy
   - Inequality
   - Crime
   - Transformations
   - DataFromTheWeb
description: Countries with higher income or consumption inequality tend to have more homicides per population. A multilevel model with a random country effect shows a much weaker relationship than a simple model that treats each repeated country observation as equally valuable, or a multilevel model that includes an average country-level inequality fixed effect.
image: /img/0303-scatter.png
socialimage: https:/freerangestats.info/img/0303-scatter.png
category: R
---

So this is a blog post about making and finding an error in modelling strategy, in an apparently simple situation. 

I saw [this post (toot?) on Mastodon](https://mastodon.sdf.org/@dlakelan/115055789165555934) from Daniel Lakens mentioning in passing that "On the other hand, the homicide rate across the world is well modeled as exp(k * income_gini_coefficient)." This struck me as worth checking; So I did! It turns out it is more or less correct.

Here is a chart I drew of homicides per 100,000 population on the vertical axis versus income or consumption inequality on the horizontal. 

<object type="image/svg+xml" data='/img/0303-scatter.svg' width='100%'><img src='/img/0303-scatter.png' width='100%'></object>

It's not a big point, but the model implied by Professor Lakens would be a straight diagonal line, what you'd get if you just used `geom_smooth(method = lm, formula = y ~ x - 1)`. I haven't shown his, but two slightly more complex models. Here's the code that downloads the data, fits those models and draws the chart. A few things to note:

* There's a bit of fancy footwork to define which countries I label on the chart, and in the end also a few I chose manually and ad hoc because I was interested in them.
* While the y axis in this plot uses a logarithm transform, for the model itself I transformed the response variable not with a logarithm but with a BoxCox transformation that still works when countries have zero homicides in a particular year. I toyed with alternatives like using a generalized linear model with a log link (which still works when data is observed to be zero, because it will be predicted/expected to be slightly more) but decided to avoid this so I could use `lme` which lets me specify autocorrelated error terms
* Because the model and plot are both a bit complex I set out my own prediction grid and calculate expected values and crude (ie assuming asymptotic normality) confidence intervals rather than using `marginaleffects` package to get me something out of the box
* The ribbons drawn are showing the expected homicide rate for the country with random country effect closest to zero, which happens to be Czechia with the data at the time of writing (17 October 2025)
* The ribbons are also based on the assumption that the hypothetical country they refer to has an average inequality (over all years for which there are observations) as shown on the horizontal axis. This point is pretty crucial.

{% highlight R lineanchors %}
library(tidyverse)
library(WDI)
library(nlme)
library(forecast) # for chosing box cox parameters
library(AICcmodavg) # for predictSE with lmer
library(ggrepel)
library(GGally)

# Analysis to follow up this remark:
# https://mastodon.sdf.org/@dlakelan/115055789165555934
# "On the other hand, the homicide rate across the world 
#  is well modeled as exp(k * income_gini_coefficient)."

#--------Download World Bank data and prep it------------
# These searches were used to find series with a lot of data for country-year combinations:
# WDIsearch("homicide")
# WDIsearch("Gini")

# Metadata for SI.POV.GINI at https://data360files.worldbank.org/data360-data/metadata/WB_WDI/WB_WDI_SI_POV_GINI.pdf
# Key points include:
# * individuals (not household)
# * adjusted for household size
# * can be income or consumption depending on what was available

d <- WDI(indicator = c(homicide = "VC.IHR.PSRC.P5", 
                              gini = "SI.POV.GINI")) |> 
  as_tibble()

# Countries we are going to want to highlight in our chart
highlights <- c("United States", "Russian Federation", 
                  "Samoa", "Australia", "United Kingdom",
                  "Fiji", "Mexico", "Papua New Guinea")

# There are a few country-years with zero homicides so can't use a simple
# logarithm transformation, but can have a Box-Cox that has a similar result
# Choose a lambda that gives a distribution after the transformation where
# changes are relatively stable in absolute terms:
lambda <- forecast::BoxCox.lambda(d$homicide)

# version of the data we will use for plotting and modelling:
d2 <- d |> 
  drop_na() |> 
  group_by(country) |> 
  mutate(type = ifelse(year == max(year), "Latest", "Earlier")) |> 
  mutate(ctry_avg_gini = mean(gini)) |> 
  ungroup() |> 
  mutate(label = ifelse(type == "Latest" & (gini > 53 | homicide > 35 | 
                                 gini < 26 | homicide < 0.26 | country %in% highlights),
                        country, ""),
         hom_tran = BoxCox(homicide, lambda = lambda),
         country = as.factor(country))  |> 
  arrange(year)

#------------Modelling and predictions (for drawing 95% confidence intervals on charts)----
# you could have random slopes too but a fair number of countries have only one
# or two observations
# see https://www.frontiersin.org/journals/education/articles/10.3389/feduc.2017.00058/full
# for example which says you want at least 6 observations per group to have
# a random slope. Not sure how many you need, but 1 isn't enough :)

# Barchart of number of observations per country, used later in the blog post
p1 <- d2 |> 
  count(country, name = "n_obs") |> 
  count(n_obs, name = "n_countries") |> 
  ggplot(aes(x = n_obs, y = n_countries)) +
  geom_col(fill = "steelblue") +
  labs(x = "Number of observations (ie years)",
       y = "Number of countries",
      title = "Complete data for Gini coefficient and homicide rates")

print(p1)

# super simple model, not mentioned in the actual blog post
model1 <- lm(hom_tran ~ gini, data = filter(d2, type == "Latest"))

# multilevel model with a country random intercept, and inequality only at the lowest level
model2 <- lme(hom_tran ~ gini, random = ~1  | country, 
                    data = d2, correlation = corAR1())


# multilevel model with country random intercept and countries' average inequality, as well
# as the lowest level (country-year) granularity inequality:
model3 <- lme(hom_tran ~ gini + ctry_avg_gini, random = ~1  | country, 
                    data = d2, correlation = corAR1())

# Fairly high (and similar) coefficients, about 0.11, but for differ
summary(model1) # 0.12 for gini
summary(model3) # 0.11 for ctry_avg_gini; gini not significant

# Relatively low coefficients - the country randomness 
# soaks up a lot of the randomness:
summary(model2) # gini not significant

# Is the average random country effect basically zero? - check:
stopifnot(round(mean(ranef(model2)[[1]]), 10) == 0)
stopifnot(round(mean(ranef(model3)[[1]]), 10) == 0)

# Find the country with random effect closest to zero. Needed for predictions
# to draw an average country ribbon on the chart
avg_country <- ranef(model3) |> 
  arrange(abs(`(Intercept)`)) |> 
  slice(1) |> 
  row.names()

pred_grid <- tibble(gini = 20:65, 
                    ctry_avg_gini = 20:65, 
                    country = avg_country)

pse1 <- predict(model1, newdata = pred_grid, se = TRUE)
pse2 <- predictSE(model2, newdata = pred_grid, se = TRUE)
pse3 <- predictSE(model3, newdata = pred_grid, se = TRUE)


pred_grid <- pred_grid |> 
  mutate(predicted1 = pse1$fit,
         lower1 = predicted1 - 1.96 * pse1$se.fit,
        upper1 = predicted1 + 1.96 * pse1$se.fit) |> 
  mutate(predicted2 = pse2$fit,
         lower2 = predicted2 - 1.96 * pse2$se.fit,
        upper2 = predicted2 + 1.96 * pse2$se.fit) |> 
  mutate(predicted3 = pse3$fit,
         lower3 = predicted3 - 1.96 * pse3$se.fit,
        upper3 = predicted3 + 1.96 * pse3$se.fit) |> 
  mutate(across(predicted1:upper3, function(x){InvBoxCox(x, lambda = lambda)}))

#------------------Draw chart--------------------

mod_cols <- c("purple", "darkgreen", "brown", "pink")

p2 <- d2 |> 
  ggplot(aes(x = gini, y = homicide)) +
  scale_y_log10() +
  xlim(18, 70) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_manual(values = c("steelblue", "black")) +
  labs(x = "Inequality (Gini coefficient), based on individual income or in some cases, consumption. Higher means more inequality.",
       y = "Homicide rate (per 100,000)",
      colour = "Observation year:",
      shape = "Observation year:",
      title = "Higher inequality countries have more homicides.",
      caption = "Source: World Bank, World Development Indicators, series VC.IHR.PSRC.P5 and SI.POV.GINI. Analysis by freerangestats.info.") 
  
  
p2a <- p2 +
  # model2 - just country-year level data, country random effect
  geom_ribbon(data = pred_grid, aes(ymin = lower2, ymax = upper2, y = NA), 
                fill = mod_cols[2], alpha = 0.2) +
  
  #model3 - country-year but also country average data, country random effect
  geom_ribbon(data = pred_grid, aes(ymin = lower3, ymax = upper3, y = NA), 
                fill = mod_cols[3], alpha = 0.2) +
  
  geom_point(aes(shape = type, colour = type)) +
  geom_label_repel(aes(label = label), max.overlaps = Inf, colour = "grey10", size = 2.8,
                   seed = 123, label.size = unit(0, "mm"), fill = rgb(0,0,0, 0.04))  +
  # annotations - text and arrows - for models 2 and 3
  annotate("text", x = 61.5, y = 0.9, colour = mod_cols[2], hjust = 0, vjust = 1,
               label = str_wrap("Model with no such average country inequality effect.", 24)) +
  annotate("text", x = 61.5, y = 200, colour = mod_cols[3], hjust = 0, vjust = 1,
               label = str_wrap("Model including an effect for average inequality in each 
                                country over time.", 26)) +
  
  annotate("segment", x = 64, xend = 64, y = 1, yend = 2, colour = mod_cols[2], 
            arrow = arrow(length = unit(2, "mm"))) +
  annotate("segment", x = 64, xend = 64, y = 75, yend = 45, colour = mod_cols[3], 
            arrow = arrow(length = unit(2, "mm"))) +
  
  labs(subtitle = "Selected countries highlighted. 
Shaded ribbons show 95% confidence interval of mixed effects models with random country effect and autocorrelated error terms.",
)

print(p2a)
{% endhighlight %}



In fact at first I just had that green line&mdash;`model2` in the code&mdash;which is pretty flat. I was naturally struck at how it doesn't seem to go through the data points. At first I thought this was because I was so clever, and had incorporated a country level random effect, and allowed autocorrelation over time for the multiple observations of each country.

To extract that model&mdash;the one with the green line that's conspicuously under the data&mdash;from the mess of code above, it's `model2` and it looks like this:

{% highlight R lineanchors %}
model2 <- lme(hom_tran ~ gini, random = ~1  | country, 
                    data = d2, correlation = corAR1())
{% endhighlight %}

This seemed like the really obvious thing to fit for me. Why not just some ordinary least squares thing? Because many of the countries have multiple observations, as we see in this plot (which was created in the code chunk we've already seen above):

<object type="image/svg+xml" data='/img/0303-number-obs.svg' width='100%'><img src='/img/0303-number-obs.png' width='100%'></object>

I would say that the thing that is most often done in this situation is to filter the data down to just one observation per country&mdash;perhaps by taking the latest observation for each country, or the one that is closest to some year that is chosen precisely because most countries have an observation in that year or close to it. 

That seemed like a sad waste of data to me (although in my code above you can see that I did fit such a model, `model1`, and if you want to look at it, it avoids the problem I'm about to talk about!). So my thinking was to use all the data in a mixed effects model, with a random intercept at the country level to allow for the fact that each new observation on a country, while definitely worth *something*, isn't worth as much as an independent observation because it's going to be similar to the other observations for that country. To do this even better, as well as the country level random intercept I throw in an autocorrelation of the residuals, showing that the values in one year are expected to be correlated with those in the previous year (as indeed they turn out to be)

That's all well and good but in this case it backfired on me. When I first drew this plot, with only the green line of `model2` showing, I thought "Oh that's cool, when we correct for the fact that many of these observations are low-value repeats of the same country's previous observations, it turns out there's very little relationship between inequality and homicide after all". But more thinking, and looking at some residuals, and comparing it to the more positive results from the much simpler `model1`, quickly made me realise I was barking badly up the wrong tree.

The problem is best illustrated by this chart that I drew at some point along the way. I realised that as I have a random intercept that is different for each country, the expected value for that country from the model will vary very much by country. But then the modelling of homicide on inequality will start from that point&mdash;the model will be seeking to explain variance in homicide rates *in this particular* country based on its observations. 

<object type="image/svg+xml" data='/img/0303-faceted.svg' width='100%'><img src='/img/0303-faceted.png' width='100%'></object>

If we had a random slope as well, the result would look like the dark grey lines in these plots&mdash;a different relationship for each country. But I hadn't done this, because I judged there just weren't enough observations per country for a random slope (many countries in fact have a single observation&mdash;economic inequality is difficult and costly to measure, needing a special household consumption survey if possible). So my models look more like the pale red ribbon shown here:

What's happening is that we have two things going on:

* inequality *between* countries does quite a good job of explaining differences in homicide rates
* *within* each country that has observations over multiple years, there is much less relationship between the two; and where there is a relationship, it differs country by country. For example, judging by the the black dot representing the most recent point, Australia and the United States look to have gotten more unequal over time but less homicides; Mexico has gotten less unequal and more homicides; whereas Russia, Ukraine and the UK have had decreases in both inequality and homicides. In other words, for Russia, Ukraine and the UK, the within-country data matches the between-country positive rleationship between inequality and homicide, but Australia, USA and Mexico have a negative relationship between the two variables.

Another way of looking at this is to say that the random country intercept absorbs the variance in homicide that could be instead explained by that country's enduring average inequality. But we don't have "enduring, average inequality" as a variable in model2. If we calculate a proxy for that the obvious way (average of the inequality values we have for that country, disregarding the fact that they come from different years for each country), we can look at the relationship between this "enduring inequality" and the country intercepts in `model2`, and we see a strong positive relationship. That's the middle left panel (a scatter plot) in the chart below, as well as the middle top correlation (0.608, well different from zero).

<object type="image/svg+xml" data='/img/0303-country-level-ranef.svg' width='100%'><img src='/img/0303-country-level-ranef.png' width='100%'></object>

The other variable in that scatter plot matrix is the country effects from `model3`, which has been specified like this:

{% highlight R lineanchors %}
model3 <- lme(hom_tran ~ gini + ctry_avg_gini, random = ~1  | country, 
                    data = d2, correlation = corAR1())
{% endhighlight %}

The difference from `model2` is that now we have `ctry_avg_gini` there as our estimate of enduring average country inequality. We still have `gini`, which is the value of inequality that varies for this country year by year. That will pick up the average within-country effect.

In the scatterplot matrix above we can see that once we include each country's average inequality in the model, there is no relationship between that variable and the country level random effects. This is to be expected, precisely because the model fitting process is designed to achieve this.

Here's the summary results for `model3`:

```



```

We see the strong effect of average country-level inequality effect, and no significant evidence of an effect of inequality within countries. Interesting! But beyond my scope today to go into that.

Finally I want to finish off with a clean plot of my best model and the phenomenon under question&mdash;the strong empirical relationship between economic inequality in a country and the homicide rate.

<object type="image/svg+xml" data='/img/0303-scatter-final.svg' width='100%'><img src='/img/0303-scatter-final.png' width='100%'></object>

Here's the code for that final clean plot:
{% highlight R lineanchors %}
p2b <- p2 +
  
  #model3 - country-year but also country average data, country random effect
  geom_ribbon(data = pred_grid, aes(ymin = lower3, ymax = upper3, y = NA), 
                fill = mod_cols[3], alpha = 0.2) +
  
  geom_point(aes(shape = type, colour = type)) +
  geom_label_repel(aes(label = label), max.overlaps = Inf, colour = "grey10", size = 2.8,
                   seed = 123, label.size = unit(0, "mm"), fill = rgb(0,0,0, 0.04)) 

print(p2b)
{% endhighlight %}
Note that this builds on the object `p2` that was the basis of the first plot, the one showing both `model2` and `model3`, to avoid repetition.

And here's the code for the playing around with residuals at different levels and drawing that facet plot version with 12 selected countries:
{% highlight R lineanchors %}
#---------------residuals from model2 and model3---------------

# Individual level residuals - plot not used in blog
p3 <- d2 |> 
  mutate(`Residuals from Model 2` = residuals(model2),
          `Residuals from Model 3` = residuals(model3)) |> 
  select(gini, `Residuals from Model 2`, `Residuals from Model 3`) |> 
  gather(variable, value, -gini) |> 
  ggplot(aes(x = gini, y = value)) +
  facet_wrap(~variable) +
  geom_point() +
  labs(x = "Inequality",
       y = "Residuals (on Box-Cox transformed scale)",
       title = "Residuals from two mixed-effects models of homicide",
       subtitle = "Residuals at the most granular level ie year-country are uncorrelated with inequality")

svg_png(p3, "../img/0303-residuals", w = 8, h = 4)

# out of curiousity what are those low outlier residuals? - Iceland and Malta
round(sort(residuals(model2)),2)[1:5]
round(sort(residuals(model3)),2)[1:5]

# Country level effects plot - pairs plot used in blog
p4 <- function(){
  p <- d2 |> 
  distinct(country, ctry_avg_gini) |> 
  arrange(country) |> 
  mutate(`Country effects in Model 2` = ranef(model2)[[1]],
         `Country effects in Model 3` = ranef(model3)[[1]]) |> 
  select(-country) |> 
  rename(`Countries' average inequality` = ctry_avg_gini) |> 
  ggpairs() +
  labs(title = "Comparison of country-level random effects in two models",
      subtitle = "Model 3 has a fixed effect for countries' average inequality; Model 2 doesn't.
The country-level residuals are correlated with this variable in Model 2, but not Model 3.")
 
    print(p)
}

p4()


#---------------further illustrations - facets----------------------
d3 <- d2 |> 
  filter(country %in% c(highlights, "South Africa", "France", "Japan", "Ukraine")) |> 
  group_by(country) |> 
  summarise(r = coef(lm(log10(homicide) ~ gini))[2]) |> 
  mutate(r = replace_na(r, 0)) |> 
  mutate(country = fct_reorder(country, r) |>  fct_drop()) |> 
  arrange(country)


pred_grid2 <- d2 |> 
  filter(country %in% d3$country) |> 
  distinct(country, ctry_avg_gini) |> 
  expand_grid(gini = 20:70)

more_preds <- predictSE(model3, newdata = pred_grid2, se = TRUE)


pred_grid2 <- pred_grid2 |> 
  mutate(predicted3 = more_preds$fit,
         lower3 = predicted3 - 1.96 * more_preds$se.fit,
        upper3 = predicted3 + 1.96 * more_preds$se.fit) |> 
  mutate(across(predicted3:upper3, function(x){InvBoxCox(x, lambda = lambda)}))



p5 <- d2 |> 
  mutate(country = factor(country, levels = levels(d3$country))) |> 
  drop_na() |> 
  ggplot(aes(x = gini, y = homicide)) +
  scale_y_log10(label = comma, limits = c(0.1, 100)) +
  xlim(18, 70) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_manual(values = c("steelblue", "black")) +
  geom_ribbon(data = pred_grid2, aes(ymin = lower3, ymax = upper3, y = NA), 
                fill = mod_cols[3], alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE, colour = "grey50", fullrange = TRUE, linewidth = 0.5) +
  geom_point(aes(shape = type, colour = type)) +
  facet_wrap(~country, ncol = 3)  +
  labs(x = "Inequality (Gini coefficient), based on individual income or in some cases, consumption. Higher means more inequality.",
       y = "Homicide rate (per 100,000)",
      colour = "Observation year:",
      shape = "Observation year:",
      title = "Higher inequality countries have more homicides.",
      subtitle = "But for any given country, the relationship over time may well be the reverse.
Pale ribbon shows overall model's confidence interval for homicide for this country, at its average over time level of inequality.
Dark line shows simple model fit to just this country's data.",
      caption = "Source: World Bank, World Development Indicators, series VC.IHR.PSRC.P5 and SI.POV.GINI. Analysis by freerangestats.info.") 
  

print(p5)
{% endhighlight %}

The full set of code, in sequence, can be [found on GitHub]() if the above presentaiton is confusing.
