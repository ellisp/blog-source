---
layout: post
title: Men's domestic chores and fertility rates - Part II, technical notes
date: 2025-08-23
tag: 
   - ModellingStrategy
   - DataFromTheWeb
description: A few specific notes on technical issues relating to a previous post. On drawing network graphs with different coloured edges; modelling strategy; different specifications of models; and accessing UN SDG and gender inequality data.
image: /img/0290-compare-ti-and-s.png
socialimage: https:/freerangestats.info/img/0290-compare-ti-and-s.png
category: R
---

This post is a collection of more technical notes forming a companion piece to [the previous post on men's time spent on domestic chores and total fertility rates at the country level](/blog/2025/08/15/timeuse-summary). 

The target audience for today's post is future-me, and anyone else similarly interested in the quite specific issues it is jotting down notes for. Those issues include:

* Drawing directed graphs with `ggdag` that have different coloured edges
* Accessing the UN Sustainable Development Goals (SDGs) database API
* Data options on gender inequality
* Fitting the same mixed effects model with `lme4::lmer`, `mgcv::gamm` and `mgcv::gam`, comparing results and extracting group-level residuals
* Hand-drawing prediction plots to show the results of models with interactions and comparing that to the `marginaleffects` package
* Diagnostics of a simplified version of the model
* Modelling strategy questions relating to researcher degrees of freedom, use of splines and interaction effects

My approach to presenting this (both today, and in Part I) is different to my usual one, where I try to make the post itself a stand-alone, reproducible artefact with all the code necessary to produce the results in the post. That approach just didn't work out in this case; the code was too long and boring to include in the main blog, there are too many different audiences to write for, and I went down too many dead ends in exploring some of the modelling options. Even in this technical companion piece, this post isn't going to be fully reproducible, but just have the snippets of code that best illustrate the point.


To make up for this, as ever, the full code to produce that blog post is on [GitHub as part of my overall blog-source repository](https://github.com/ellisp/blog-source/blob/master/_working/0290-house-work.R).

### Directed graphs with different coloured edges

For the main post, I had to draw a couple of directed graphs with different coloured edges connecting the nodes. Specifically, I wanted to use a red line to show a negative direction effect, and blue to show a positive, like this:

<object type="image/svg+xml" data='/img/0290-dg-simplified.svg' width='100%'><img src='/img/0290-dg-simplified.png' width='100%'></object>

This was surprisingly fiddly to do and required a bit of a hack. Specifically, you have to explicitly call the `tidy_dagitty` function, which turns a `ggdag` graph object of class `dagitty` into a data frame of class `tidy_dagitty`; then you add a column to that data frame which has the *actual colours* as its values, conditional on whatever algorithm you need to determine those colours. In this example, I want it to be red when the line segment is connect "opp" (Opportunities for women and girls) to "tfr" (Total fertility rate), and blue otherwise. 

As far as I could tell, you can't just map a column of character or factor values to colour and let the colour scale match it, which would be the approach more consistent with the `ggplot2` philosophy. Instead, you only have the choice of an identity scale, which is why that column `edge_type` I  add has to have the values "darkred" and "steelblue". That's the main trick for doing this.

{% highlight R lineanchors %}
dg2 <- dagify(tfr ~ opp + hw ,
             hw ~ opp,

             labels = c(
               "tfr" = "Total fertility rate",
               "hw" = "Men doing housework",
               "opp" = "Opportunities for\nwomen and girls"
             ),
             outcome = "tfr",
             exposure = "hw"
)  |> 
  # explicitly call this usually hidden function so we can colour the edges:
  ggdag::tidy_dagitty(seed = 124) |> 
  # colour the edges. Need to specify identity of colour here, not use scale_
  mutate(edge_type = ifelse(to == "tfr" & name == "opp", "darkred", "steelblue"))


# Draw the simplified causal graph
set.seed(124)
dg2 |> 
  ggplot(aes(x = x, y = y, xend = xend, yend =yend)) +
  geom_dag_node(colour = "grey") +
  geom_dag_edges(aes(edge_colour = edge_type), 
                 arrow_directed = grid::arrow(length = unit(12, "pt"), type = "closed")) +
  geom_dag_text_repel(aes(label = label), col = lab_col) +
  theme_dag(base_family = "Roboto")
{% endhighlight %}



### Accessing the UN SDGs database

I couldn't find a simple way of accessing the United Nations Statistics Division's invaluable definitive database of the SDG indicators for all countries of the world. By which I mean, it has an API, but I didn't see anyone who'd written a nice R package to conveniently interact with it. If anyone knows of someone who's done this, or wants to do it themselves, please let me know.

So I had to write my own API request by myself, like an animal. I did this in what I am sure is a suboptimal way, but it works. From playing around with [the UN's API](https://unstats.un.org/sdgs/UNSDGAPIV5/swagger/index.html) I found the `curl` command I wanted to download the data:

```
curl -X POST --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/octet-stream' -d 'seriesCodes=SL_DOM_TSPD' 'https://unstats.un.org/sdgapi/v1/sdg/Series/DataCSV'
```

Then I used functions from Bob Rudis' `curlconverter` R package to convert this to a request for the old-fashioned `httr` package to use. As the comments in this code say, I know all this is outmoded; but it works for now.

{% highlight R lineanchors %}
#-----------downloading some SDG time use data from the UN database-------------
# Note sure this is the best way to do this, it was clunky to work out,
# but it works. Someone should (or have they already?) build an R package.
#
# this is all httr, I understand httr2 is the  current thing now, but this still works 
library(curlconverter)
library(httr)
request <- "curl -X POST --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/octet-stream' -d 'seriesCodes=SL_DOM_TSPD' 'https://unstats.un.org/sdgapi/v1/sdg/Series/DataCSV'" |> 
  straighten() |> 
  make_req()

gender_txt <- content(request[[1]](), as = "text")

gender <- read_csv(gender_txt) |> 
  clean_names()
{% endhighlight %}

The end results is I want a variable, from that `SL_DOM_TPSD` indicator (time spent on domestic chores and care work by sex and urban/rural location) that can be represented like this:

<object type="image/svg+xml" data='/img/0290-time-share-bar.svg' width='100%'><img src='/img/0290-time-share-bar.png' width='100%'></object>

There are significant data wrangling challenges, though, in particular the different age categories used in each country, the different years that surveys were conducted, and the presence of multiple observations for some but not all countries.

The main reason for including this next snippet is to remind myself of what was needed to do to fiddle with those age categories. For example, note that some countries have values for multiple open ended categories like 3+ and 15+; we need a rule for deciding which of these is best for our desired constructed variable of men's share of adult domestic domestic and care work (in this case, 15+ is better than 3+, when both are available for a country):

{% highlight R lineanchors %}
count(gender, sex)      # two categories, FEMALE and MALE - no TOTAL
count(gender, age)      # many different ages used for different countries
count(gender, location) # there's ALLAREA, RURAL and URBAN

# should be only one indicator:
stopifnot(length(unique(gender$series_description)) == 1)
# which is 
# Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%) 

time_chores <- gender |> 
  # we don't want rural and urban, just country total:
  filter(location == "ALLAREA") |> 
  # we want the ages like 15+, 12+ etc, not those like 15-59 with an upper bound
  filter(grepl("^[0-9]*\\+$", age)) |> 
  # but not the retirees, which some countries include. We want the 15+, not 15+
  # and 65+ separately:
  filter(!age %in% c("65+", "85+", "60+")) |> 
  # calculate the male time spent as a proportion of total (male and female) time spent
  group_by(geo_area_name, time_period, age) |> 
  summarise(prop_male = value[sex == 'MALE'] / sum(value[sex == 'MALE'] + value[sex == 'FEMALE'])) |> 
  group_by(geo_area_name) |> 
  # Label the latest survey per country. Note that any modelling needs to
  # include a country random effect for the multiple observations per country:
  mutate(is_latest = ifelse(time_period == max(time_period), "Most recent", "Earlier")) |> 
  # limit to just the best age group, closest to adults, for each country/time:
  group_by(geo_area_name, time_period) |> 
  mutate(age = factor(age, levels = c("15+", "16+", "18+", "12+", "10+", "6+", "5+", "3+"))) |> 
  arrange(geo_area_name, time_period, age) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(iso3_code = countrycode(geo_area_name, origin = "country.name.en", destination = "iso3c"))
{% endhighlight %}


### Data on gender inequality

I spent quite a bit of time looking for data on gender inequality independent of the housework question. I wanted something that resembled women's and girls' opportunities in education and the economy more broadly. I had various deadends in pursuing this. My first idea was some kind of literacy measure&mdash;female literacy at some given age, or for adults overall, as a ratio for equivalent male literacy. But the various sources for this just didn't have enough observations. 

The main sources for literacy would be self-report in a census or possibly a social survey; or a standardised test at a given year of schooling. After some fruitless time with SDGs, the World Bank's World Development Indicators, and various other sources, I concluded that neither of these seem to be readily available in a comparable basis for enough years that matched with the year-country combinations that I had time-use data for.

I ended up using the [Gender Inequality Index (GII) from the UNDP](https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII) instead. Now, this index is complex and relies on a bunch of indicators that are obviously going to be at least as hard to measure as literacy&mdash;like level of secondary education (needs admin data or survey or census) and maternal mortality ratio (needs good civil registry, or survey data as a less satisfactory alternative). Here's how the GII is constructed:

<img src="/img/0290-gii-diagram.png" width = "100%">

But the GII is available for all country-year combinations, which simply can't be based on direct observations of these variables. Obviously the UNDP do a bunch of modelling to interpolate all the missing values. I didn't look into this but just trusted the UNDP to have done the best job possible. It's certainly very convenient to get this measure of gender inequality for so many countries (206 'countries', but this includes some regional groupings), and for so many years.

<object type="image/svg+xml" data='/img/0290-gii-lollipop.svg' width='100%'><img src='/img/0290-gii-lollipop.png' width='100%'></object>

There were multiple ways to download this GII data from the Human Development Reports website, but it turns out the best is to download all the Human Development Report data for the latest year in a single, big CSV:

{% highlight R lineanchors %}
# You can download all the HDR components (including GII):
df <- "hdr25.csv"

if(!file.exists(df)){
  download.file("https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv",
                destfile = df)
}
hdr <- read_csv(df)
{% endhighlight %}

From there it's straightforward data wrangling to extract just the GII data and combine with my other datasets using year and the ISO three character codes for countries to join by.

### Fitting the same mixed effects model with `lmer`, `gamm` and `gam`

#### Specifying models

One of the things I wanted to sort in this post was the near equivalence of some of the many different ways of specifying and fitting a mixed effects model in R. There's a good post from Gavin Simpson on ['Using random effects in GAMs with mgcv'](https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/) that I referred to repeatedly in preparing this.

Specifically, I wanted to check that these four models, set out below, are all very similar. By which I actually mean the last three are *identical* statistically, but have different ways of being estimated and/or the formula written down; and the first is a statistically different model in terms of probability distributions and link functions, but effectively very very similar indeed to the other three:

{% highlight R lineanchors %}
# note response variable is ltfr, defined earlier as log(tfr):
model2 <- lmer(ltfr ~ gii + log(gdprppppc) * prop_male + (1 | country_fac), 
               data = model_ready)

model7a <- gamm(tfr ~ gii + log(gdprppppc) * prop_male +  s(country_fac, bs = 're'), 
               data = model_ready, family = quasipoisson)


model7b <- gamm(tfr ~ gii + log(gdprppppc) * prop_male,
               random = list(country_fac = ~ 1),
               data = model_ready, family = quasipoisson)

model7c <- gam(tfr ~ gii + log(gdprppppc) * prop_male +  s(country_fac, bs = 're'), 
                data = model_ready, family = quasipoisson, method = "REML")
{% endhighlight %}

These are:

* `model2` - fit with `lme4::lmer`, response variable is log-transformed first and then response is Gaussian, country level random effect is specified with `1 | country_fac` formula notation. Note that I *can't* use `glmer` becuase it doesn't allow family = quasipoisson.
* `model7a` - fit with `mgcv::gamm` which is an iterative process where the mixed effects are with with `lmer` and smoothing splines are done with `gam`, iterate till converges. The end result contains the final version of both the `lme` model and the `gam` model. There's no pre-transformation done of the response variable because we are using a generalized linear model with quasipoisson family - that is, variance is proportional to the mean, but not forced to be equal to it. The random country level effect is specified with `s(country_fac, bs = 're')` (`re` stands for random effects), which is passed on to `lme` that treats it as `Formula: ~1 | country_fac`.
* `model7b` - identical to 7a except the random effects are specified by `random = list(country_fac = ~ 1)`
* `model7c` - fit with `mgcv::gam`, using the same model specification as `model7b`. Unlike `gamm`, this does all the work within `gam` itself, there's no iterating to the functions of the `lme4` package. There are limitations imposed as a result - the random effects can't be correlated with eachother, and you can't specify complex error structures (autocorrelation etc) like you could with `gamm` or `lmer`. But I don't have a need for either of these things. **Importantly** `model7c` has to use restricted maximum likelihood as its estimation method if we want it to get equivalent results to the `lmer`-based methods.

In the end, none of these models were referred to in my main post because I went for an approach based purely on the use of `gam()` and with various non-linear effects even in the base, null model. But it was a very useful learning experience for me to work out exactly what is and isn't different in a bunch of similar model specifications.

#### Getting the same fixed coefficients

Here is code to extract the coefficients for the fixed effects from these four essentially identical models:

{% highlight R lineanchors %}
# Fixed coefficients of the four similar linear models:
summary(model7a$lme)$tTable |> 
  as.data.frame() |> 
  select(mod7a = Value) |> 
  mutate(mod7b = pull(as.data.frame(summary(model7b$lme)$tTable), Value)) |> 
  mutate(mod7c = pull(as.data.frame(summary(model7c)$p.table), Estimate)) |> 
  mutate(mod2 = fixef(model2)) |> 
  mutate(across(where(is.numeric), round, digits = 2))
{% endhighlight %}

which gives

```
                          mod7a mod7b mod7c  mod2
X(Intercept)               3.55  3.55  3.53  3.65
Xgii                       1.30  1.30  1.30  1.27
Xlog(gdprppppc)           -0.34 -0.34 -0.34 -0.35
Xprop_male                -8.17 -8.17 -8.12 -8.52
Xlog(gdprppppc):prop_male  0.87  0.87  0.86  0.90
```

The biggest difference in the coefficients is with model2, not surprising because it has the biggest difference in its specification from the other three. The log transformation is done before modelling and the response is then treated as Gaussian, as opposed to the quasipoisson link and variance functions approach of the other three.

Note that from the above snippet of code that not least of the differences between these models is the different techniques used to extract those fixed coefficients.

#### Getting the same group level random effects

Another check that these models are basically the same was to compare the country-level random effects. For example, is the "Oman" effect going to be the same in each of these four models?

To answer this I first had to work out how to extract the random effects from the versions that used the `s(country_fac, bs = 're')` notation to set the random effects. It turns out the best way to do this is with the [`gratia` package](https://gavinsimpson.github.io/gratia/) by Gavin Simpson (again), which has the `smooth_coefs` function for this and related purposes. So this next chunk of code extracts all those country effects and draws a pairs plot of them.

{% highlight R lineanchors %}
tibble(
   rf2 = ranef(model2)[[1]][, 1],
   rf7a = smooth_coefs(model7a, "s(country_fac)"),
   rf7b = ranef(model7b$lme)[, 1],
   rf7c = smooth_coefs(model7c, "s(country_fac)")
) |> 
   ggpairs()
{% endhighlight %}

Which gives this result, with a pleasing high correlation in the country effects of the four different models:

<object type="image/svg+xml" data='/img/0290-country-effects-pairs.svg' width='100%'><img src='/img/0290-country-effects-pairs.png' width='100%'></object>

Again, model2 is a little different from the other three, for the same reason. I'm actually struck with how much we get near-identical results in a model that does the log transformation before modelling to those that use a log link function.

### Showing marginal effects

At some point when playing around with the different ways of specifying models I was having trouble understanding some of the output&mdash;some coefficients I thought should be identical weren't&mdash;and started building my own, very basic predicted mean values, by multiplying numbers by the coefficients. The original problem went away when I discovered some mistake or other, but I repurposed what I'd done into the code to produce this plot. 

This is the sort of plot I'd been imagining to use to illustrate the interaction of the male housework variable with GDP per capita. I'd been expecting to see something like this once I'd seen the direction of the trend swap around in high income countries compared to low income countries:

<object type="image/svg+xml" data='/img/0290-home-made-preds.svg' width='100%'><img src='/img/0290-home-made-preds.png' width='100%'></object>

This plot was produced with this very hacked-together, brittle, function that multiplies variables by their coefficients:

{% highlight R lineanchors %}
# Manual way of building a plot. Not even using predict()
b <- fixef(model2)

#' Predict TFR given those coefficients
calc_tfr <- function(prop_male, gdp, gii = mean(model_ready$gii)){
  exp(b[1] + 
      b[2] * gii + 
      b[3] * log(gdp) + 
      b[4] * prop_male + 
      b[5] * prop_male * log(gdp))
}

# Home-made prediction plot to show the interaction effect:
tibble(prop_male = rep(seq(from = 0.05, to = 0.45, length.out = 50), 3),
       gdp = rep(c(3000, 10000, 80000), each = 50)) |> 
  mutate(tfr = c(calc_tfr(prop_male, gdp)),
         gdp = dollar(gdp),
         gdp = fct_relevel(gdp, "$3,000")) |> 
  ggplot(aes(x = prop_male, colour = gdp, y = tfr)) +
  geom_line(linewidth = 1.5) +
  geom_point(data = model_ready, colour = "black") +
  scale_x_continuous(label = percent) +
  labs(x = "Proportion of adult housework done by men",
       y = "Predicted total fertility rate",
       title = "Interaction of income, housework done by men on fertility rate",
       subtitle = "Calculations done for a hypothetical country that otherwise has the average Gender Inequality Index",
       colour = "PPP GDP per capita",
       caption = full_caption)
{% endhighlight %}

It's not something I'd use for real because I'd want to calculate the standard errors at each point too. At some point, we say that's what the various package authors gave us the `predict` method for the classes they made holding fitted models. But even using `predict` and applying it to a carefully chosen grid of values is made easy these days by the [`marginaleffects` package](https://doi.org/10.18637/jss.v111.i09), by Vincent Arel-Bundock, Noah Greifer and Andrew Heiss. I was using this for the first serious time in this exercise.

It turns out that `marginaleffects` is great for this purpose; easy to use to get you a nearly good enough plot. Here's the results of the `marginaleffects::predict_plot()`

<object type="image/svg+xml" data='/img/0290-margeff-preds.svg' width='100%'><img src='/img/0290-margeff-preds.png' width='100%'></object>

It's like my home-made plot, but better in at least one respect; it has confidence intervals. There were some hitches in scales and guides:

* the y axis really wanted to be labelled on the scale of the linear predictor, and in the end I let it have its way and add a secondardy axis on the right hand side labelled on the original scale
* controling the colour scale looked to be non-trivial, as did labelling it with $ signs. In the end I didn't persist on this; there are ways to get `plot_predictions` to give you the data rather than draw a plot, but I didn't slow things down.

Here's the nice and simple code to draw that; the "nice and simple" bit partiuclarly referring to the easy way you can specify the variables' values to illustrate. Once I'd realised how easy this was, I used it for the rest of the blog post, including the considerably more complex generalized additive models that were my actual preferred models.

{% highlight R lineanchors %}
plot_predictions(model2, points = 1, condition = list(
  "prop_male",
  "gdprppppc" = c(3000, 10000, 80000))) +
  scale_y_continuous(trans = transform_exp(),
                     breaks = log(c(2, 4, 6)),
                     label = comma,
                     sec.axis = sec_axis(exp, name = "Total Fertility Rate")) +
  scale_x_continuous(label = percent) +
  labs(y = "log(total fertility rate)",
       colour = "PPP GDP per capita",
       fill = "PPP GDP per capita",
       x = "Proportion of adult housework done by men",
       title = "Interaction of income, housework done by men on fertility rate",
       subtitle = "Calculations done for a hypothetical country that otherwise has the average Gender Inequality Index",
       caption = full_caption)
# note the warning that this only takes into account the uncertainty of
# fixed-effect parameters. This is probably ok? if we are interested in 
# the causality rather than predicting new countries?
{% endhighlight %}

### Modelling choices and checks

#### Detoured into the garden of forking paths?
Now, all that stuff in the previous section is mostly cosmetics. Keen readers will have noticed that the model described there is not the one I used in the main blog at all. In particular, it has a straightforward linear interaction of GDP per capita and male housework, whereas I ultimately used a smoothed spline interaction instead; I added a smoothed time effect; and made gender inequality index also a non-linear spline.

To refresh memories, the two models contrasted in the main blog were these two:
{% highlight R lineanchors %}
model4b <- gam(tfr ~ s(time_period) + s(gii, k = 3) + s(log(gdprppppc)) + s(country_fac, bs = 're'), 
                data = model_ready, family = quasipoisson, method = "REML")

model6b <- gam(tfr ~ s(time_period) + s(gii, k = 3) + s(log(gdprppppc), prop_male) + s(country_fac, bs = 're'), 
                data = model_ready, family = quasipoisson, method = "REML")
{% endhighlight %}

I found `model6b` explained virtually no extra deviance compared to `model4b`. The difference between `model6b` and the `model2` used above is all the `s()` splines, and the nuisance effect of `time_period` being controlled for.

If you look at the plot in the previous section showing the marginal effect of increased male domestic work from the linear model with no splines, it appears to be significant. And the output seems to confirm this&mdash;the summary below shows the male domestic work as negatively related to fertility, and its interaction with GDP per capita as positively related (so for higher GDP per capita AND higher levels of male housework, total fertility rate goes up). These are clearly significant at conventional levels. And **this is the opposite of what I reported in my blog**, which was that there was no male housework impact on fertility when we control for gender inequality and GDP per capita.

```
> summary(model2, cor = FALSE)
Linear mixed model fit by REML ['lmerMod']
Formula: ltfr ~ gii + log(gdprppppc) * prop_male + (1 | country_fac)
   Data: model_ready

REML criterion at convergence: -149.8

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-3.03513 -0.41571  0.07449  0.46057  2.51483 

Random effects:
 Groups      Name        Variance Std.Dev.
 country_fac (Intercept) 0.037475 0.1936  
 Residual                0.008724 0.0934  
Number of obs: 172, groups:  country_fac, 79

Fixed effects:
                         Estimate Std. Error t value
(Intercept)               3.65360    0.60667   6.022
gii                       1.26916    0.20080   6.320
log(gdprppppc)           -0.34957    0.06275  -5.571
prop_male                -8.51667    2.05957  -4.135
log(gdprppppc):prop_male  0.90124    0.21501   4.192
```

This is where I come to a problem that actually worries me&mdash;did I go down the garden of forking paths? You could accuse me of making the model more complex&mdash;by adding a time effect, non-linear gender inequality effect and GDP per capita effects&mdash;until I got the desired result, of no remaining deviance explained by male time spent on housework. 

My defence against this has to be that I always intended to add in these non-linear effects, and that I'd only stopped to specify these models without them because I wanted to check out the `lmer` v `gam` v `gamm` specification question. And this is true. But it's also true that I had expected that even without non-linear splines added, the male time on housework would be non-significant; an expectation that turned out to be mistaken.

In fact, before going down the `lmer` v `gam` v `gamm` rabbit hole, I had started with a `model0`, specified by this:

{% highlight R lineanchors %}
# A basic null model:
model0 <- lmer(ltfr ~ gii + log(gdprppppc)  + (1 | country_fac), 
                data = model_ready)
{% endhighlight %}

I had then done this diagnostic check, and a plot of the residual fertility rate against male housework (bottom right panel in the plot below):

<object type="image/svg+xml" data='/img/0290-diagnose-0.svg' width='100%'><img src='/img/0290-diagnose-0.png' width='100%'></object>

This seemed (visually) like only random noise remained, and I probably got careless in assuming that splines and stuff, while where I wanted to head, weren't essential, hence it was ok to fit these linear models first. It's just that when it turned out that there was an apparent "significant" effect from doing this, I was left with a dirty taste in my mouth that I was trying fitting models until I found the one that suited my expectation (of no male housework effect after controlling for gender inequality and GDP per capita).

Luckily, this is only a blog; no-one expects me to pre-register my analytical plan for it; and anyway I am convinced of the substance of the final finding; and I really do remember intending to use the versions with splines. But I'm guessing a lot of researchers feel this when they exercise their researcher degrees of freedom. 

#### Spline v tensor product smooths

When I posted the main blog, [Stephen Wild made the following comment](https://bsky.app/profile/stephenjwild.bsky.social/post/3lwgwggqdtc2z): "I'm curious about `s(log(gdprppppc), prop_male)` in your model rather than `ti(log(gdprppppc), prop_male)`". In fact, I hadn't considered this option, and I should have. So after this comment I went back and fit a new model with tensor product smooths. Note that it's also necessary to add the `ti(prop_male) + ti(log(gdprppppc)` single terms explicitly in this case, unlike when using `s()`.

{% highlight R lineanchors %}
model6c <- gam(tfr ~ s(time_period) + s(gii, k = 3) + 
                   ti(log(gdprppppc)) + ti(prop_male) + ti(log(gdprppppc), prop_male) + 
                   s(country_fac, bs = 're'), 
                data = model_ready, family = quasipoisson, method = "REML")
{% endhighlight %}

That way of specifying the individual terms with eg `ti(prop_male)` isn't one of those suggested [by Gavin Simpson here](https://gavinsimpson.github.io/nso-2022-workshop/03-gams/index.html#48); I think it's ok though. If not I'll doubtless have a future blog trying to get this straight in my head.

Now, ideally I would have a digression here where I explain the theoretical and practical differences between spline and tensor product smooths and when to use each, but I'm not feeling up to that. One thing I know is that a tensor product smooth is invariant to changes in the original scale of the variable, which would make it more robust if you're concerned about different scales of your different variables; this seems to me the main point emphasised when this issue is discussed in Gavin Simpson's [definitive book on generalized additive models in R](https://www.routledge.com/Generalized-Additive-Models-An-Introduction-with-R-Second-Edition/Wood/p/book/9781498728331)

Anyway, in this particular case there's little to chose from, as seen in this thrown together collection of plots. Those in the top show the GDP per capita and male housework effects when using a tensor product smooth; those at the bottom are the same when using a spline:

<img src="/img/0290-compare-ti-and-s.png" width = "100%">

My hunch is that the tensor product smooth is a bit better here, but I won't change history by editing the main blog to use it. The addition of the `male_prop` variable still isn't statistically 'significant'; while we can see that for higher income countries there is a bit of an upwards slope (in the top right panel of the collection of plots above), it's not explaining a material amount of extra material.

```
> summary(model6c)

Family: quasipoisson 
Link function: log 

Formula:
tfr ~ s(time_period) + s(gii, k = 3) + ti(log(gdprppppc)) + ti(prop_male) + 
    ti(log(gdprppppc), prop_male) + s(country_fac, bs = "re")

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.68636    0.02561    26.8   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                                edf Ref.df      F  p-value    
s(time_period)                2.742  3.357  5.425 0.001134 ** 
s(gii)                        1.342  1.458 33.437  < 2e-16 ***
ti(log(gdprppppc))            3.485  3.652  6.563 0.000112 ***
ti(prop_male)                 1.000  1.001  0.418 0.519799    
ti(log(gdprppppc),prop_male)  1.761  1.940  2.119 0.175333    
s(country_fac)               64.879 78.000  7.850  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.964   Deviance explained = 97.7%
-REML = -176.7  Scale est. = 0.013138  n = 172

> anova(model6c, model4c)
Analysis of Deviance Table

Model 1: tfr ~ s(time_period) + s(gii, k = 3) + ti(log(gdprppppc)) + ti(prop_male) + 
    ti(log(gdprppppc), prop_male) + s(country_fac, bs = "re")
Model 2: tfr ~ s(time_period) + s(gii, k = 3) + ti(log(gdprppppc)) + s(country_fac, 
    bs = "re")
  Resid. Df Resid. Dev      Df Deviance F Pr(>F)
1    82.875     1.2599                          
2    86.013     1.2055 -3.1375 0.054375  
```

There's negligible extra deviance explained by the model with the `prop_male` and its GDP interaction, compared to the simpler model without them.

### Concluding remarks

Actually, I don't really have much to add here.

I could have talked more about some of this, eg that unexplained `k=3` in the spline for gender inequality, and my thinking about diagnostic plots for GAMs (and my rushed, imperfect implementation of it); but at some point there are diminishing marginal returns. I've covered off the main things here; mostly things that I think future-me will want to refer to next time I'm doing similar things.

For some people, it's probably worth checking out the [full script of original code](https://github.com/ellisp/blog-source/blob/master/_working/0290-house-work.R) if there's additional points of interest or questions.
