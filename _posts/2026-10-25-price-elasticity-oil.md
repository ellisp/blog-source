---
layout: post
title: Price elasticity of demand for crude oil
date: 2026-10-25
tag: 
   - Energy
   - Economics
   - Visualisation
   - DataFromTheWeb
description: I rework an important and useful, but difficult to read, Haver Analytics graphic illustrating illustrating how disruption to Gulf oil exports could affect crude oil prices.
image: /img/0335-lines.svg
socialimage: https:/freerangestats.info/img/0335-lines.png
category: R
---

This blog post is about me trying to make a better version of an existing diagram that I think is important, but difficult to read in the version it keeps popping up on my Bluesky feed.

Robin J Brooks of the [Brookings Institution](https://www.brookings.edu/people/robin-brooks/) has been using a useful diagram, I think since March 2026, to present a set of illustrative theoretical impacts of the USA-Iran war on crude oil prices. For example, he reproduces it in [this recent blog post](https://robinjbrooks.substack.com/p/whats-priced-into-brent-at-117). Relative to many other voices, Brooks has generally been bearish on crude oil prices in this conflict, with his approach very much based on this diagram. The diagram is attributed to [Haver Analytics](https://www.haver.com/), although I can't find the original.

I think it's an important graphic because it does make us confront that the demand curve for crude oil isn't vertical. There are in fact estimates of the price elasticity of demand for crude oil, and they vary, but certainly sometimes they come out somewhere between -0.1 and -0.2. Usually the price elasticity of demand is introduced to beginner economists as a value of -0.1 means "a price increase of 1% will lead to a decrease in the quantity demanded of 0.1%". But for this exercise, we will turn the relationship around&mdash;if the equilibrium point where supply meets demand is forced down by 0.1% (because of a shift in the supply curve), this implies a price increase of approximately 1%.

In the case of the current war, at its peak, events removed an amount of oil from the market of somewhere around 10% of normal global oil supply. Up to 20% of world supply has been exposed to risk, although I don't think we ever reached a reduction of that magnitude. The amount removed is much less right now at the time of me writing (meaning, oil is flowing at a much higher rate than a few months ago), but who knows where it will be in a few weeks. The above reasoning implies an impact of a 10% cut in supply on the price of 100% (ie doubling) if the price elasticity of demand is -0.1; or around 50% if it is -0.2. This is basically what the diagram used by Brooks shows.

In doing this, the graphic uses a linear elasticity approximation. I'm not sure this really applies for the scale of change we're talking about here, with a 10% or 20% drop in supply. But it's a starting point.

Also note that none of this factors in a change in the demand curve. These are changes up and down the existing demand curve. Over a longer period, the curve will shift with changes in income, policy, buyer attitudes and technology, in particular through the obvious mechanism of accelerated transition to clean, renewable electricity. For now, we are putting those changes aside.

We're also putting aside how drawing down on existing inventory can (and certainly did) cushion markets from the reduction in supply. We're trying to illustrate a point and get a plausible range given various assumptions; not actually forecast prices.

## The perspective plot

Unfortunately, I find the chart from Haver Analytics very difficult to follow. It's trying to show the relationship between two key variables&mdash;the oil successfully exported from the Gulf, and resulting impact on prices&mdash;and one uncertain parameter, the short-term price elasticity of demand for crude oil. We don't know this elasticity for sure, but it's plausibly somewhere between -0.1 and -0.2. So we want to see a range of impacts on price given this unknown parameter. 

Note: in the plots below, I follow Haver Analytics in showing the absolute magnitude of the price elasticity of demand, dropping the negative sign.

To deal with this, the original uses a 3-dimensional perspective plot. Here's my rough reproduction in R of the original chart:

<object type="image/svg+xml" data='/img/0335-persp.svg' width='100%'><img src='/img/0335-persp.png' width='100%'></object>

I think my version is actually a bit easier to read than Haver Analytics' original because of the colour shading, but it's only a marginal improvement. The issue is the common one with these sorts of 3-dimensional plots&mdash;it's just very hard to read across from the flexing surface to the values on the three axes, and it's even harder to take in a general trend or tendency in the way we find straightforward with a 2-dimensional scatter plot or line plot. 

I simulated the illustrative data behind the diagram and made that 3-dimensional perspective plot with this code:

{% highlight R lineanchors %}
library(tidyverse)
library(plot3D)
library(scales)
library(glue)
library(RColorBrewer)

#---------------Simulating data---------------------------

global_oil_consumption  <- 100
original_gulf_exports <- 20
prewar_oil_price <- 72.5

elasticities <- seq(from = 0.1, to = 0.2, length.out = 11)
exports <- 2:original_gulf_exports
length(exports)
length(elasticities)

d <- expand_grid(
    elasticity = elasticities,
    export = exports
) |>
    mutate(
        lost_oil = (original_gulf_exports - export) / global_oil_consumption ,
        price_rise = lost_oil / elasticity * 100
    )

#---------------common caption and title across all plots-------------------

the_caption <- str_wrap(
    glue(
        "Illustrative calculation assuming global oil consumption of ",
        "{global_oil_consumption } million barrels per day and pre-war Gulf exports of ",
        "{original_gulf_exports} million barrels per day. ",
        "Re-worked by freerangestats.info based on original analysis by Haver Analytics."
    ),
    115
)
the_title <- "Illustrative impact of Gulf export cuts on oil prices"
#----------------Perspective plot---------------
# this recreates the original plot by Haver Analytics

prices_matrix <- d |>
    arrange(export, elasticity) |>
    select(elasticity, export, price_rise) |>
    pivot_wider(names_from = elasticity, values_from = price_rise) |>
    select(-export) |>
    as.matrix()

reds <- colorRampPalette(c("mistyrose", "darkred"))(100)

persp3D(
    x = exports,
    y = elasticities,
    z = prices_matrix,
    col = reds,
    xlab = "\nOil exported from the Gulf",
    ylab = "\n\nPrice elasticity of demand",
    zlab = "\nRise in oil prices (%)",
    main = the_title,
    # box type must be one of “b”, “b2”, “f”, “g”, “bl”, “bl2”, “u”, “n”
    bty = "g",
    ticktype = "detailed",
    colkey = FALSE
)

mtext(
    the_caption,
    side = 1,
    line = 4,
    cex = 0.8,
    col = "grey30"
)
{% endhighlight %}

## Improvement 1: heatmap

Often, a perspective plot is improved by turning it into some kind of heatmap. These use the same type of data&mdash;a grid of values on x and y axes, with a variable of interest on a third dimension. In the perspective plot, that third variable is mapped to the z vertical axis; in a heatmap we map it to the colour of the fill.

Here's my first attempt at improving the perspective plot by redrawing it as a heatmap:

<object type="image/svg+xml" data='/img/0335-heatmap.svg' width='100%'><img src='/img/0335-heatmap.png' width='100%'></object>

I think this is already a big improvement, but it's come at the cost of nearly turning the graphic into a table. This is easier to read, but doesn't feel like we're really using the power of visualisation.

Here's the code that draws the heatmap version:

{% highlight R lineanchors %}
#-----------------Heat map--------------------
elasticities2 <- c(0.1, 0.12, 0.14, 0.16, 0.18, 0.2)
exports2 <- 1:10 * 2

p1 <- d |>
    filter(round(elasticity, 2) %in% elasticities2) |>
    filter(export %in% exports2) |>
    mutate(text_col = ifelse(price_rise >= 100, "Pale", "Dark")) |>
    ggplot(aes(x = export, y = elasticity, fill = price_rise)) +
    geom_tile() +
    geom_text(
        aes(label = percent(price_rise, scale = 1), colour = text_col),
        size = 4
    ) +
    scale_fill_gradientn(colours = reds, label = percent_format(scale = 1)) +
    scale_y_continuous(breaks = elasticities2) +
    scale_x_continuous(breaks = exports2) +
    scale_colour_manual(
        values = c("Pale" = "white", "Dark" = "black"),
        guide = "none"
    ) +
    labs(
        title = the_title,
        subtitle = str_wrap(
            "Illustrative price increase for crude oil depends on the price elasticity of demand and the amount of oil that is exported from the Gulf.",
            100
        ),
        x = "Oil exported from the Gulf (in million barrels per day)",
        y = "Price elasticity of demand for crude oil",
        fill = "Price increase for crude oil",
        caption = the_caption
    )

print(p1)
{% endhighlight %}

## Improvement 2: line plot

So I had a go at a line plot. I think the most important relationship here is between the amount of oil successfully exported from the Gulf and the change in world crude oil price. So I make this the x and y axes. The visual relationship between x and y is the most striking thing for many readers of charts; certainly for me. The uncertainty about the price elasticity of demand is of secondary interest, and we can show that just by drawing a different line for a bunch of different plausible values.

Here's what I came up with:

<object type="image/svg+xml" data='/img/0335-lines.svg' width='100%'><img src='/img/0335-lines.png' width='100%'></object>

I think this is definitely the best version. 

One of its advantages is that I can use a secondary axis to convert the percentage price increases into an implied dollar value per barrel, based on a pre-war price of around US$72.50 per barrel. This is a nice addition for comprehensibility.

I think the fan of colour-coded lines is also a good way of showing the sensitivity of results to the different possible assumptions about the price elasticity of demand. I've deliberately colour-coded these based on the distance from the middle of the range of possible assumptions, rather than from lowest to highest - because I think this better draws attention to this as a type of uncertainty.

But the most important thing with this chart is definitely that we have put the two most important variables on the x and y axes. It means, for example, we can simply see the implied relationship as a trend, which is so hard to see in the original. 

It also makes it easy to read across from today's price and up from the crude exports level (no-one knows what this is today, but say somewhere between 7 and 15 million barrels a day) to hit the fan of different lines, and see a) what any particular value implies about the price elasticity of demand and/or b) if there is an extra price impact being built in, perhaps from other information on inventories, perceived risk, known coming changes in supply and demand, etc.

For what it's worth, with plausible values for today, the chart implies that there is a bit extra built into the prices. My presumption is that this is likely a (whether rationally founded or not) fear that the exports could well rapidly drop from the current reasonably high levels.

Here's the code for drawing the line plot version:

{% highlight R lineanchors %}
#-----------------line plot------------------
annot_col <- "grey12"

p2 <- d |>
    mutate(
        elasticity_distance = abs(elasticity - 0.15),
        is_midpoint = ifelse(round(elasticity, 2) == 0.15, "solid", "dashed")
    ) |>
    ggplot(aes(
        x = export,
        y = price_rise,
        group = elasticity,
        colour = elasticity_distance
    )) +
    geom_line(aes(linetype = is_midpoint)) +
    geom_text(
        data = filter(
            d,
            export == min(export),
            round(elasticity, 2) %in% c(0.1, 0.15, 0.2)
        ),
        aes(label = glue("Elasticity = {elasticity}")),
        hjust = 1,
        position = position_nudge(x = -0.1),
        colour = annot_col,
        size = 2.8
    ) +
    scale_linetype_manual(
        values = c("solid" = 1, "dashed" = 5),
        guide = "none"
    ) +
    scale_y_continuous(
        label = percent_format(scale = 1),
        sec.axis = sec_axis(
            ~ prewar_oil_price * (1 + . / 100),
            name = "Implied crude oil price (US$/barrel)",
            label = dollar
        )
    ) +
    annotate(
        "text",
        x = 17,
        y = 0,
        hjust = 1,
        size = 2.5,
        label = "Pre-war situation",
        colour = annot_col
    ) +
    annotate(
        "segment",
        x = 17,
        xend = 19.5,
        y = 0,
        yend = 0,
        arrow = arrow(length = unit(0.15, "cm")),
        colour = annot_col
    ) +
    expand_limits(x = c(0, 20)) +
    scale_colour_gradientn(
        colours = brewer.pal(9, "Blues")[9:4],
        breaks = c(0, 0.025, 0.05),
        limits = c(0, 0.05)
    ) +
    labs(
        x = "Oil exported from the Gulf (million barrels per day)",
        y = "Price increase for crude oil",
        colour = "Distance from midpoint of assumed elasticity range:",
        title = the_title,
        subtitle = str_wrap(
            "Illustrative price increase for crude oil depends on the price elasticity of demand and the amount of oil that is exported from the Gulf.",
            100
        ),
        caption = the_caption
    ) +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey88"),
        panel.grid.minor = element_blank()
    )

print(p2)
{% endhighlight %}

That's all for today. I like my final graphic, but I like the underlying analytical framework even more. I haven't set out to criticise the original graphic, but to redraw the chart in a way that's easier for me to read. Turns out the most important thing is to put the two most important variables on the x and y axes.
