---
layout: post
title: Visualising income inequality social tables
date: 2026-02-08
tag: 
   - Economics
   - History
   - Inequality
   - Visualisation
description: Visualising income inequality social tables as a scatter plot rather than a dual axis bar/line plot. I use examples from eighteenth and nineteenth century France, England and Wales drawing on Branko Milanovic's <i>Visions of Inequality</i> 2023 publication. 
image: /img/0311-quesnay-scatter.svg
socialimage: https:/freerangestats.info/img/0311-quesnay-scatter.png
category: R
---
I recently read Branko Milanovic's 2023 book, [*Visions of Inequality: from the French Revolution to the Cold War*](https://www.goodreads.com/book/show/194208539-visions-of-inequality) and gave it five stars. The bulk of the book is an excellent critical look at the views of six major economic thinkers&mdash;Quesnay, Smith, Ricardo, Marx, Pareto and Kuznets&mdash;on economic inequality. Even though the first four of these (yes, even Marx) wrote little directly on inequality narrowly defined, their class-based approach to understanding the economy has critical implications for the subject.

One of the things Milanovic does well is place each thinker in the context of economic inequality in the time they were writing&mdash;both as it was known to themselves, and as per our best modern estimates. There's a *lot* of data and tools available to us that were not there for contemporary commentators, but each of these thinkers did exceptionally well with the information they had, and has been chosen because they combine narrative, theory, and empirical work in a way that earns our respect and is still fruitful. 

After a chapter on the barren world of cold war economic inequality analysis, in his final chapter Milanovic identifies three factors that have made 21st century inequality studies take off. These are Piketty's new, insightful and influential analysis on the implications of a rate of profit that is persistantly greater than economic growth;  new data, tools and concepts relating to 'global inequality'; and new historical empirical work including the increasing volume and quality of 'social tables' setting out income and population by class.

I'm using this blog post to explore an alternative visualisation for these social tables. 

## France in the time of Quesnay
Milanovic makes good use of charts like (but not exactly like) this one, and anyone wondering what a social table is can think about them as just the tabular version of the data on income and population by class shown here:

<object type="image/svg+xml" data='/img/0311-quesnay-bar.svg' width='100%'><img src='/img/0311-quesnay-bar.png' width='100%'></object>

I've added the colour for the two different axes&mdash;I think this is super-helpful for dual axes plots to work, as [I've written about in 2016](/blog/2016/08/18/dualaxes), but obviously impossible in a grayscale publication&mdash;and used data-specific vertical axis labels rather than regular gridlines, in a nod to the sort of style Tufte might like. But otherwise this is pretty much the plot format used by Milanovic.

The estimates here are actually those of Quesnay himself, in Mirabeau's *La philosphie rurale*, the physiocrat masterwork which really set out to be the definitive book of the French economy. Quesnay himself has a fair claim to being the world's first modern economist. Some of his categories look a little odd to us, such as the very French category just for self-employed viticulturalists; the non-existence of capitalists other than tenant farmers; or perhaps most importantly, wrapping all the first and second "estates" (clergy and aristocrats) into one category along with their administrative support. This last has the effect of hiding some pretty material income disparities.

This plot type is ok but I definitely found a bit difficult to absorb. I found myself looking class by class at the numbers and effectively converting them to a table in my head, usually a sign that we're not using the power of data visualisation to its best. 

I thought the obvious alternative is a scatter plot so I drew this one:

<object type="image/svg+xml" data='/img/0311-quesnay-scatter.svg' width='100%'><img src='/img/0311-quesnay-scatter.png' width='100%'></object>

Again, I've used data-specific labels on the axes&mdash;this only works when you've only got a small number of data points. I've reduced a lot of clutter (gridlines, etc.) and made the points' labels a bit of a background colour relative to the points themselves. But it's a pretty straightforward plot altogether. I think it will work for many audiences, and I like it.

Here's the code to create the data (such a small number of points it's ok to just hard-key it into an R script) and draw that first bar and line plot. It's a bit complex because of the micro control I'm taking over things like where the breaks and labels go on the axes and the colours of the axes. But it's all well within a regular approach to `ggplot2` graphics, helped out just with `ggtext` to get italics for the references in the subtitle and caption.

{% highlight R lineanchors %}
#==================setup==================
library(tidyverse)
library(ggtext)
library(ggrepel)


# some general graphics parameters:
inc_col <- "red"
pop_col <- "blue"

theme_set(
  theme_minimal(base_family = "Roboto") +
    theme(
      panel.grid.minor = element_blank(),
      plot.subtitle = element_markdown(),
      plot.caption = element_markdown(colour = "grey50"),
      plot.title = element_markdown(family = "Sarala")
    )
)

#==============Quesnay France 1763=====================

d1 <- tribble(~population, ~income, ~class, ~class_detail,
              48, 0.5, "Workers",            "Agricultural labourers",
              22, 0.6, "Workers",            "Manufacturing low-skill workers",
              6, 0.8, "Self-employed",       "Self-employed in viticulture",
              4, 2.3, "Self-empoyed",        "Artisans and crafsmen in manufacturing",
              8, 2.7, "Capitalists",         "Tenant farmers",
              12, 2.3, "The Elite",          "Landlords, clergy, government administrators"
              ) |> 
  mutate(country = "France",
         period = "1763",
         class = factor(class,levels = unique(class)),
         class_detail = str_wrap(class_detail, 25),
         class_detail = factor(class_detail, levels = class_detail),
         pop_prop = population / max(population),
         inc_prop = income / max(income),
         pop_ratio = max(population),
         inc_ratio = max(income))


stopifnot(sum(d1$population) == 100)


#-------------dual axis bar and line chart-----------------
# as per Milanovic's style

d1 |> 
  ggplot(aes(x = class_detail)) +
  # we want the 'gridlines' to be coloured, match the values, and behind the columns:
  geom_hline(yintercept = c(0, 1/2.7, d1$inc_prop), colour = inc_col, alpha = 0.1) +
  geom_hline(yintercept = c(0, d1$pop_prop), colour = pop_col, alpha = 0.1) +
  # columns for population:
  geom_col(aes(y = pop_prop), fill = pop_col, alpha = 0.7) +
  # lines and points for income:
  geom_line(aes(x = as.numeric(class_detail), y = inc_prop), colour = inc_col) +
  geom_point(aes(x = as.numeric(class_detail), y = inc_prop), colour = inc_col) +
  # annotated labels, also colour coded:
  annotate("text", x = 4.2, y = 0.8, label = "Relative income (right axis)", colour = inc_col, hjust = 0) +
  annotate("text", x = 1.6, y = 0.6, label = "Population share (left axis)", colour = pop_col, hjust = 0) +
  # two different sets of labels for the different variables:
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.1),
                     breaks = c(0, d1$pop_prop), labels = c(0, d1$population),
                     sec.axis = dup_axis(breaks = c(0, 1/2.7, d1$inc_prop), 
                                         labels = c(0, "1.0", d1$income), 
                                         name = "Income relative to the mean (1.0)")) +
  labs(x = "",
       y = "Percentage of poulation",
       title = "Contemporary understanding of income inequality in France in the time of Louis XV",
       subtitle = "Class-based income distribution in *La philosophie rurale* by Mirabeau and Quesnay, 1763. Gini estimated to be between 49 and 55.",
       caption = "Quesnay's original estimates, reproduced in Table 1.1 of Milanovic's *Visions of Inequality*, and plot style adapted from Milanovic's.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        axis.line.y.right = element_line(colour = inc_col),
        axis.line.y.left = element_line(colour = pop_col),
        axis.title.y.left = element_text(colour = pop_col),
        axis.title.y.right = element_text(colour = inc_col),
        axis.text.y.left = element_text(colour = pop_col),
        axis.text.y.right = element_text(colour = inc_col))
{% endhighlight %}

As an aside, relating to why I don't use a large language model to help me write code for my blog: this sort of code is exactly the situation where I like writing code, not trying to explain in natural language to a computer what I want doing. I feel the `ggplot2` syntax is exactly the right combination of precision, concision and legibility. Anything I said in English that was as precise about what I wanted to do would take longer to write (and definitely to polish) than the R code.

Next up is the code for the scatter plot version. This is a bit shorter, mostly because we are using the same data and setup as the last chart, but partly because there's less fiddly customisation needed as I'm not having to specify the dual axis complications of the bar/line chart.

{% highlight R lineanchors %}
#---------------scatter plot--------------

d1 |> 
  ggplot(aes(x = population, y = income, label = class_detail)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey80") +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  annotate("text", x = 30, y = 1.1, label = "Average income", colour = "grey80") +
  scale_x_continuous(breaks = c(0, d1$population), limits = c(0, 50), expand = c(0,0 )) +
  scale_y_continuous(breaks = c(1, d1$income), limits = c(0, 3), expand = c(0,0 )) +
  labs(y = "Income relative to the mean",
     x = "Percentage of population",
     title = "Contemporary understanding of income inequality in France in the time of Louis XV",
     subtitle = "Class-based income distribution in *La philosophie rurale* by Mirabeau and Quesnay, 1763. Gini estimated to be between 49 and 55.",
     caption = "Quesnay's original estimates, reproduced in Table 1.1 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))
{% endhighlight %}


## England and Wales in the time of Adam Smith
The second economist Milanovic considers in his book in chronological order is of course Adam Smith himself. Here we move to our contemporary (21st century) understanding of income inequality and make use of social tables reconstructed by Robert Allen from contemporary sources. Here's my scatter plot version of some of the data used by Milanovic:

<object type="image/svg+xml" data='/img/0311-smith-scatter.svg' width='100%'><img src='/img/0311-smith-scatter.png' width='100%'></object>

There's white space at the top as a result of giving the vertical axis the same scale as the plot from the time of Ricardo (see a bit later in this post).

A thing that leaps out of course is the high income of England and Wales' aristocratic landowners at the time relative to other groups, and the way the other groups are compressed vertically as a result. Sometimes we would use a logarithmic transform of the income variable to show the variation. This would give us a plot like this one:

<object type="image/svg+xml" data='/img/0311-smith-scatter-log.svg' width='100%'><img src='/img/0311-smith-scatter-log.png' width='100%'></object>

But I don't much like this for our purpose. After all we are reading a book about inequality. I think the original scale is better, and the way the landed aristocracy sit up at the top by themselves is the point!

Here's the code for those two charts:

{% highlight R lineanchors %}
#==============Adam Smith's time 1759==========

# Allen, Robert C. “Class Structure and Inequality during the Industrial
# Revolution: Lessons from England’s Social Tables, 1688-1867.” The Economic
# History Review 72, no. 1 (2019): 88–125.

#page 105 of Allen for % ofpopulation but I am using Milanovic's labels from his Figure 2.1
# page 106 for income in pounds

d2 <- tribble(~population, ~income, ~class,
              1.5, 452.78, "Landed aristocracy",
              4.2, 145.37,  "Capitalists",
              9.4, 27.17, "Shop owners",
              18.9,  21.57, "Peasants",
              56.4, 13.58, "Workers",
              9.6, 3.62  , "Paupers"
              ) |> 
  mutate(year = 1759)

avg_inc <- 23.14
# note, not the same as:
mean(d2$income)


bry <- round(d2$income)[c(1:3, 6)]
brx <- c(0, round(d2$population, 1))[c(1:6)]

p3 <- d2 |> 
  ggplot(aes(x = population, y = income, label = class)) +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  scale_x_continuous(breaks = brx, limits = c(0, 65), expand = c(0, 0)) +
  scale_y_continuous(breaks = bry, limits = c(0, 800), expand = c(0, 0)) +
  labs(y = "Income in pounds",
       x = "Percentage of population",
       title = "Modern understanding of income inequality in England and Wales in 1759",
       subtitle = "Average income by earner in pounds per year, as estimated in 2019. Gini index between 45 and 51.",
       caption = "Robert Allen, “Class Structure and Inequality during the Industrial
Revolution: Lessons from England’s Social Tables, 1688-1867.”<br>*The Economic
History Review 72*, no. 1 (2019): 88–125, reproduced in Figure 2.1 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))

svg_png(p3, "../img/0311-smith-scatter", w = 10, h = 6)


p4 <- d2 |> 



{% endhighlight %}


## England and Wales in the time of David Ricardo

<object type="image/svg+xml" data='/img/0311-ricardo-scatter.svg' width='100%'><img src='/img/0311-ricardo-scatter.png' width='100%'></object>



# France in the time of Marx

<object type="image/svg+xml" data='/img/0311-marx-scatter.svg' width='100%'><img src='/img/0311-marx-scatter.png' width='100%'></object>



{% highlight R lineanchors %}



{% endhighlight %}

