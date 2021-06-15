---
layout: post
title: Principal components and penguins
date: 2021-06-14
tag: 
   - Transformations
   - Tools
description: Beware that the direction of a principal component can vary depending on the sequence of the original data.
image: /img/0202-pc-comparison.png
socialimage: http://freerangestats.info/img/0202-pc-comparison.png
category: R
---

A short post today to get me back in the swing of it, after a four month break - my longest since starting the blog. But a short post with a warning - this is something that nearly caught me out and has potentially catastrophic but silent (ie unnoticed) consequences.

It's common in my corner of the world to take high dimensional data and turn it into a one-dimensional indicator using principal components analysis as part of the algorithm. For example, see [this publication](https://publications.csiro.au/rpr/download?pid=csiro:EP194014&dsid=DS7) by CSIRO which explains how an adaptive capacity index was created as the unweighted sum of three subindices: human capital, social capital and economic diversity. Those subindexes in turn were created by (in effect) taking the first principal component of multidimensional data and rescaling them from zero to one. I'm not associated with that publication at all, but the method described there is something we have reproduced a couple of times for clients.

Let's illustrate the method with the Palmer penguins data. We have 333 complete observations of penguins' bill length and depth, flipper length and body mass. If we wanted to summarise this four-dimensional data in a single-dimensional index (let's call it the Penguin Size Indicator, as obviously it will roughly indicate the size of the penguin), we might do this by taking the first principal component of a scaled version of the data. If we want an end index between zero and one due to the audience's familiarity with that range, we could then easily rescale the result. This would give us something like this:

<object type="image/svg+xml" data='/img/0202-pc1.svg' width='100%'><img src='/img/0202-pc1.png' width='100%'></object>

...created with this pretty minimal set of R code:

{% highlight R lineanchors %}
library(palmerpenguins)
library(tidyverse)
library(scales)

penguins2 <- drop_na(penguins) %>%
  mutate(row_id = 1:n())

m1 <- penguins2 %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) 

pc1 <- prcomp(m1, scale = TRUE)
penguins2$pc1 <- rescale(predict(pc1)[, 1])

penguins2 %>%
  ggplot(aes(x = body_mass_g, y = pc1, colour = species)) +
  geom_point() +
  labs(x = "Body mass in grams",
       y = "First principal component\n(rescaled from zero to 1)",
       caption = "Example data: Palmer Penguins",
       colour = "Species:",
       title = "First principal component of four-dimensional data",
       subtitle = "Fit to data in its original row order")
{% endhighlight %}

OK, great. That first principal component is positively correlated with body mass, flipper length and bill length so it's clearly reasonable to describe it as a "size" indicator, and higher numbers mean a bigger penguin. I imagine biologists are horrified at this crude procedure, but it's only for illustrative purposes.

However, principal components are only defined up to an arbitrary positive or negative sign. And the numeric calculation that takes place under the hood can be unstable and go either way, with the same data. In particular, if I just change the order of the data but otherwise do the exact same procedure, I can end up with a "size" index where 1 is actually the score of the smallest penguin and 0 that of the largest:

<object type="image/svg+xml" data='/img/0202-pc2.svg' width='100%'><img src='/img/0202-pc2.png' width='100%'></object>

Here's the code that did that. The only substantive difference from the first chunk of code is that I have sorted the data in random order (this is what happens when you create sample with same number of rows as the original, without replacement, as is being done by the `sample_n()` function).

{% highlight R lineanchors %}
set.seed(123)
m2 <- penguins2 %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, row_id) %>%
  sample_n(n())

pc2 <- prcomp(select(m2, -row_id), scale = TRUE)

penguins3 <- m2 %>%
  mutate(pc2 = rescale(predict(pc2)[, 1])) %>%
  select(row_id, pc2) %>%
  inner_join(penguins2, by = "row_id") 

penguins3 %>%
  ggplot(aes(x = body_mass_g, y = pc2, colour = species)) +
  geom_point() +
  labs(x = "Body mass in grams",
       y = "First principal component\n(rescaled from zero to 1)",
       caption = "Example data: Palmer Penguins",
       colour = "Species:",
       title = "First principal component of four-dimensional data",
       subtitle = "Fit to data in a different row order")
{% endhighlight %}

This next chart shows the relationship between the two "size" indices we have made with the same procedure. The second version is in fact one minus the first version. It is just as good an indicator of size, but it has weak interpretability because higher numbers are smaller penguins. Worse, if this was just a sub indicator on the way to being combined with others to form an overall index, it would be easy to assume after a visual check on one run of the procedure that "higher is bigger" and feed it into the overall index on that assumption - which can't be relied on for the future. If some new data comes in, or even if the process is run on the same data but in a different order, the meaning of the indicator will be exactly reversed.

<object type="image/svg+xml" data='/img/0202-pc-comparison.svg' width='100%'><img src='/img/0202-pc-comparison.png' width='100%'></object>

Where this nearly caught us out at work the data was coming from a database that was automatically rebuilt multiple times during the project, and hence the row order can differ for identical queries and data. To address the issue, we had to add an automated check for the correlation of the final index with a known trend, and swap the direction of the index if necessary. Problem averted. Very easy to get in trouble with this, I think.

Final chunk of code that was used to make that comparison of the two indexes:

{% highlight R lineanchors %}
penguins3 %>%
  ggplot(aes(x = pc1, y = pc2, colour = species)) +
  geom_point() +
  labs(x = "When fit to data in original row order",
       y = "When fit to data in a different row order",
       title = "The direction of a principal component can vary based on the data's arrangement",
       subtitle = "Comparison of the first principal component, scaled from 0 to 1, fit to the same data",
       caption = "Example data: Palmer Penguins",
       colour = "Species:") +
  coord_equal() +
  theme(plot.title.position = "plot")
{% endhighlight %}





