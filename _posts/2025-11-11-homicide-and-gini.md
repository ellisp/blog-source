---
layout: post
title: Homicide and income inequality
date: 2025-11-11
tag: 
   - ModellingStrategy
   - Inequality
   - Crime
   - Transformations
   - DataFromTheWeb
description: Countries with higher income or consumption inequality tend to have more homicides per population. A multilevel model with a random country effect shows a much weaker relationship than a naive one that treats each repeated country observation as equally valuable.
image: /img/0303-scatter.png
socialimage: https:/freerangestats.info/img/0303-scatter.png
category: R
---

Fresh from the over-long time I'd spent on country regressions when looking at male time on domestic work and total fertility rates, I saw [this post (toot?) on Mastodon](https://mastodon.sdf.org/@dlakelan/115055789165555934) from Daniel Lakens mentioning in passing that "On the other hand, the homicide rate across the world is well modeled as exp(k * income_gini_coefficient)." This struck me as worth checking; it turns out it is more or less correct.

Here is a chart of homicides per 100,000 population on the vertical axis versus income or consumption inequality on the horizontal. 

<object type="image/svg+xml" data='/img/0303-scatter.svg' width='100%'><img src='/img/0303-scatter.png' width='100%'></object>

The model implied by Professor Lakens would be a straight diagonal line, what you'd get if you just used `geom_smooth(method = lm, formula = y ~ x - 1)`. I haven't shown his, but two slightly more complex models as I started down a rabbit hole a bit different from the original point being made.


{% highlight R lineanchors %}


{% endhighlight %}

