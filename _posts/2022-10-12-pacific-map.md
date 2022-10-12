---
layout: post
title: Pacific island choropleth map
date: 2022-10-12
tag: 
   - DataVisualization
   - WorkRelated
   - Pacific
description: I show how to draw a choropleth map of Pacific Island countries and territories.
image: /img/0241-map1.png
socialimage: http://freerangestats.info/img/0241-map1.png
category: R
---

So I wanted a nice clean map of the Pacific Island countries and territories that I work with in my day job and a workflow I could use for producing statistical graphics, particularly choropleth maps. I am going to build this into my `frs` R package to make it easier to re-use, but today's blog is my prototype putting it together from first principles.

First, here's the end product. As sample data I've used population per square kilometre of the exclusive economic zone, which is a slightly unusual metric that I was interested in at the time.

<object type="image/svg+xml" data='/img/0241-map1.svg' width='100%'><img src='/img/0241-map1.png' width='100%'></object>



One of the parts of the Pacific Data Hub that is most developed is [PDH.Stat](https://stats.pacificdata.org/), 





{% highlight R lineanchors %}



{% endhighlight %}

