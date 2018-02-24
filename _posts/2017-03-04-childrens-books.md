---
layout: post
title: Visualising relationships between children's books
date: 2017-03-04
tag: 
   - ClusterAnalysis
   - R
description: Statistical methods like hierarchical clustering and principal components analysis can help understand and visualise literary concepts but don't replace reading the books and engaging with them in traditional critical ways!
image: /img/0083-dendro1.svg
socialimage: http://ellisp.github.io/img/0083-dendro1.png
category: R
---
At the [OZCOTS (Australian Conference on Teaching Statistics) in late 2016](http://iase-web.org/documents/anzcots/OZCOTS_2016_Proceedings.pdf) George Cobb gave a great talk entitled "Ask not what data science can do for the Humanities. Ask rather, what the Humanities can do for data science."


> "George Cobb of Mt Holyoke College gave this keynote address to open the Australian Conference on Teaching Statistics. His approach was literary, throughtful, and totally out of left field. He had a paper handout! He exhorted us to read Dickens' *Bleak House*!"

From [ALICEINSTATISTICSLAND](https://aliceinstatisticsland.wordpress.com/2016/12/16/ask-not-what-data-science-can-do-for-the-humanities-ask-rather-what-the-humanities-can-do-for-data-science/)

One of my takeouts from that talk was that for all our modern whizz-bang tools, applied statistics is a long way from being able to analyse much of the power of literature; this doesn't mean we shouldn't apply statistical techniques to text, but we should do it humbly, always thinking of context, and with an eye to improving statistical understanding (which is still taking baby steps in this space) rather than an arrogant view that it is easy to add insight to traditional critical methods.

I found myself thinking along similar lines recently when trying to articulate common elements in literature I'd read as a child and that still stuck with me.  For example, the theme of four related, self-reliant children in exciting circumstances is common to Arthur Ransome's *Swallows and Amazons*, Enid Blyton's *Famous Five* and C. S. Lewis' *The Lion, the Witch and the Wardrobe*, while all of these books seem to me to be influenced by works of Edith Nesbit like *Five Children and It*.  But danger and fighting bad people differentiate the Famous Five and the children in Narnia from the Swallows and Amazons' playful exploration of nature in the Lake District, and the exploratory tone of *Five Children and It*.  And Nesbit and Lewis' children are in a magical world whereas the other two are in more mundane reality (relatively speaking - they still seem to get plenty of excitement).  

At one point I was thinking of using statistics to try to understand the books better, then I realised it was as much a chance to understand statistics better by applying them to well known books.  I frequently come across a problem of looking for pattern in high dimensional data and being unsure how successful methods like cluster analysis or principal components analysis have been.  Perhaps by applying those methods to books where I felt I had an intuitive feel of the structure I might learn something about the analytical methods?  Well maybe.  If nothing else, it's an excuse for drawing some charts.

## Defining and documenting one's own subjective views
To create a dataset for this, I constructed a set of thematic elements in which I was interested such as (incomplete list):

* Boarding school
* Fantasy or magic
* Summer holidays
* Hidden alternative world
* Meant to be funny

I classified a bunch of my favourite children's books (defined loosely and inarticulately) against whether they included each element.  This rating process felt a bit unreliable, not least because generally I was operating on relatively distant memory.  The choice of books was necessarily arbitrary and is basically those I enjoyed at a particular age (too old for *The Very Hungry Caterpillar*, too young for *War and Peace*), or in a few cases discovered recently and think I would have enjoyed if I'd known about them.  They very much reflect my particular history and background culture, which is clearly to be expected.

You can see the full set of 23 thematic elements, 46 books, and the 1/0 assessments in this [Excel workbook](http://ellisp.github.io/data/childrens-books.xlsx).

The whole process, particularly the choice of thematic elements to highlight, is clearly arbitrary and subjective, but in a sense that is the whole point. In this exercise, I'm seeking to define and articulate a bunch of things of interest *to me* about books that *I find interesting*, with the intent of applying statistical exploratory techniques and seeing if they identify patterns that *ring true to me*.  In doing this I'm hoping to get a visualisation of the patterns as I see them, and learn something about data exploration while I'm at it.  It's not about recovering some objective structure but about articulating my own subjective views.

## Cluster analysis

One method I tried was the "DIvisive ANAlysis clustering" algorithm, or "DIANA".  This works by dividing the observations (in this case, books) repeatedly into smaller and smaller groups, based on their similarity as recorded in my judgements against the thematic elements.  It has the advantage of coming with an intuitive (to me) display method in the form of a tree or [dendrogram](https://en.wikipedia.org/wiki/Dendrogram).  `diana` is implemented in the standard `cluster` R package, and plot polish can be added with the `dendextend` package.

<img src='/img/0083-dendro1.svg' width = '100%'>

Some of the clustering interpretation is straightforward and does indeed match my intuitions:  

- At the top of the diagram we see a set of "siblings / cousins exploring stuff" - *Swallows and Amazons*, *Famous Five* and friends.
- Below that we see a set of relatively serious and at times violent books - most of the four categories into which I divided Biggles' adventures; *The Hobbit*; and *The Black Cauldron*.  Asterix, Tintin and the Hunt Brothers' exploits in *Amazon Adventure* and sequels are offshoots of this branch.
- The bottom cluster - from *The Cat in the Hat* and friends to Edward Lear's *A Book of Nonsense* is obviously the humorous group.
- *The Little Prince*, *Charlotte's Web*, *The Secret Garden* and *Ann of Green Gables* form another relatively serious chunk, with a focus on relationships, responsibilities and friendship.

And so on.  

I was a little surprised that there wasn't a tighter "boarding school" cluster: P. G. Wodehouse's *Tales of St Austin's* (in which I mentally included all his boarding school stories), Rowling's *Harry Potter*, are distant from *Tom Brown's Schooldays*, *Molesworth* and Blyton's *Malory Towers* and *Naughtiest girl in the school* with a wide variety of other elements (such as humour, magic, earnest muscular Christianity, progressivism - listed in no particular order!), apparently largely orthogonal to the boarding school setting.

The "hidden, secret world" dimension is another that didn't seem to feature prominently in the clustering, with books as diverse as *Alice in Wonderland*, *Narnia* and *The Enchanted Wood* including this idea but not grouping together.

Here are some alternative visualisations of the same DIANA clustering; two variants on the sort of trees more usually used for phylogenetics, courtesy of the `ape` package.  First an "unrooted" version:

<img src='/img/0083-dendro2.svg' width = '100%'>

And second a "fan" version:

<img src='/img/0083-dendro3.svg' width = '100%'>

Generally, I think I prefer the very first dendrogram, which used the "triangle" method to draw the edges.

## Principal components

A second visualisation method I tried was a biplot of the first two principal components.  These plots aim to show the contribution of variables to the two aritificial dimensions that summarise the most variance possible, and allow a reasonably intuitive connection between units of observation (in this case books) and the contributing factors.  Here's the result in our case:

<img src='/img/0083-pc.svg' width = '100%'>

I have a soft spot for these sorts of charts, as a valiant effort to reduce high dimensional data to the flat page.  If the first two principal components explain a good chunk of the variance in the data they can be very powerful.  But in this case only 25 percent of the variance is explained by the first two components, so we get a very incomplete picture; seen in the way that the clusterings visible in the dendrogram are not obvious in this chart.  The graphic is an interesting one and reveals a number of the patterns I'd expected (*Secret Seven* and *Famous Five* very similar; strong "boarding school" dimension; "talking animals" and "fighting crime" pointing in different directions and essentially not appearing in the same books).  But it's definitely less comprehensive.

## Other

I experimented with some network graphs but they didn't work well, or add any insight to the above methods.

Oh, if you haven't encountered Edith Nesbit but are interested at all in this sort of thing, check out this [great 1964 article in the New York Review of Books by Gore Vidal](http://www.nybooks.com/articles/1964/12/03/the-writing-of-e-nesbit/).

## Code

Here's the R code that produces the above.  Pretty simple today.

{% highlight R %}
library(tidyverse)
library(xlsx)
library(cluster)
library(ape)
library(dendextend) 

download.file("http://ellisp.github.io/data/childrens-books.xlsx", 
              destfile = "tmp.xlsx", mode = "wb")
books_orig <- read.xlsx("tmp.xlsx", sheetIndex = 1, 
                        check.names = FALSE, stringsAsFactors = FALSE)
unlink("tmp.xlsx")

books_thin <- books_orig %>%
   gather(Trope, Present, -Author, -Title) %>%
   filter(!is.na(Present)) %>%
   as_tibble() %>%
   mutate(Present = 1)

books_wide <- books_thin %>%
   select(-Author) %>%
   spread(Trope, Present, fill = 0)

books_mat <- books_wide %>%
   select(-Title) %>%
   as.matrix()
rownames(books_mat) <- books_wide$Title


#---------cluster analysis-------
books_d <- daisy(books_mat)

cld <- as.dendrogram(diana(books_d))

# Three versions of the dendrogram:

# need to use hang.dendrogram to keep the hanging raggedness
par(mar = c(5,4,4,7), font.main = 1)
par(bg = "grey99")
plot(hang.dendrogram(cld), horiz = TRUE, yaxt = "n", type = "triangle",
     xlim = c(4.5, -1), 
     edgePar = list(col = "steelblue"),
     main = "Selected childrens books or series, clustered by thematic elements")

par(mar = c(1,1,1,1))
par(bg = "grey99")
plot(as.phylo(cld), type = "unrooted", cex = 0.6) # maybe

par(bg = "grey99")
plot(as.phylo(cld), type = "fan", cex = 0.6) # maybe

#-----------principle components-------------
books_pc <- prcomp(books_mat, scale = TRUE)

par(family = "myfont")
par(bg = "grey99")
biplot(books_pc, choices = 1:2, col = c("darkblue",  "grey75"), pc.biplot = TRUE)
{% endhighlight %}
