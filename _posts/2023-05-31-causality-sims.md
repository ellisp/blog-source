---
layout: post
title: Simulating confounders, colliders and mediators
date: 2023-05-31
tag: 
   - ModellingStrategy
description: I do some simulations to show different data where the causal relationship between x and y is in the presence of a third variable that is either a confounder, collider or mediator.
image: /img/0247-results-with-dags.svg
socialimage: http://freerangestats.info/img/0247--results-with-dags.png
category: R
---


So [this tweet](https://twitter.com/cremieuxrecueil/status/1662882966857547777) came across my feed. It refers to [this article, *Statistical Control Requires Causal Justification*](https://journals.sagepub.com/doi/10.1177/25152459221095823) by Wysocki, Lawson and Rhemtulla.  I was struck by the clarity of Figure 4 from that paper, for at once explaining the difference between a confounder, collider and mediator and showing how they have different impacts on a regression and hence lead to different decisions about whether to include it in the model. The image comes from  What I'm showing here isn't the original, but my own version of it, which I'll eventually come to describing how I did it later in this post:

<object type="image/svg+xml" data='/img/0247-results-with-dags.svg' width='90%'><img src='/img/0247-results-with-dags.png' width='90%'></object>

The idea is that in all three cases we are interested in the degree to which X causes Y. A third variable C is also present and is correlated with both X and Y. Should we 'control' for it, ie include it as another variable on the right hand side of a regression? The answer is, it depends on whether C is a confounder, collider or mediator - and there will be no way to tell from your data which it is. So some kind of reasoning based on prior knowledge and theorising is required, or making (and documenting) some explicit assumptions. Some sciencing is required before you do the actual model specification and fitting.

The basic interpretation of these terms is as follows (as applies to most things I write about, this isn't my specialist area, so if a genuine expert sees an error please point it out). For illustrative purposes I am going to use examples from a hypothetical situation where we want to understand the impact of education on income; that is, X is education and Y is income:

- a *confounder* is when C impacts on both X and Y. For example, gender is known to lead to different education outcomes, and it also leads to differences in income in various complicated ways. Another example might be innate ability, if such a thing exists and could be measured. You want to control for confounders, because if you don't include them in the regression, some of the coefficient in front of X is going to be picking up causal impacts that are really down to the confounder. In the chart on the right we see that the green 'simple coefficient' - the coefficient we'd estimate for X in a simple regression of Y ~ X - gets the wrong result. The correct value of the impact of X on Y is 0.15, and to get this you need the yellow 'partial coefficient' ie the coefficient for X you get when you control for C, in a regression of Y ~ X + C. How far out the simple coefficient is depends on the size of a, the causal effect of C on X; if a is zero then both methods get you the same answer
- a *collider* is when both X and Y impact on C, but it itself does not impact on either of them. It can be harder to think of pure examples of these, but in my case I'm going to use hobbies as one. The hobbies you've got definitely depend on what you were exposed to in your education; and whether you can afford them financially and timewise will depend on your income (in complicated ways). True, hobbies will also potentially impact on your income but I'm hoping (for purposes of illustration) in ways that are less important than the Y -> C part of the diagram here. You *don't* want to control for colliders in a regression (see how the green 'simple coefficient' line correctly estimates the true causality of X -> Y, whereas the yellow 'partial coefficient' line doesn't). The intuition for this is that because C doesn't really impact on Y, when you include it in the regression some of the true impact of X on Y is falsely attributed to C just because of its correlation with both the important variables. (Side note - I'm not thrilled with the example of 'hobbies' here, but it serves the point. I also considered 'suburb you live in' and 'sports team you support', none of which are pefect either.)
- a *mediator* is when X causes C and C causes Y. A good example in our case could be occupation; your education has a big impact on your occupation, which in turn has a big impact on your income. You don't want to control for a mediator if you are interested in the full effect of X on Y! Because a huge part of how X impacts on Y is precisely through the mediation of C, in our case choice of income. If you 'control' for occupation you will be greatly underestimating the importance of education.

Incidentally this last point is behind one of the great perpetual culture war debates, whether to control for occupation and experience when estimating the gender pay gap. Much of the gender pay gap goes away when we do so, so does this mean there is no problem here? No, because occupation and experience are mediator variables which are precisely the way gender leads to a pay gap. By controlling for them you are missing out on the actual mechanism of precisely in what you are interested.

However there is a time when you might be interested in controlling for occupation and experience in a regression of pay gap on gender. This is when you are seeking to measure the direct impact, and only the direct impact, of gender in pay decisions - most likely in the context of gender discrimination at the final stage of remuneration determination. In this case, occupation and experience are no longer mediators, they are not even confounders, because X is no longer "gender" but "gender discrimination in the current workplace making a pay decision" and C is correlated with Y but not with X. So if you want to estimate that final "equal pay for equal work" step of the chain then yes it is legitimate to control for occupation and experience. Just don't mix this up with the total impact of gender on the pay gap, which *is* mediated through occupation and experience.




*Post continues after R code*
{% highlight R lineanchors %}





{% endhighlight %}






*Post continues after R code*
{% highlight R lineanchors %}

{% endhighlight %}


*Post continues after R code*
{% highlight R lineanchors %}




{% endhighlight %}


<img src='/img/0247-original-diagram.jpeg' width=80%>


