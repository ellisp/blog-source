---
layout: post
title: Free text in surveys - important issues in the 2017 New Zealand Election Study
date: 2020-09-26
tag: 
   - VotingBehaviour
   - Surveys
   - Text
   - NewZealand
   - Distributions
description: I try out biterm topic modelling on a free text question in the 2017 New Zealand Election Study about the most important issue in the election.
image: /img/0193-pv-issues.svg
socialimage: http://freerangestats.info/img/0193-pv-issues.png
category: R
---

This is a quick post looking at using biterm topic modelling, a new technique for me, on the free text responses to a survey question. I'm interested in whether this type of topic modelling might be a shortcut to analysing free text, quicker than having a human read the answers and code them. The question I'm using to try this out is from the [New Zealand Election Study](http://www.nzes.org/) which collected data just after the 2017 New Zealand election. The actual question is "What was the single most important issue for you in the election?".

## Human-coded version

This question had been classified/coded by hand by the original researchers. Here are the percentages of voters categorised by the issue they mentioned in that free text:

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Single most important issue in the New Zealand election (2017) </th>
   <th style="text-align:right;"> Percentage naming most important: </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> None Named </td>
   <td style="text-align:right;"> 20.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Economy </td>
   <td style="text-align:right;"> 14.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Housing </td>
   <td style="text-align:right;"> 11.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Health </td>
   <td style="text-align:right;"> 7.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inequality </td>
   <td style="text-align:right;"> 6.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tax </td>
   <td style="text-align:right;"> 6.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Environment </td>
   <td style="text-align:right;"> 5.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Immigration </td>
   <td style="text-align:right;"> 5.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Government </td>
   <td style="text-align:right;"> 4.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poverty </td>
   <td style="text-align:right;"> 4.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Education </td>
   <td style="text-align:right;"> 3.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Welfare </td>
   <td style="text-align:right;"> 3.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Party Bias </td>
   <td style="text-align:right;"> 2.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Change/continuation </td>
   <td style="text-align:right;"> 1.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Infrastructure </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Law and Order </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Moral </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kindness Common Sense </td>
   <td style="text-align:right;"> 0.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maori </td>
   <td style="text-align:right;"> 0.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rural Regional </td>
   <td style="text-align:right;"> 0.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Defence </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pike River </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
</tbody>
</table>


When we compare the issues they identified by the party vote of respondents, we see this complex but understandable relationship:

<object type="image/svg+xml" data='/img/0193-pv-issues.svg' width='100%'><img src='/img/0193-pv-issues.png' width='100%'></object>

The relationship between vote and issue of importance is pretty much as we'd expect. The environment is the top issue for Greens voters (followed by inequality); immigration for New Zealand First; housing for Labour and the Māori Party; and the economy for National voters.

To examine the actual words in the free text that was classified this way against issue, we can knock out the common stopwords and reduce words to their meaningful stems. This gives us this impression:

<object type="image/svg+xml" data='/img/0193-stems-issues.svg' width='100%'><img src='/img/0193-stems-issues.png' width='100%'></object>

Again, all seems straightforward. Words with the stem "hous" are strongly associated with the human-coded "Housing" issue (of course), "ineq" with Inequality, "poverti" with Poverty, and so on. So this makes sense in retrospect, but could I have created those topics from scratch with an unsupervised topic model?

## Biterm topic modelling

[Biterm topic modelling](https://www.researchgate.net/publication/262244963_A_biterm_topic_model_for_short_texts) is an approach to gleaning the latent topics discussed in a number of documents. It is explicitly designed to cope efficiently and effectively with short source documents, with a sparse document term matrix. It's available via a C++ library by Xiaohui Yan, wrapped in the `BTM` R package maintained by Jan Wijffels.

Is topic modelling appropriate, or should I be looking for a simpler classification? While the original survey question asked for a single issue of most importance, this is the world of surveys and respondents don't always do what they're asked, and they don't always agree with the researcher on what a single issue is. For example, many people gave responses like "housing and poverty". This can meaningfully be parsed as a single issue, but then how does it relate to others who think just "housing" is the issue, independently of poverty? Then there are responses such as ""No party had policy that would remedy major problems, police, education, health, councils". From the respondent's view point this is a single 'issue' (the parties' policies are no good!) but from the point of view of the researchers this is multiple subject-matter issues grouped under a single meta-issue. The original coders coped with this by classifying each response to up to 3 issues (`rimpissue`, `rimpissue2`, `rimpissue3`) and retaining the full original text in the `rmpissuex` column. In the charts earlier in this blog, I used the `rimpissue` column, the first issue identified by the coders.

Like other topic modelling approaches, biterm topic models allow each document to refer to multiple topics, but we can control this to a degree by tweaking the prior distribution of topics for each document. The prior distribution of the probability of each document discussing each of the k models is a Dirichlet distribution. I built myself a [Shiny app](https://ellisp.shinyapps.io/dirichlet/) to check that the alpha parameter to a Dirichlet distribution works the way I think it does. The `BTM()` function defaults to a value of alpha of 50/k. By changing this to a value of less than one we greatly decrease the parameter space where multiple topics are allowed, without reducing it to zero.

<object type="image/svg+xml" data='/img/0193-btm.svg' width='100%'><img src='/img/0193-btm.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0193-heatmap-btm.svg' width='100%'><img src='/img/0193-heatmap-btm.png' width='100%'></object>


{% highlight R lineanchors %}

{% endhighlight %}
