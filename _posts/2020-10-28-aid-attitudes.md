---
layout: post
title: Reproduce analysis of a political attitudes experiment
date: 2020-10-28
tag: 
   - NewZealand
   - Australia
   - ModellingStrategy
   - Reproducibility
description: I reproduce the analysis of data from a recently published experiment on the impact on Australians' and New Zealanders' attitudes to overseas aid of being exposed to writing about Chinese aid in the Pacific. Along the way I muse about the Table 2 fallacy, and try to avoid it while still using multiple imputation, bootstrap and adjusting for covariates to slightly improve the original analysis.
image: /img/0198-support-aus.svg
socialimage: http://freerangestats.info/img/0198-support-aus.png
category: R
---

Terence Wood, Chris Hoy and Jonathan Pryke recently published this paper on
[The Effect of Geostrategic Competition on Public Attitudes to Aid](https://www.cambridge.org/core/journals/journal-of-experimental-political-science/article/effect-of-geostrategic-competition-on-public-attitudes-to-aid/A3761D2D3A9D4574DACA4D8530BE7C0D). I was interested a) because of my past interest in public attitudes to aid (my very first real, full time permanent job was a a community campaigns coordinator for Community Aid Abroad, which later rebranded as Oxfam Australia) b) because Terence and I used to share a cubicle in NZAID / DFAT and c) because I am particularly interested in experiments at the moment, and this was a nice example of an experiment in a non-medical area with reproducible data and code.

The experiment was to compare the attitudes to aid of members of the Australian and New Zealand public, with and without the "treatment" of being exposed to written vignettes of varying degrees of forcefulness about China's rise as an aid donor in the Pacific. Data collection was by questions included in Ipsos MORI online omnibus surveys.

From the Australian findings:

> "As expected, treating participants reduced hostility to aid and increased support for more aid focused on the Pacific. Counter to expectations, however, treatment reduced support for using aid to advance Australian interests."

The result in the first sentence, but not the second was replicated with the New Zealand subjects (with questions appropriately reworded for New Zealand, rather than Australia, of course).

The finding about treatment reducing support for using aid to advance Australian interests was a surprise because the researchers had expected, based on other literature to date, that being exposed to China's aid activities in the Pacific (which are fairly transparently undertaken with a heavy dose of geo-strategic competitive motives) would make Australians **more** inclined to use our own aid program the same way.

## Reproducing results

Terence and friends have made the [data and Stata code for the analysis available on the Harvard Dataverse](https://t.co/svo2zDhm3B?amp=1). My first move was to check that I could reproduce their results.  Here's the original table of results from the Australian analysis:

<img src = '/img/0198-original-tab1.png' width='100%'>

### Regression results

That table combines the regression coefficients in the odd-numbered columns with contrasts between the 'Measured' and 'Forceful' versions of the vignette in the even-numbered columns. I found this slightly untidy so will reproduce these in two different steps. Here's my reproduction of columns (1), (3) and (5) from that first table:

#### Table 1a - Regression results, experiment with three response variables, Australian public

<table style="text-align:center"><caption><strong>Regression results for Australians</strong></caption>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Too much aid</td><td>More to Pacific</td><td>Help Australia</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Measured vignette</td><td>-0.079<sup>***</sup></td><td>0.052<sup>*</sup></td><td>-0.063<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.029)</td><td>(0.028)</td><td>(0.028)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Forceful vignette</td><td>-0.093<sup>***</sup></td><td>0.089<sup>***</sup></td><td>-0.097<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.029)</td><td>(0.028)</td><td>(0.028)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Control group mean</td><td>0.518<sup>***</sup></td><td>0.257<sup>***</sup></td><td>0.598<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.020)</td><td>(0.020)</td><td>(0.020)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,816</td><td>1,647</td><td>1,844</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.007</td><td>0.006</td><td>0.007</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.006</td><td>0.005</td><td>0.005</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.497 (df = 1813)</td><td>0.459 (df = 1644)</td><td>0.497 (df = 1841)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>6.077<sup>***</sup> (df = 2; 1813)</td><td>5.150<sup>***</sup> (df = 2; 1644)</td><td>6.028<sup>***</sup> (df = 2; 1841)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Good, I seem to have exactly the same results. Here's my R code that does the analysis so far for both the Australian and New Zealand results.

*Post continues below R code*
{% highlight R lineanchors %}
# Wood, Terence, 2020, "Replication Data for The effect of geostrategic
# competition on public attitudes to aid", https://doi.org/10.7910/DVN/3VVWPL,
# Harvard Dataverse, V1, UNF:6:a3zSXQF/lkQQhkGYNqchGg== [fileUNF]

library(tidyverse)
library(haven)
library(glue)
library(stargazer)
library(kableExtra)
library(emmeans)
library(patchwork)
library(boot)
library(mice)
library(ggdag)
library(clipr)

the_caption <- "Reproduction data from Terence Wood et al, 'The Effect of Geostrategic Competition on Public Attitudes to Aid'"

# Data needed downloaded manually from Harvard Dataverse
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3VVWPL
aust <- read_stata("1 Australia data JEPS FINAL.dta") %>%
  mutate(treatment_group = as_factor(treatment_group))

nz <- read_stata("2 NZ data JEPS FINAL.dta") %>%
  mutate(treatment_group = as_factor(treatment)) %>%
  rename(too_much_aid = toomuchaid,
         more_to_pac = morepac,
         favour_nz = favournz)


# nz gets this error, but it is only a problem for the print method
# Error in gsub(finish, start, ..., fixed = TRUE) : 
#   input string 3 is invalid UTF-8

#-------------Australian models--------------------

aust_mods <- list()
all_response_vars <- c("too_much_aid", "more_to_pac", "favour_aus")
all_response_labs <- c("Too much aid", "More to Pacific", "Help Australia")

for(i in 1:length(all_response_vars)){
  form <- as.formula(glue("{all_response_vars[[i]]} ~ treatment_group"))
  
  aust_mods[[i]] <- lm(form, data = aust)
  
}

# Regression results:
stargazer(aust_mods[[1]], aust_mods[[2]], aust_mods[[3]], 
          type = "html",
          dep.var.labels = all_response_labs,
          title = "Regression results for Australians") %>%
  write_clip()


#---------------------New Zealand models------------------
nz_mods <- list()
all_response_vars_nz <- c("too_much_aid", "more_to_pac", "favour_nz")
all_response_labs_nz <- c("Too much aid", "More to Pacific", "Help New Zealand")

for(i in 1:length(all_response_vars_nz)){
  form <- as.formula(glue("{all_response_vars_nz[[i]]} ~ treatment_group"))
  
  nz_mods[[i]] <- lm(form, data = nz)
  
}

# Regression results:
stargazer(nz_mods[[1]], nz_mods[[2]], nz_mods[[3]], 
          type = "html",
          dep.var.labels = all_response_labs_nz,
          title = "Regression results for New Zealanders")
{% endhighlight %}

... and here are the results for the New Zealand side of the experiment. Again, these match the original article. Note that the New Zealand experiment only had one version of the vignette on the Chinese aid program (unlike the Australian which had 'forceful' and 'measured' versions).

#### Table 1a - Regression results, experiment with three response variables, New Zealand public

<table style="text-align:center"><caption><strong>Regression results for New Zealanders</strong></caption>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Too much aid</td><td>More to Pacific</td><td>Help New Zealand</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Vignette</td><td>-0.077<sup>***</sup></td><td>0.070<sup>**</sup></td><td>-0.027</td></tr>
<tr><td style="text-align:left"></td><td>(0.028)</td><td>(0.030)</td><td>(0.031)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Control group mean</td><td>0.350<sup>***</sup></td><td>0.310<sup>***</sup></td><td>0.531<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.020)</td><td>(0.021)</td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,070</td><td>998</td><td>1,070</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.007</td><td>0.005</td><td>0.001</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.006</td><td>0.004</td><td>-0.0002</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.463 (df = 1068)</td><td>0.474 (df = 996)</td><td>0.500 (df = 1068)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>7.341<sup>***</sup> (df = 1; 1068)</td><td>5.422<sup>**</sup> (df = 1; 996)</td><td>0.798 (df = 1; 1068)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

### Contrasts

Tables (2), (4) and (6) of the original Australian table where the contrasts between the 'measured' and 'forceful' versions of the vignette on Chinese aid activity. There's no significant evidence that the two versions of the vignette have different effects, but it's important to check I can get the same results. Here's my estimate of those contrasts, calculated with the help of the [`emmeans` R package by Russell Lenth et al](https://cran.r-project.org/package=emmeans)


#### Table 2 - Contrast between effect of two vignettes - 'measured' and 'forceful' - on three different response variables

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> response </th>
   <th style="text-align:right;"> 'Measured' minus 'Forceful' </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> t.ratio </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Too much aid </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 1813 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> More to Pacific </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 1644 </td>
   <td style="text-align:right;"> -1.33 </td>
   <td style="text-align:right;"> 0.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Help Australia </td>
   <td style="text-align:right;"> -0.03 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 1841 </td>
   <td style="text-align:right;"> 1.19 </td>
   <td style="text-align:right;"> 0.24 </td>
  </tr>
</tbody>
</table>

`emmeans` is a nice package for comparing different levels of a factor in a linear model. It helps create plots of the comparisons too, looking after the tedious calculations of correconfidence intervals for you. This next chart, for just one of the response variables, is similar to Figure 1 from the original article. It shows the average proportion of the sample favouring the national interest as a motivation for aid for the different types of treatment. The red arrows inside the bars are an interpretive aid to help make confidence intervals for comparisons between two different effects, whereas the blue bars are the confidence intervals for a single level of the effect

<object type="image/svg+xml" data='/img/0198-margins.svg' width='100%'><img src='/img/0198-margins.png' width='100%'></object>



{% highlight R lineanchors %}
#----------Australian Contrasts--------------
rbind(
  as_tibble(pairs(emmeans(aust_mods[[1]], "treatment_group"))[3, ]),
  as_tibble(pairs(emmeans(aust_mods[[2]], "treatment_group"))[3, ]),
            as_tibble(pairs(emmeans(aust_mods[[3]], "treatment_group"))[3, ])
) %>%
  # pairs actually gives us the Measured estimate relative to Forceful; we want
  # the reverse:
  mutate(estimate = -estimate) %>%
  # shuffle some stuff for presentation:
  select(-contrast) %>%
  rename(`'Measured' minus 'Forceful'` = estimate) %>%
  mutate(response = all_response_labs) %>%
  select(response, everything()) %>%
  kable(digits = 2) %>%
  kable_styling()


#-------------margins plot, Figure 1:---------------
p3a <- plot(emmeans(lm(favour_aus ~ treatment_group, data = aust), specs = "treatment_group"),
            comparisons = TRUE) +
  labs(x = "Aid should help Australia",
       y = "")
p3b <- plot(emmeans(lm(favour_poor ~ treatment_group, data = aust), specs = "treatment_group"),
            comparisons = TRUE) +
  labs(x = "Aid should help the poor",
       y= "")

p3a + 
    p3b +
    plot_annotation(title = "Surprising impact on Australians of considering overseas aid in context of Australian aid",
                subtitle = "Subjects were given a description of Chinese aid that was either forceful, measured or none (control)",
                caption = the_caption)
{% endhighlight %}


{% highlight R lineanchors %}
{% endhighlight %}


{% highlight R lineanchors %}
{% endhighlight %}



{% highlight R lineanchors %}
{% endhighlight %}



## More complex models

The [code for the online appendix is available with the rest of the replication data](https://globaldataversecommunityconsortium.github.io/dataverse-previewers/previewers/TextPreview.html?fileid=4002954&siteUrl=https://dataverse.harvard.edu&datasetid=4002952&datasetversion=1.0&locale=en).

<object type="image/svg+xml" data='/img/0198-graph.svg' width='100%'><img src='/img/0198-graph.png' width='100%'></object>



<object type="image/svg+xml" data='/img/0198-bs-too-much.svg' width='100%'><img src='/img/0198-bs-too-much.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0198-more-pacific.svg' width='100%'><img src='/img/0198-more-pacific.png' width='100%'></object>


<object type="image/svg+xml" data='/img/0198-support-aus.svg' width='100%'><img src='/img/0198-support-aus.png' width='100%'></object>
