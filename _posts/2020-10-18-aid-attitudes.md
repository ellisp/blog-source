---
layout: post
title: Reproduce analysis of a political attitudes experiment
date: 2020-10-18
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


[The Effect of Geostrategic Competition on Public Attitudes to Aid](https://www.cambridge.org/core/journals/journal-of-experimental-political-science/article/effect-of-geostrategic-competition-on-public-attitudes-to-aid/A3761D2D3A9D4574DACA4D8530BE7C0D)

#### Table 1a - Regression results, no controls, experiment with three response variables, Australian public


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

#### Table 1a - Regression results, no controls, experiment with three response variables, New Zealand public

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



*Post continues below R code*
{% highlight R lineanchors %}
{% endhighlight %}


<object type="image/svg+xml" data='/img/0198-margins.svg' width='100%'><img src='/img/0198-margins.png' width='100%'></object>


## More complex models

The [code for the online appendix is available with the rest of the replication data](https://globaldataversecommunityconsortium.github.io/dataverse-previewers/previewers/TextPreview.html?fileid=4002954&siteUrl=https://dataverse.harvard.edu&datasetid=4002952&datasetversion=1.0&locale=en).

<object type="image/svg+xml" data='/img/0198-graph.svg' width='100%'><img src='/img/0198-graph.png' width='100%'></object>



<object type="image/svg+xml" data='/img/0198-bs-too-much.svg' width='100%'><img src='/img/0198-bs-too-much.png' width='100%'></object>

<object type="image/svg+xml" data='/img/0198-more-pacific.svg' width='100%'><img src='/img/0198-more-pacific.png' width='100%'></object>


<object type="image/svg+xml" data='/img/0198-support-aus.svg' width='100%'><img src='/img/0198-support-aus.png' width='100%'></object>
