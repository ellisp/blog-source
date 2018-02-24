---
layout: post
title: Success rates of appeals to the Supreme Court by Circuit
date: 2017-02-26
tag: 
   - Reproducibility
   - R
description: It's important to use the correct denominator when considering performance.  While a high percentage (more than 50%) of decisions from the US appeal circuit courts that get all the way to the Supreme Court are overturned, this is only a tiny proportion of total appeals decided by the lower courts.
image: /img/0081-total-ratios.svg
socialimage: http://ellisp.github.io/img/0081-total-ratios.png
category: R
---

In the chaos of the last month or so of United States of America governance, one item that grabbed my attention was the claim by President Trump that 80% of appeals decided by the [Ninth Circuit Court of Appeal](http://www.ca9.uscourts.gov/information/) are overturned by the Supreme Court of the United States (SCOTUS):

> "In fact, we had to go quicker than we thought because of the bad decision we received from a circuit that has been overturned at a record number. I have heard 80 percent, I find that hard to believe, that is just a number I heard, that they are overturned 80 percent of the time. I think that circuit is -- that circuit is in chaos and that circuit is frankly in turmoil. But we are appealing that, and we are going further."

*(Donald Trump during his 77 minute [16 February 2017 press conference](http://edition.cnn.com/2017/02/16/politics/donald-trump-news-conference-transcript/))*

Put aside whether it is a good thing for the head of state in a press conference to be speculating on things that are "just a number I heard" and making casual judgments about whether a Federal Circuit Court of Appeal is in chaos (would the Queen do this for one of her realms' courts?  Probably not).  This particular number sounded bad, and having eighty percent of appeals overturned by the higher appeals court surely would be a sign of chaos.  The reality however is that this is eighty percent of the subset of cases from that appeals circuit that are *considered* by the Supreme Court, not eighty percent of the circuit's total cases.  The potential to mislead is nicely explained by the [Snopes fact-checking/legend-busting site](http://www.snopes.com/ninth-circuit-court-most-overturned/) about a widely cited blog post arguing the 9th Circuit has known liberal bias and quoting a Congress member describing it as "presumptively reversible":

> "So, although correctly worded, the blog post left many readers with the mistaken impression that 80 percent of the Ninth Circuit Court’s decisions were being overturned by SCOTUS. What it actually said was that of the very tiny fraction of decisions by federal courts of appeal that SCOTUS agrees to review each year (0.1%), 80 percent of that small portion of appeals originating with the Ninth Circuit Court were overturned."

*(Snopes)*

Any reasonable view of the performance of one of the Circuit appeals courts should use as its denominator the number of cases decided by the Circuit, not the number of decided cases that are then accepted for consideration by the Supreme Court.  As the Trump administration is I suspect currently discovering, just disagreeing with an appeals decision does not mean that there is any prospect of putting together a case worthy of consideration by the Supreme Court.

Most of the discussion on the web is based on analysis in Roy E. Hofer's 2010 paper [Supreme Court Reversal Rates: Evaluating the Federal Courts of Appeals](http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf), which is a perfectly respectable piece of analysis.  His results are presented in tables so to better understand them I tried a visual:

<img src='/img/0081-total-ratios.svg' width = '100%'>

I'm afraid it's pretty ugly; lots of information but too wordy.  Basically, in the period considered in Hofer's analysis, about one in a thousand of the appeals decided by the Ninth Circuit was successfully overturned; a far cry from "presumptively reversible".  True however that if you get as far as putting together a coherent appeal and the Supreme Court agreeing to consider it, you have a good chance of success at the final hurdle.

Sometimes more basic visualizations are better.   So here's a good old-fashioned stacked bar chart:

<img src='/img/0081-bars.svg' width = '100%'>

This makes the tiny proportion of cases considered and overturned by the Supreme Court visually obvious.  The graphic is scalable but you will have to zoom in a long way to see the slivers of green or pink that indicate a Supreme Court affirmation or overturn.

## The data

Here's how I went about exploring today's data.

After some hunting around I found that more up to date (and not particularly interesting for today's purposes) data are available from the Supreme Court's [own webpage](http://www.scotusblog.com/reference/stat-pack/).  More confusion is perpetuated by describing as a "Circuit Scorecard" a table that refers again to the percentage of cases that get to SCOTUS that are overturned, the same false view of the performance of Circuits that started this.  As the Supreme Courts' data although commendably open was not in a particularly convenient structure, I decided to confine myself for today's purposes to just examining Hofer's original results.

Hofer's paper is only available as a PDF, so the first task was to extract his tables into R.  This is easy with the [amazing `tabulizer` package](https://github.com/ropensci/tabulizer), part of the `ropensci` project.  The code snippet below downloads the file and extracts all of the tables in the document into a list called `tabs`.

{% highlight R %}
library(tidyverse)
library(scales)
library(stringr)
library(tabulizer)
library(forcats)
library(grid)

#-----------------download file and extract the tables---------------------
thefile <- tempfile()

download.file("http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf",
              destfile = thefile, mode = "wb")

tabs <- extract_tables(thefile)
{% endhighlight %}

The first table in the paper was of the total number of cases decided by each Circuit by year.  This is of some interest in itself and I produced this chart while familiarising myself with the data

<img src='/img/0081-cases-time.svg' width = '100%'>

Here's the code that grooms that data - which effectively forms the correct denominator to use in considering the degree to which appeals end up being overturnable - and produces the chart:

{% highlight R %}
#----------------------prepare data---------------
total_cases <- as.data.frame(tabs[[1]][ -(1:2), ])
names(total_cases) <- c("Court", 1999:2008, "Total")

total_cases <- total_cases %>%
   gather(Year, Cases, -Court) %>%
   mutate(Cases = as.numeric(gsub(",", "", Cases)),
          Court = str_trim(Court))

total_cases %>%
   filter(Year != "Total") %>%
   mutate(Year = as.numeric(Year)) %>%
   ggplot(aes(x = Year, y = Cases)) +
   geom_point() +
   geom_line() +
   facet_wrap(~Court, scales = "free_y", ncol = 3) +
   scale_y_continuous("Total appeals terminated\n", label = comma)
{% endhighlight %}

Next task was to extract the third table of the paper, which has the numbers of cases taken to the Supreme Court and what happened to them.  This data comes out of the `tabulizer` process a little messier and needed a bit of hand treatment.

Once stored in object called `scotus`, I combined this with the total cases denominator and drew the two graphs I presented in this post earlier:
{% highlight R %}
scotus <- as.data.frame(tabs[[3]][-(1:3), 1:5])
scotus <- cbind(scotus[ , -5], str_split(scotus[ , 5], " ", simplify = TRUE)[ , 1:2])
names(scotus) <- c("Court", "Reversed", "Vacated", "Affirmed", "Reversed + Vacated", "Appealed")
scotus <- scotus %>%
   mutate(Court = str_trim(Court)) %>%
   gather(Result, Number, - Court) %>%
   mutate(Number = as.numeric(as.character(Number))) %>%
   spread(Result, Number)

combined <- total_cases %>%
   filter(Year == "Total" & Court != "Annual Totals") %>%
   select(Court, Cases) %>%
   left_join(scotus, by = "Court") %>%
   mutate(rv_prop_cases = `Reversed + Vacated` / Cases,
          rv_prop_appealed = `Reversed + Vacated` / Appealed) %>%
   arrange(desc(rv_prop_cases)) %>%
   mutate(Court = factor(Court, levels = Court))
   
combined %>%
   select(Court, Cases, rv_prop_cases, rv_prop_appealed, Appealed) %>%
   gather(Denominator, Proportion, -Court, -Cases, -Appealed) %>%
   mutate(Denominator = ifelse(grepl("appealed", Denominator), "Percentage of total appeals", "Percentage of total cases"),
          Denominator_value = ifelse(grepl("total cases", Denominator), Cases, Appealed)) %>%
   ggplot(aes(x = Proportion, y = Court, size = Cases, label = Denominator_value)) +
   # force x axis to go to zero by drawing an invisible line:
   geom_vline(xintercept = 0, alpha = 0) +
   facet_wrap(~Denominator, scales = "free_x") +
   geom_text(colour = "steelblue") +
   scale_x_continuous(label = percent) +
   scale_size_area("Total original cases:", label = comma) +
   theme(legend.position = "none") +
   ggtitle("Federal courts of appeals cases reversed or vacated by the Supreme Court 1999 - 2008", 
           "From the 9th Circuit, 80% of 175 cases that went to SCOTUS were overturned,
but that was only 0.12% of the 114,199 total cases originally decided by that circuit in the period.") +
   labs(caption = "Data from http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf
        Analysis at http://ellisp.github.io",
        y = "Appeals circuit\n\n",
        x = "Horizontal position indicates the proportion of cases overturned by the Supreme Court.
The printed number is the denominator - total cases that could have been overturned at that stage.
The size is proportionate to the total cases decided by each circuit.") 

grid.text(0.75, 0.26, 
          label = "But only a tiny percentage of the number\nof cases originally decided by the circuit judges.",
          gp = gpar(cex = 0.75, family = "myfont", col = "grey50"))

grid.text(0.35, 0.3, 
          label = "A high proportion of the small number of\ncases that are taken to the Supreme Court\ndo end up reversed or vacated.",
          gp = gpar(cex = 0.75, family = "myfont", col = "grey50"))
		  
#--------------barchart version------------
combined %>%
   mutate(`Not considered by SCOTUS` = Cases - Appealed) %>%
   select(Court, `Not considered by SCOTUS`, Affirmed, `Reversed + Vacated`) %>%
   gather(Result, Number, -Court) %>%
   mutate(Result = ifelse(Result == "Reversed + Vacated", "Overturned", Result),
          Result = factor(Result, levels = c("Overturned", "Affirmed", "Not considered by SCOTUS"))) %>%
   ggplot(aes(x = Court, weight = Number, fill = Result)) +
   geom_bar(position = "stack") +
   coord_flip() +
   scale_y_continuous(label = comma) +
   scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
   ggtitle("Federal courts of appeals cases reversed or vacated by the Supreme Court 1999 - 2008") +
   labs(caption = "Data from http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf
        Analysis at http://ellisp.github.io",
        x = "Appeals circuit", y = "Number of appeal cases")		  
{% endhighlight %}

By the way, you might have asked (as I did) what do "vacated" and "reversed" mean?  As far as I can tell, they have a similar effect in overturning the original decision.  According to [Josh Blackman](http://joshblackman.com/blog/2016/03/28/scotus-style-manual-on-difference-between-reverse-and-vacate/),

> "Reverse is when things are really, really wrong. Vacate is when it is somewhat wrong."

## Inferential analysis

I was interested in whether the different proportions overturned of Circuits' total cases were statistically significant evidence of different rates.  I expected that they would be; not so much because of differing Circuit culture and competence (although these would be expected to vary at least somewhat), as that different types of cases go to different Circuits.  I would summarise the conclusion as significant evidence of a difference (p value from the ANOVA test below is basically zero, thanks to the large sample size of cases), but not a particularly material difference.

I fitted a generalized linear model with a binomial response (also known as logistic regression) to test this.  Here are the confidence intervals for the effect on total overturnability (to coin a phrase) of the different Circuits, relative to the Eighth Circuit which I chose as a reference point because of its near-average performance.

<img src='/img/0081-model-results.svg' width = '100%'>

And here's the code for the model fitting and presentation:

{% highlight R %}
model_data <- combined %>%
   mutate(Court = relevel(factor(Court), ref = "Eighth Circuit"))

#----------------proportion of original------------
model1 <- glm(rv_prop_cases ~ Court, family = "binomial", weights = Cases, data = model_data)
anova(model1, test = "Chi")
res <- confint(model1)[-1, ]

res_df <- as.data.frame(res) %>%
   mutate(Court = gsub("Court", "", rownames(res)),
          center = (`2.5 %` + `97.5 %`) / 2) %>%
   mutate(Court = fct_reorder(Court, center))

ggplot(res_df, aes(y = Court, yend = Court, x = `2.5 %`, xend = `97.5 %`)) +
   geom_segment() +
   geom_point(aes(x = center)) +
   labs(x = "95% confidence interval for increase in logarithm of odds of a completed case 
being appealed successfully by Supreme Court, relative to the Eighth Circuit",
         y = "",
        caption = "Data from http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf
        Analysis at http://ellisp.github.io")
{% endhighlight %}


{% highlight R %}

{% endhighlight %}
