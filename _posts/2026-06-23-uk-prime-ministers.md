---
layout: post
title: United Kingdom prime ministers
date: 2026-06-23
tag: 
   - Timeseries
description: UK has had a spurt of prime ministerial turnover in the past decade or so, but it's by no means unprecedented. I download data from Wikipedia and try several ways to visualise that turnover.
image: /img/0323-gantt.svg
socialimage: https:/freerangestats.info/img/0323-gantt.png
category: R
---
With the recent resignation announcement from United Kingdom (UK) Prime Minister Keir Starmer, there have been a flurry of people talking about how many UK prime ministers there have been in the past decade, short terms for prime ministers, and so on. I wanted a historical perspective and so grabbed the data from [Wikipedia](https://en.wikipedia.org/wiki/List_of_prime_ministers_of_the_United_Kingdom). Wikipedia has a convenient single table list of all of the UK prime ministers since the term began being used informally by Robert Walpole. Walpole was effectively prime minister of the Kingdom of Great Britain from 1721 onwards. 

The first prime minister of the *United* Kingdom of Great Britain *and Ireland* was William Pitt in 1801; and of United Kingdom of Great Britain and *Northern* Ireland was Andrew Bonar Law in 1922. But these distinctions will be largely disregarded for the purpose of this blog post.

## Downloading prime ministerial data from Wikipedia

Here's code to download and import that list from Wikipedia. This worked as at 23 June 2026, but Wikipedia pages are known to change in format so it's brittle about whether it will work forever:

{% highlight R lineanchors %}
library(rvest)
library(tidyverse)
library(janitor)
library(slider) # for rolling sum
library(scales)
library(ggrepel)
library(kableExtra)

#-----------------Import and process data------------------------

url <- "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_the_United_Kingdom"

page <- read_html(url)

# The main PM table is the first wikitable on the page
pm_table <- page |>
  html_element("table.wikitable") |>
  html_table(fill = TRUE) |> 
  clean_names() |> 
  select(
    pm = prime_minister_office_lifespan,
    start = term_of_office,
    end = term_of_office_2
  ) |> 
  # drop second line of column titles:
  slice(-1) |> 
  # find the PMs' names - everything up to the first [
  mutate(pm = str_extract(pm , ".*?\\["),
         pm = str_replace(pm, "\\[", ""),
        ) |> 
  # strip all the footnotes and stuff from the dates:
  mutate(across(everything(), ~ str_remove_all(.x, "\\[.*?\\]"))) |>  # remove [1] refs
  mutate(across(everything(), str_squish)) |> 
  mutate(start = as.Date(start, format = "%d %B %Y"),
         end = as.Date(end, format = "%d %B %Y"),
         end = if_else(is.na(end) & pm == "Keir Starmer",
                       as.Date("2026-07-10"),
                       end),
        duration = as.numeric(end - start),) |> 
  distinct() |> 
  mutate(pm = fct_reorder(pm, start, .desc = TRUE)) |> 
  group_by(pm) |> 
  mutate(last_end = max(end)) |> 
  ungroup()
{% endhighlight %}

## Most and longest serving prime ministers

This lets us do some simple analysis. First, here are the UK prime ministers who have served the most often&mdash;that is, had more than one term:

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Prime minister </th>
   <th style="text-align:right;"> Terms </th>
   <th style="text-align:left;"> Earliest start </th>
   <th style="text-align:left;"> Latest finish </th>
   <th style="text-align:right;"> Total duration </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> William Ewart Gladstone </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> 1868-12-03 </td>
   <td style="text-align:left;"> 1894-03-02 </td>
   <td style="text-align:right;"> 4508 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Edward Smith-Stanley </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1852-02-23 </td>
   <td style="text-align:left;"> 1868-02-25 </td>
   <td style="text-align:right;"> 1381 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Robert Gascoyne-Cecil </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1885-06-23 </td>
   <td style="text-align:left;"> 1902-07-11 </td>
   <td style="text-align:right;"> 5000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stanley Baldwin </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1923-05-22 </td>
   <td style="text-align:left;"> 1937-05-28 </td>
   <td style="text-align:right;"> 2639 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Thomas Pelham-Holles </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1754-03-16 </td>
   <td style="text-align:left;"> 1762-05-26 </td>
   <td style="text-align:right;"> 2763 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Charles Watson-Wentworth </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1765-07-13 </td>
   <td style="text-align:left;"> 1782-07-01 </td>
   <td style="text-align:right;"> 478 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> William Cavendish-Bentinck </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1783-04-02 </td>
   <td style="text-align:left;"> 1809-10-04 </td>
   <td style="text-align:right;"> 1178 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> William Pitt the Younger </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1783-12-19 </td>
   <td style="text-align:left;"> 1806-01-23 </td>
   <td style="text-align:right;"> 6917 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arthur Wellesley </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1828-01-22 </td>
   <td style="text-align:left;"> 1834-12-09 </td>
   <td style="text-align:right;"> 1051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> William Lamb </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1834-07-16 </td>
   <td style="text-align:left;"> 1841-08-30 </td>
   <td style="text-align:right;"> 2447 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Robert Peel </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1834-12-10 </td>
   <td style="text-align:left;"> 1846-06-29 </td>
   <td style="text-align:right;"> 1883 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Henry John Temple </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1855-02-06 </td>
   <td style="text-align:left;"> 1865-10-18 </td>
   <td style="text-align:right;"> 3429 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Benjamin Disraeli </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1868-02-27 </td>
   <td style="text-align:left;"> 1880-04-21 </td>
   <td style="text-align:right;"> 2530 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ramsay MacDonald </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1924-01-22 </td>
   <td style="text-align:left;"> 1935-06-07 </td>
   <td style="text-align:right;"> 2480 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Winston Churchill </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1940-05-10 </td>
   <td style="text-align:left;"> 1955-04-05 </td>
   <td style="text-align:right;"> 3160 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Harold Wilson </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1964-10-16 </td>
   <td style="text-align:left;"> 1976-04-05 </td>
   <td style="text-align:right;"> 2835 </td>
  </tr>
</tbody>
</table>

Since the mid twentieth century, only Churchill and Wilson have had a second chance to be prime minister. In the nineteenth century it was much more common, with big names like Gladstone, Disraeli and Gascoyne-Cecil dominating politics while in government and out.

Here are those who have served the longest durations in total:

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Prime minister </th>
   <th style="text-align:right;"> Terms </th>
   <th style="text-align:left;"> Earliest start </th>
   <th style="text-align:left;"> Latest finish </th>
   <th style="text-align:right;"> Total duration </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Robert Walpole </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1721-04-03 </td>
   <td style="text-align:left;"> 1742-02-11 </td>
   <td style="text-align:right;"> 7619 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> William Pitt the Younger </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1783-12-19 </td>
   <td style="text-align:left;"> 1806-01-23 </td>
   <td style="text-align:right;"> 6917 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Robert Jenkinson </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1812-06-08 </td>
   <td style="text-align:left;"> 1827-04-09 </td>
   <td style="text-align:right;"> 5418 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Robert Gascoyne-Cecil </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1885-06-23 </td>
   <td style="text-align:left;"> 1902-07-11 </td>
   <td style="text-align:right;"> 5000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> William Ewart Gladstone </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> 1868-12-03 </td>
   <td style="text-align:left;"> 1894-03-02 </td>
   <td style="text-align:right;"> 4508 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Frederick North </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1770-01-28 </td>
   <td style="text-align:left;"> 1782-03-27 </td>
   <td style="text-align:right;"> 4441 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Margaret Thatcher </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1979-05-04 </td>
   <td style="text-align:left;"> 1990-11-28 </td>
   <td style="text-align:right;"> 4226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Henry Pelham </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1743-08-27 </td>
   <td style="text-align:left;"> 1754-03-06 </td>
   <td style="text-align:right;"> 3844 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tony Blair </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 1997-05-02 </td>
   <td style="text-align:left;"> 2007-06-27 </td>
   <td style="text-align:right;"> 3708 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Henry John Temple </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> 1855-02-06 </td>
   <td style="text-align:left;"> 1865-10-18 </td>
   <td style="text-align:right;"> 3429 </td>
  </tr>
</tbody>
</table>


The first UK prime minister, Robert Walpole, was also the longest serving. From the 20th and 21st century, only Thatcher and Blair make the top ten list.

Those two simple tables were produced with this code:

{% highlight R lineanchors %}
#------------summary highlights----------

# prime ministers number of terms and total duration:
pm_summary <- pm_table |> 
  rename(`Prime minister` = pm) |> 
  group_by(`Prime minister`) |> 
  summarise(Terms = length(`Prime minister`),
            `Earliest start` = min(start),
            `Latest finish` = max(end),
            `Total duration` = sum(duration)) |> 
  arrange(desc(Terms), `Earliest start`) 

# Prime ministers with more than one term:
pm_summary |> 
  filter(Terms > 1) |> 
  kable() |> 
  kable_styling() 

# Longest serving prime ministers:
pm_summary |> 
  arrange(desc(`Total duration`)) |> 
  slice(1:10) |> 
  kable() |> 
  kable_styling() 
{% endhighlight %}

## Graphic summaries

Tables are nice but graphics are better. Here is my attempt to summarise all the prime ministers of the UK (and of the predecessor Kingdom of Great Britain) in one picture. You probably need a full-sized screen for this, but with the right display I think the Gantt chartish style works nicely.

<object type="image/svg+xml" data='/img/0323-gantt.svg' width='100%'><img src='/img/0323-gantt.png' width='100%'></object>

That chart produced with this code. There are a few clutter-minimisation polishing details here on top of my usual blog style, like suppressing the y axis labels and adding them instead as text close to the data. Much easier to read. And suppressing the horizontal gridlines.

{% highlight R lineanchors %}
--------------Draw plots-------------
the_title <- "Prime ministers of the United Kingdom and its predecessors, 1721 to 2026"

pm_table |> 
  ggplot(aes(y = pm, yend = pm)) +
  geom_segment(aes(x = start, xend = end),
               linewidth = 2, colour = "steelblue") +
  geom_text(data = distinct(pm_table, pm, last_end),
            aes(label = pm, x = last_end + 500),
            size = 2, hjust = 0, colour = "grey50") +
  scale_x_date(
    breaks = seq(as.Date("1720-01-01"), as.Date("2035-01-01"), by = "20 years"),
    date_labels = "%Y",
    sec.axis = sec_axis(~.),
  ) + 
  labs(x = "Year",
       y = "",
       title = the_title) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y  = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())
{% endhighlight %}

Secondly, it seems highly relevant to produce a plot of the distribution of durations:
<object type="image/svg+xml" data='/img/0323-density.svg' width='100%'><img src='/img/0323-density.png' width='100%'></object>

And one showing the trend (or lack of trend) in durations over time:

<object type="image/svg+xml" data='/img/0323-over-time.svg' width='100%'><img src='/img/0323-over-time.png' width='100%'></object>

Those two simple plots produced with this code. Perhaps the only point of particular interest here is how I used a subset of the data to highlight the names of prime ministers with term duration of less than 120 days or more than 3,000:

{% highlight R lineanchors %}
pm_table |> 
  ggplot(aes(x = duration)) +
  geom_density(colour = "steelblue") +
  geom_rug(colour = "steelblue") +
  scale_x_continuous(label = comma) +
  labs(x = "Duration in days",
       title = the_title)

pm_table |> 
  ggplot(aes(x = start, y = duration)) +
  geom_smooth(method = "gam", colour = "white") +
  geom_point(colour = "steelblue") +
  geom_text_repel(data = filter(pm_table, duration < 120 | duration > 3000), 
                   aes(label = pm), size = 2.8, seed = 123) +
  scale_y_sqrt(breaks = c(0.5, 1, 1:4 * 2) * 1000, label = comma) +
  labs(x = "Starting date of premiership",
       y = "Duration in days",
       title = the_title,
      subtitle = "Durations shown are of individual periods in office, not lifetime totals.")
{% endhighlight %}

Finally, the big question that seems to get a lot of attention. How many prime ministers per decade? Below is my effort at calculating and presenting this.

<object type="image/svg+xml" data='/img/0323-rolling-pms.svg' width='100%'><img src='/img/0323-rolling-pms.png' width='100%'></object>

We can see that we are indeed going through a decade that is rich in UK prime ministers (and will be richer still in a month or so). But it's not unprecedented. We've been at similar levels seeral times in the past, and in the politically turbulent 1830s there were even more premierships.

In fact, in the late twentieth century with Thatcher and Blair, the UK faced a period of unusually slow turnover of prime ministers. But that was a formative period in the life of many of today's political commentators, so its not surprising that the current rapid turnover comes across as a surprise.

Code for this is below. Note that I calculated this on a daily basis. I'm not 100% I've got it right, but it passes my simplest reality checks (eg manually counting those we've had in the past ten years - six so far, although expeted soon to become seven).

{% highlight R lineanchors %}
cumulative_pms <- pm_table |> 
  full_join(tibble(start = seq(from = min(pm_table$start), 
                               to = max(pm_table$end), 
                               by = "1 day"))) |> 
  arrange(start) |> 
  mutate(starting_pms = if_else(is.na(pm), 0 , 1),
         rolling_pms = slide_sum(starting_pms, before = 3653),
         # I'm not sure I've got this right yet, but the idea is that in any given day,
         # the number of PMs in thepast 10 years is however many started in those 10 years,
         # plus 1 PM that you came into the period with. The exception being the time
         # of the very first prime minister, for which time you only have the rolling sum
         # of PMs that started:
         rolling_pms = if_else(start < (pm_table[1, ]$start + 3654), 
                               rolling_pms, 
                               rolling_pms + 1))

# When was the peak number of PMs in the last decade:
arrange(cumulative_pms, desc(rolling_pms))


cumulative_pms |> 
  ggplot(aes(x = start, y = rolling_pms)) +
  geom_line(colour = "steelblue") +
  scale_y_continuous(breaks = 0:max(cumulative_pms$rolling_pms)) +
  labs(x = "",
       y = "Number of prime ministers in past 10 years",
       title = the_title,
       subtitle = "Peak prime ministers per decade was in the 1830s")
{% endhighlight %}

That's all for now.
