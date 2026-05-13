---
layout: post
title: Durations of wars
date: 2026-05-13
tag: 
   - History
   - Distributions
description: Modern wars on average last longer than a year, but the distribution is very right-skewed. Conditional on a war lasting 74 days and with no information available other than past durations, the median total duration is 261 days, and an 80% prediction interval would be (95, 1,752) days.
image: /img/0321-cumulative-density.svg
socialimage: https:/freerangestats.info/img/0321-cumulative-density.png
category: R
---

How long do wars last, on average? If a war such as that currently under way in Iran has lasted 74 days so far, how long do we expect it to last in total? For all sorts of reasons, inquiring minds are interested. Luckily there are some very well curated datasets out there, including the [Correlates of War](https://correlatesofwar.org/data-sets/cow-war/), that make it easy to answer these questions.

A caveat to all this applies that I am not a military historian, just an interested amateur. I'm very open to having mistakes of interpretation or method pointed out to me.

## Distribution of wars' durations

The Correlates of War data lets us see, for example, that this is the distribution (on a logarithmic scale) of durations of wars post-Napoleon:

<object type="image/svg+xml" data='/img/0321-density.svg' width='100%'><img src='/img/0321-density.png' width='100%'></object>

You can see I've compared this to a log-normal distribution and found that it doesn't have quite as fat tails as that. But that's ok, I'm not too worried about the precise shape, because later on I'll be using pretty straightforward empirical methods.

This data is only for inter-state wars, which are in contrast to intra-state (eg civil wars) and extra-state (eg with external non-state actors). As I'm interested in a reference population to compare the current USA-Israel-Iran war to, it's the inter-state population I want.

The median length of a war is 139 days and the mean is 408 days. 

The four day war in the dataset is the so-called "[Football War](https://en.wikipedia.org/wiki/Football_War)" of 1969 between Honduras and El Salvador. The 3,734 day war was the much better-known "Vietnam War Phase II", involving USA, Australia, Vietnam, Cambodia and others.

Here's the code to import the data from the Correlates of War project and draw that first density plot:

{% highlight R lineanchors %}
library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(ggrepel)
library(scales)

# https://correlatesofwar.org/data-sets/cow-war/


#----- import interstate war data----------------------

interstate <- read_csv("https://correlatesofwar.org/wp-content/uploads/Inter-StateWarData_v4.0.csv") |> 
  clean_names() |> 
  mutate(start_date = as.Date(sprintf("%04d-%02d-%02d", start_year1, start_month1, start_day1)),
         end_date = as.Date(sprintf("%04d-%02d-%02d", end_year1, end_month1, end_day1)))

interstate_wars <- interstate |> 
  group_by(war_num, war_name) |> 
  summarise(earliest_start= min(start_date),
            latest_end = max(end_date),
            bat_death = sum(bat_death)) |> 
  mutate(duration = as.numeric(latest_end - earliest_start),
         start_year = year(earliest_start)) |> 
  ungroup()

# what years covered? 1823 to 2003 at time of writing
range(interstate_wars$start_year)

#==========================plots=================
 
simple_caption <- "Source: Correlates of War, Inter-State War Data; analysis by freerangestats.info"

#-----------------distribution of duration------------
summary(interstate_wars$duration)

sim_norm <- data.frame(duration = 10 ^ (rnorm(1e6, 
                                        mean = log10(interstate_wars$duration), 
                                        sd = sd(log10(interstate_wars$duration)))))

interstate_wars |> 
  ggplot(aes(x = duration)) +
  geom_density() +
  geom_rug() +
  geom_density(data = sim_norm, colour = "orange") +
  annotate("text", x= 1, y = 0.18, label = "Simulated log-normal distribution", 
           colour = "orange", hjust = 0) +
  annotate("text", x= 300, y = 0.51, label = "Empirical distribution of war durations", 
           colour = "black", hjust = 0) +
  # carefully chosen labels for x axis:
  scale_x_log10(label = comma, breaks = c(range(interstate_wars$duration), 10, 100, 1000)) +
  labs(x = "Duration of wars (in days, logarithmic scale)",
       y = "Density",
       title = "Distribution of war durations, 1823 to 2003",
       subtitle = "More concentrated, less-fat tails than a log-normal distribution",
       caption = simple_caption) +
  # use coord to limit x axis so statistical calculations are all done on full data:
  coord_cartesian(xlim = c(1, 8000))
{% endhighlight %}

OK, so my main analytical task here is to work out the conditional expected duration of a war that has reached 74 days - the length so far of the USA-Israel-Iran war. Yes, I know there's an incompletely observed ceasefire, but there's also a blockade (or two), and that's unambiguously an act of war under international law. So I'm counting the war as ongoing. 

My chart to answer this question is this one:

<object type="image/svg+xml" data='/img/0321-cumulative-distribution.svg' width='100%'><img src='/img/0321-cumulative-distribution.png' width='100%'></object>

What's happening here is:

* the empirical cumulative distribution function of durations is the dark line - basically the cumulative frequency on the vertical axis, but expressed as a proportion.
* the grey line is a simple LOESS smoother of that cumulative frequency, useful for modelling values that aren't exactly matched in the data.
* the red lines show the duration of the current war, and where it would fit in the distribution of 1823 to 2003 wars. It's about 0.33 (defined in the code below as the variable `current_cf`), meaning that the current war is already longer than about 33% of wars.
* the horizontal blue line is half way in the vertical space between the horizontal red line and 1. Where it meets the smoothed line and drops a vertical blue line shows the expected median duration of a war that has gotten to this 0.33 point on the cumulative frequency.

So we see that of wars that get as long as 74 days, we expect the median total length to be 261 days. That's a bit grim for those of us who think that even extending into June is going to be very bad indeed for the world economy, but it's good to know. Of course, there's plenty of wars that get to 74 days and then stop soon after, so there's hope there too.

Here's the code to do that bit of statistical inference and draw the chart:

{% highlight R lineanchors %}
#-------------------cumulative distribution--------------
interstate_cumulative <- interstate_wars |> 
  arrange(duration) |> 
  mutate(cumulative_freq = 1:n() / n()) 

# smoothed model of the cumulative distribution, including estimates of where
# the Iran war is on it:
model <- loess(cumulative_freq ~ log(duration), data = interstate_cumulative)
current_dur <- 74 # as at 13 May 2025 - war started 28 February 2026
current_cf <- predict(model, newdata = data.frame(duration = current_dur))

# inverse model to estimate duration given a cumulative frequency, useful for
# annotations on the chart:
inv_model <- loess(duration ~ x, 
                   data = data.frame(duration = interstate_cumulative$duration, 
                                     x = fitted(model)))

# of wars that last this long, what is the median cumulative frequency (i.e. half-way to 1):
conditional_median_freq <- (1 + current_cf) / 2
# of wars with that median cumulative frequency, convert it back into a duration,
conditional_median_dur <- predict(inv_model, data.frame(x = conditional_median_freq))

# Draw chart of cumulative distribution:
interstate_cumulative |> 
  ggplot(aes(x = duration, y = cumulative_freq)) +
  geom_smooth(method = "loess", colour = "grey80") +
  geom_line() +
  # note that (seems a bit odd) need to manually do the scale transform to geom_segment here:
  geom_segment(x = log10(current_dur), xend = log10(current_dur), y = -Inf, yend = current_cf, colour = "red") +
  geom_segment(x = 0, xend = log10(current_dur), y = current_cf, yend = current_cf, colour = "red") +
  geom_segment(x = log10(conditional_median_dur), xend = log10(conditional_median_dur), y = -Inf, yend = conditional_median_freq, colour = "blue") +
  geom_segment(x = 0, xend = log10(conditional_median_dur), y = conditional_median_freq, yend = conditional_median_freq, colour = "blue") +
  
  annotate("text", x = current_dur * 0.95, y = 0.39, label = "Current Iran war", colour = "red", hjust = 1) +
  annotate("text", x = conditional_median_dur * 1.05, y = 0.62, colour = "blue", hjust = 0, vjust = 1, 
           label = glue("Median expectation conditional 
on at least {current_dur} days")) +
  scale_x_log10(label = comma, breaks = c(10, current_dur, 100, conditional_median_dur, 1000)) +
  labs(x = "Total duration of war (in days, logarithmic scale)",
       y = "Cumulative frequency of wars",
       title = "Expectations of duration of Iran war, based on modern inter-state wars' duration",
       subtitle = glue("Comparison to wars from 1823 to 2003. The median war that lasts {current_dur} days goes on to last {round(conditional_median_dur)} days."),
       caption = simple_caption)
{% endhighlight %}

We can use the same approach to calculate not just the median war duration (conditional on getting to 74 days) but other percentiles. For example, in the below we can construct an 80% prediction interval (between the 0.1 and 0.9 quantiles) of total duration of 94.9 and 1,752 days. To put this another way, from this 74 day point, only 10% of wars will have a total duration of 94.9 or less days (ie another 21 days). 

All up, that's a big range of course; the main thing it tells us is that wars last longer than many people would like, and there's a big variation in wars' duration.

{% highlight R lineanchors %}
# some prediction intervals, conditional on getting to 74 days:
probs <- c(0.05, 0.1, 0.5, 0.8, 0.9, 0.95)
more_freqs <- probs * (1 - current_cf) + current_cf
conditional_dur <- predict(inv_model, data.frame(x = more_freqs))
tibble(probability = probs, duration = conditional_dur)
# so 80% of wars that reach 74 days will have a total duration between 95 and 1,752 days
{% endhighlight %}

```
  probability duration
        <dbl>    <dbl>
1        0.05     82.3
2        0.1      94.9
3        0.5     261. 
4        0.8    1141. 
5        0.9    1752. 
6        0.95   2119. 
```

## Duration and other factors

So I'd answered my main question but I was naturally curious about some other relationships too. Obviously one expects longer wars to have more deaths in battle; can we see this in the data? Yes we can:

<object type="image/svg+xml" data='/img/0321-duration-deaths.svg' width='100%'><img src='/img/0321-duration-deaths.png' width='100%'></object>

I like this chart as presenting the scale of nearly two centuries of inter-state war in one easy visualisation.

We also see that if there's a pattern in relationship between duration, deaths and when the war started (the starting year mapped to colour in the chart above) it's not an obvious one. We'll come back to that in the next chart, but first, here's the code to create the scatter plot above.


{% highlight R lineanchors %}
#------------------Compare duration and number of deaths----------------
interstate_wars |> 
  ggplot(aes(x = duration, y = bat_death, label = war_name)) +
  geom_point(aes(colour = start_year), size = 3.5) +
  geom_text_repel(colour = "grey50", size = 2, seed = 123) +
  scale_y_log10(label = comma) +
  scale_x_log10(label = comma) +
  scale_colour_viridis_c() +
  labs(title = "Inter-state wars, 1823-2003",
       colour = "Starting year",
       x = "Duration in days",
       y = "Number of battle deaths",
       caption = simple_caption) +
  theme(legend.position = c(0.15, 0.8))
{% endhighlight %}

I was a bit worried about that "two centuries" thing. Are recent wars all much shorter, or perhaps much longer, than older wars? If so it would be a big limitation on my inference about likely war length. So I prepared one more plot to check out if there was an obvious relationship, more rigorously than just eye-balling colour on the previous plot. I was a bit surprised to see that actually there is no real growth or reduction in war duration over time:

<object type="image/svg+xml" data='/img/0321-duration-history.svg' width='100%'><img src='/img/0321-duration-history.png' width='100%'></object>

I also quite like this chart as giving us an instant comparison of our current USA-Israel-Iran war with some of those in history. We can see that it is already longer than the Boxer Rebellion, but not quite as long as the Falkland Islands or the War for Kosovo (for all of these names I am using those provided by the Correlates of War project - I'm well aware that these are contested labels).

Here's my final chunk of code drawing that last chart:

{% highlight R lineanchors %}
#------------Compare duration with when in history it happened---------------
interstate_wars |> 
  arrange(bat_death) |> 
  ggplot(aes(x = earliest_start, y = duration)) +
  geom_hline(yintercept = current_dur, colour = "red") +
  geom_point(aes(size = bat_death), shape = 1) +
  geom_text_repel(aes(label = war_name), colour = "steelblue", size = 3, seed = 123) +
  annotate("text", x= as.Date("1820-01-01"), y = current_dur + 8, hjust = 0,
           label = "Duration of 2026 US-Israel-Iran war so far", colour = "red") +
  scale_y_log10(label = comma) +
  scale_size_area(label = comma, max_size = 25) +
  labs(title = "Inter-state wars, 1823-2003",
       subtitle = glue("Compared to the USA-Israel-Iran war as at {Sys.Date()}"),
       x = "Start of war",
       y = "Duration of war (days)",
       size = "Number of batlle deaths:",
       caption = simple_caption)
{% endhighlight %}

That's all folks. Stay safe out there.
