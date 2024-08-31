---
layout: post
title: Ratios of indexed line charts
date: 2024-08-30
tag: 
   - Economics
   - Australia
   - DataFromTheWeb
description: I draw some quick charts of Australian economic indicators and ponder the implications. A ratio of two indexes can be a useful part of exploratory analysis.
image: /img/0275-inc-exp-gdp-2014.svg
socialimage: https:/freerangestats.info/img/0275-inc-exp-gdp-2014.png
category: R
---
A few months Michael Read of the Australian Financial Review published a chart showing real household income in Australia plummetting, relative to the OECD, since late 2021. A version of this [drifted across my social media feed](https://mastodon.social/@andyjennings@aus.social/113020422083231612) and interested me. 

First I wanted to re-create the chart, which was straightforward enough as it was using data published by the OECD, accessible in SDMX format from their dot Stat tool. After a bit of experimenting I decided to also look at household expenditure and GDP per capita; because I wanted to understand how much this was just an income issue or a broader one translating into expenditure and GDP. That gives me this chart; the middle panel is identical in substance to Read's original:

<object type="image/svg+xml" data='/img/0275-inc-exp-gdp-2007.svg' width='100%'><img src='/img/0275-inc-exp-gdp-2007.png' width='100%'></object>

There's some interesting things here!
* Yes, the story is bad for Australia household income since late 2021, as per the story of the original chart.
* It's not just an income story - expenditure and GDP have both declined as well, although not as dramatically (implying that some of the lost income is translating into lower savings / wealth).
* The Covid spike upwards in income is matched with a downwards spike in expenditure and GDP.
* Putting aside recent declines, all three series (household income, household expenditure, GDP per capita) show for both Australia and the OECD average that people are better off than in 2007, when the index is set to 100.

It's worth noting before we go on that "real gross disposable income" means it has been adjusted for inflation, it is income after subtracting taxes, social security contributions, change in net equity in pension funds, and interest on financial liabilities (including, I believe, mortgages). So an increase in interest rates that hits households with mortgages is one thing that would show up here. 

Here's the R code that produced that first chart:

{% highlight R lineanchors %}
library(rsdmx)
library(tidyverse)
library(janitor)
library(lubridate)

metadata <- tribble(~measure, ~full_measure,
                    "B6GS1M_R_POP", "Real gross disposable income per capita of households and NPISH",
                    "P3S1M_R_POP", "Real final consumption expenditure per capita of households and NPISH",
                    "B1GQ_R_POP", "Real gross domestic product per capita")

the_caption = "Source: OECD dot Stat P3S1M_R_POP, B6GS1M_R_POP, B1GQ_R_POP. 'NPISH' means 'non-profit institutions serving households'."

d <- readSDMX("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_HHDASH@DF_HHDASH_INDIC,1.0/Q.OECD+AUS.B1GQ_R_POP+B6GS1M_R_POP+P3S1M_R_POP.?dimensionAtObservation=AllDimensions") |>
  as_tibble() |>
  clean_names() |>
  mutate(tp = yq(time_period) ) |>
  left_join(metadata, by = "measure") |>
  mutate(ref_area = case_when(
    ref_area == "AUS" ~ "Australia",
    ref_area == "OECD" ~ "OECD average"
  ))

palette <- c("Australia" = "red", `OECD average` = "blue")

d |>
  ggplot(aes(x = tp, y = obs_value, colour = ref_area)) +
  facet_wrap(~str_wrap(full_measure, 40)) +
  geom_line()  +
  scale_colour_manual(values = palette)  +
  theme(legend.position = c(0.1, 0.7)) +
  labs(colour = "",
       y = "Index (2007 = 100)",
       x = "",
       title = "Growth in household expenditure and income, Australia and OECD average",
       subtitle = "Indexes set to 100 in 2007. Comparison shows relative growth rates, not absolute difference.",
       caption = the_caption)
{% endhighlight %}

Now, I've long been interested in the impact of choice of year for indexing series like this. I have argued before [on this blog](/blog/2016/08/18/dualaxes) that many of the criticisisms of dual axes timeseries plots can be mitigated by treating the lines on the page as indexed time series and choosing scales accordingly; but related to this is the risk that choosing a different index year for your series can have significant difference on the visual impression of the chart.

In the case of the income chart, I was struck with how Australia's real household income grew faster than the OECD average for some years from 2007 and then, from around 2014, the OECD started to catch up. But to see this you have to pay attention to the gap between the lines, whereas the crossing of the lines in 2022 is visually dramatic and appears to really mean something. But it doesn't! or at least, not as much as the naive viewer might think; what the crossing of the lines means is just that the OECD average growth since 2007 has caught up with the Australian growth since 2007, it doesn't mean the OECD average has caught up in absolute terms. 

If we're not particularly interested in 2007 as a starting point, we can choose some other period. In the next chart I've set it to be 2014 which seems to me to be the time that OECD average growth sped up, and Australian growth slowed down. And I think this chart really helps to show that the poor relative performance of Australia on these indicators isn't just the dramatic decline in the last couple of years, but a longer lasting phenomenon:

<object type="image/svg+xml" data='/img/0275-inc-exp-gdp-2014.svg' width='100%'><img src='/img/0275-inc-exp-gdp-2014.png' width='100%'></object>

Interesting, huh? that chart drawn with this:


{% highlight R lineanchors %}
d |>
  group_by(ref_area, measure) |>
  mutate(obs_value = obs_value / obs_value[time_period == "2014-Q1"] * 100) |>
  ggplot(aes(x = tp, y = obs_value, colour = ref_area)) +
  facet_wrap(~str_wrap(full_measure, 40)) +
  geom_line()  +
  scale_colour_manual(values = palette)  +
  theme(legend.position = c(0.1, 0.7)) +
  labs(colour = "",
       y = "Index (2014 = 100)",
       x = "",
       title = "Growth in household expenditure and income, Australia and OECD average",
       subtitle = "Indexes set to 100 in first quarter of 2014. Comparison shows relative growth rates, not absolute difference.",
       caption = the_caption)
{% endhighlight %}

Finally, I wanted to help the viewer make the comparison I was doing for myself in my head. I realised that one way to do this is to turn the two lines into a single line that is the ratio between them. That gives me this chart. I like this; I think the analytical step of dividing one of the indexed indicators by the other really is adding value here, if the intent is to compare growth in Australian indicators relative to the OECD's. Now the turning point in around 2014 is much more obvious, and the shape of the line is robust to choice of index year, which is a very desirable property:

<object type="image/svg+xml" data='/img/0275-aus-oecd-ratios.svg' width='100%'><img src='/img/0275-aus-oecd-ratios.png' width='100%'></object>

That final chart drawn with this code:

{% highlight R lineanchors %}
d |>
  group_by(ref_area, full_measure) |>
  mutate(obs_value = obs_value / obs_value[time_period == "2014-Q1"] * 100) |>
  group_by(full_measure, tp, time_period) |>
  summarise(aus_oecd_ratio = obs_value[ref_area == "Australia"] / obs_value[ref_area == "OECD average"] * 100) |>
  ggplot(aes(x = tp, y = aus_oecd_ratio)) +
  facet_wrap(~str_wrap(full_measure, 40)) +
  geom_line(colour = "steelblue")  +
  theme(legend.position = c(0.3, 0.7)) +
  labs(title = "Australia | OECD average relative growth in various measures",
       subtitle = "Ratio set to equal 100 in first quarter of 2014. Indicator shows a relative comparison of growth rates, not an absolute comparison.",
       colour = "",
       caption = the_caption,
       x = "",
       y = "Index (2014 = 100)")
{% endhighlight %}

So, that's all for today. I'm not going to try to explain structural challenges in the Australian economy or look to possible causes of this relative decline since about 2014; that would be well out of scope for today's blog. But I'm still interested in anyone who believes they have the answer!