---
layout: post
title: Animating the US Treasury yield curve rates
date: 2019-04-21
tag: 
   - Visualisation
   - Economics
   - R
description: svglite is a great way for producing good graphics for the web but there's a small hack needed if you want to use web fonts with it.
image: /img/0151-all-years-one-frame.svg
socialimage: http://freerangestats.info/img/ozpolls/0151-all-years-one-frame.png
category: R
---

My eye was caught by [this tweet by Robin Wigglesworth](https://twitter.com/RobinWigg/status/1113761015106363392) of the Financial Times with an Alan Smith animation of the US Treasury yield curve from 2005 to 2009. It's a nice graphic and it made me wonder what it would look like over a longer period.

The "yield curve" is the name given to the graphic showing the different annual rates paid on investors who in effect lend money to the US Treasury. In normal circumstances, investors who are prepared to make the loan on a long term basis demand higher effective interest rates as compensation for risk associated with inflation and future interest rates. When the situation goes in the other direction, and higher effective returns come from shorter term investments, it's commonly seen as a sign of a coming recession. This isn't because the "inverted yield curve" causes the recession, but because (amongst other things) it reflects investors' fears that interest rates are about to be cut (in response to lower economic growth), hence best to lock in the return you can get now.

## Animation options

Anyway, here is my version of the animation, now with 29 years of daily data, streaming from You Tube (because it was too large to embed directly in my web page):

   <iframe width="600" height="540 "
src="http://www.youtube.com/embed/" frameborder="0" allowfullscreen="allowfullscreen">
</iframe>

Alternatively, here's a streamlined social-media-friendly version (as a GIF) that has monthly averages and scoots through a bit quicker.

<img src='/img/0151-yield-anim-monthly.gif' width='100%'>

I'm not sure this is a great graphic. For one thing, with three frames for every day with data reported, the daily version is over 30,000 frames long and even at 25 frames per second that's a pretty tedious thing to watch. But more generally, it suffers from the common problems of animated statistical graphics

- you have to be looking at just the right time to see the interesting things - like the yield inversion and other volatility in that 2006 to 2008 period
- some changes over time are actually less obvious to the viewer than if they could be presented on a single page.

In the [original FT article](https://www.ft.com/content/cf9eb29a-5220-11e9-9c76-bf4a0ce37d49), there's a more focused animation by Smith, this one showing just the last 15 months, emphasising the yield inverstion at the end of that period. It's quite effective in telling a very pointed story, rather than trying to show a long history in a single animation.

## Static alternatives

Animations aren't always the best. In fact, I loathe video tutorials because I can't flick my eye directly to the part I want but have to sit through the whole thing - even at high speed this is frustrating. Animations can have a similar problem.

For showing the longer history at a glance, consider this alternative presentation, still related to the animation's visual structure. This has all 10,000 yield curves from those 29 years on a single static chart, each one drawn with 10% opacity to avoid colouring the page solid.

<img src='/img/0151-all-years-one-frame.svg' width='100%'>

What I like about this version is that because it's all there at once, you can make the comparisons across a broader period of time than is possible with the animation. You can see:

- The secular decrease in rates over this time period
- The period around 2005 when there were no 30 year rates for some reason
- The different yield inversions in context - showing as upwards swooshes in blue, green and yellow (for different periods) at the left of the graph

Plus it's very pretty.

Finally, how about a much more traditional time series line chart. I actually find this the easiest to interpret of them all.

<img src='/img/0151-trad-line.svg' width='100%'>

- The secular decrease is obvious again.
- The carefully chosen colours let you compare the different terms quite easily.
- This is the most dramatic depictiton of the way the shorter term yields hit the floor after 2008.
- Easiest chart of them all to interpret because of the familiar convention of the horizontal axis meaning time
- Still very pretty.

## Reflections

So what do I think?

> Sometimes static graphs are beter than animations. Sometimes an old fashioned time series plot is better than less standard innovations.

More information on yield curves is easy to find on the Internet, and the [piece in the Financial Times that Wigglesworth was referring to](https://www.ft.com/content/cf9eb29a-5220-11e9-9c76-bf4a0ce37d49) is worth a read. Here's one of their very professional graphics:

<a href = https://www.ft.com/content/cf9eb29a-5220-11e9-9c76-bf4a0ce37d49><img src='/img/0151-ft-timeseries.png' width='100%'></a>

One of the things that makes a difference to this, and to the animations in the same story, is the careful use of annotations to tell a story. That's definitely what it takes to get a graphic to the next level, but it's not something I've got time for right now.

Here's the code for this (apart from that last one from the FT) - all pretty straightforward. Grabbing the data is easy with Wickham's `rvest` R package, and animations are so much easier than a few years ago thanks to Thomas Pedersen's `gganimate`. And let's not forget the Viridis colours, originally developed for Pyuthon by Nathaniel Smith and Stefan van der Walt, and ported into an R package by Simon Garnier. In these graphics I use three variants of the Viridis colour schemes to represent different variables - the difference between long and short term yields at any one point in time, year, and the term of the yield. In each case, the variable I am trying to show is fundamentally ordered, and Viridis is at its best in showing that ordered nature in its colour.

{% highlight R lineanchors %}

#------------------------------load up functionality and import data-------------
library(rvest)
library(tidyverse)
library(scales)
library(gganimate)
library(svglite)
library(frs)
library(lubridate)

the_caption <- "Data from https://www.treasury.gov/, analysis by freerangestats.info"

# we read in the data a year at a time because although there is a page with all 29 years of data,
# it was too difficult to read in all at once ie it crashed my humble laptop. So we define a function
# to get one year's data:
read_a_year <- function(y){
  url_stub <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=XXXX"
  url <- gsub("XXXX", y, url_stub)
  
  yield_page <- read_html(url)
  yield_data <- as_tibble(html_table(yield_page, fill = TRUE)[[2]] )
  
  return(yield_data)
  
}

# bring in the data one year at a time.
yields_l <- lapply(1990:2019, read_a_year)

# clean and reshape the data with some handy variables for charting
periods <- tibble(
  period = paste(c(1,2,3,6,1,2,3,5,7,10,20,30), rep(c("mo", "yr"), c(4,8))),
  period_n = c(30, 60, 90, 180, 365.25 * c(1,2,3,5,7,10,20,30))
)

yields <- do.call("rbind", yields_l) %>%
  mutate(Date = as.Date(Date, format = c("%m/%d/%y"))) %>%
  gather(period, value, -Date) %>%
  mutate(value = suppressWarnings(as.numeric(value))) %>% 
  left_join(periods, by = "period") %>%
  group_by(Date) %>%
  mutate(ratio_5_30 = value[period == "30 yr"] / value[period == "5 yr"],
         yield_av = mean(value, na.rm = TRUE, tr = 0.2),
         yield_30yr = value[period == "30 yr"],
         yield_3mo = value[period == "3 mo"],
         diff303 = yield_30yr - yield_3mo) %>%
  ungroup() %>%
  filter(!is.na(value)) 


#----------------All dates in one chart---------------------  
col_br <- tibble(
  lab = c(1990, 2000, 2010),
  date = as.Date(paste0(c(1990, 2000, 2010), "-03-01"))
) %>%
  mutate(date_n = as.numeric(date))

p2 <- yields %>% 
#  filter(Date < as.Date("1990-01-31")) %>%
  ggplot(aes(x = period_n, y = value, group = Date, colour = as.numeric(Date))) +
  geom_path(alpha = 0.1) +
  scale_colour_viridis_c("",
                         breaks = pull(col_br, date_n),
                         labels = pull(col_br, lab)) +
  scale_y_continuous("Treasury yield curve rate") +
  scale_x_continuous("", breaks = periods[c(10:12), ]$period_n,
                     labels = periods[c(10:12), ]$period) +
  labs(caption = the_caption)

print(p2)


#--------------animated MP4 version----------
d <- yields  #%>% 
  filter(Date < as.Date("1991-02-28"))

a <- d %>% 
  ggplot(aes(x = period_n, y = value)) +
  geom_segment(data = distinct(d, Date, yield_3mo, yield_30yr),
               x = 90, xend = 10958, aes(y = yield_3mo, yend = yield_30yr),
               colour = "grey50") +
  geom_line(size = 1.5, aes(colour = diff303)) +
  scale_y_continuous("Treasury yield curve rate") +
  scale_x_continuous("", breaks = periods[c(10:12), ]$period_n,
                     labels = periods[c(10:12), ]$period) +
  scale_colour_viridis_c("Premium for long term lending:\n30 year yield minus 3 month yield", 
                         option= "A", direction = -1) +
  labs(title = "US Treasury Yield Curve Rates, 1990 to 2019",
       subtitle = 'Date: {frame_time}',
       caption = the_caption) +
  transition_time(Date) 



dir.create("tmp")
res <- 150
animate(a, nframes = length(unique(d$Date)) * 3, dev = "png", fps = 30,
        type = "cairo-png", antialias = "subpixel", 
        width = 6 * res, height =  4.3 * res, res = res,
        renderer = file_renderer(dir = "tmp"))
# use movie software to pin together the files into an animation outside of R

#----------------traditional time series version-----------
p3 <- yields %>%
  mutate(period = fct_reorder(period, period_n)) %>%
  ggplot(aes(x = Date, y = value, colour = period)) +
  geom_line() +
  scale_colour_viridis_d(option = "C") +
  labs(x = "", colour = "Term", y = "Treasury yield curve rate",
       caption = the_caption) +
  ggtitle("US Treasury Yield Curve Rates, 1990 to 2019")

print(p3)


#-----------------Gif version for Twitter------------
# shorter version with just one main observation per month
d2 <- yields %>%
  mutate(mon = month(Date),
         yr = year(Date)) %>%
  group_by(mon, yr, period_n) %>%
  summarise(value = mean(value),
            yield_3mo = mean(yield_3mo),
            yield_30yr = mean(yield_30yr),
            diff303 = mean(diff303)) %>%
  ungroup() %>%
  mutate(Date = as.Date(paste(yr, mon, 15, sep = "-"), format = "%Y-%m-%d"))
  

a2 <- d2 %>% 
  ggplot(aes(x = period_n, y = value)) +
  geom_segment(data = distinct(d, Date, yield_3mo, yield_30yr),
               x = 90, xend = 10958, aes(y = yield_3mo, yend = yield_30yr),
               colour = "grey50") +
  geom_line(size = 1.5, aes(colour = diff303)) +
  scale_y_continuous("Treasury yield curve rate") +
  scale_x_continuous("", breaks = periods[c(10:12), ]$period_n,
                     labels = periods[c(10:12), ]$period) +
  scale_colour_viridis_c("Premium for long term lending:\n30 year yield minus 3 month yield", 
                         option= "A", direction = -1) +
  labs(title = "US Treasury Yield Curve Rates, 1990 to 2019",
       subtitle = 'Date: {frame_time}',
       caption = the_caption) +
  transition_time(Date) 



res <- 150
animate(a, nframes = length(unique(d2$Date)) * 6, dev = "png", fps = 15,
        type = "cairo-png", antialias = "subpixel", 
        width = 6 * res, height =  4.3 * res, res = res)

anim_save("0151-yield-anim150.gif")

{% endhighlight %}

{% highlight R lineanchors %}

{% endhighlight %}


{% highlight R lineanchors %}

{% endhighlight %}
