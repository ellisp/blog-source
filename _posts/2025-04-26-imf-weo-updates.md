---
layout: post
title: World Economic Outlook
date: 2025-04-26
tag: 
   - Economics
   - DataFromTheWeb
   - Pacific
   - OpenData
   - WorkRelated
description: I have a quick look at the latest World Economic Outlook released by the IMF, with a particular eye on the economic growth forecasts for Pacific island countries. The Pacific countries that have had the biggest revision downwards in their growth prospects over the six months since the last Outlook are the three in the Compact of Free Association with USA (Palau, Marshall Islands, and Federated States of Micronesia), plus Fiji.
image: /img/0288-pict-growth.png
socialimage: https:/freerangestats.info/img/0288-pict-growth.png
category: R
---

This week the World Bank and International Monetary Fund "Spring Meetings" have been in progress, and a highlight is always the publication of the latest World Economic Outlook. This never fails to get a bit of press attention around the world. This year much of it was like [this BBC piece](https://www.bbc.com/news/articles/czx415erwkwo), focusing on the revision in growth forecasts for 2025 (ie this year) compared to forecasts made for the same year in the last (January 2025) update of the Outlook. Of course, the current context is dramatic policy changes in the USA since January - the three months since the last update coincide with the new US administration of Donald Trump.

### Revised growth estimates for 2025

I had several motivations for exploring this data. In fact, the first one was I wanted to see exactly what "economic growth" is being highlighted by the BBC and others in this situation - is it real or nominal, per capita or just the absolute size of the economy? But I also wanted to see what the IMF had to say about the economic growth prospects for the Pacific island countries that I work with in my day job. 

So the first plot I polished up was this one, using a similar approach to the main graphic in the BBC article linked above, but highlighting the Pacific rather than G7 members:

<object type="image/svg+xml" data='/img/0288-pict-growth.svg' width='100%'><img src='/img/0288-pict-growth.png' width='100%'></object>

Unlike the BBC, I'm comparing the April 2025 World Economic Outlook to the edition that came out in October 2024 - the last time the database was published for all countries that the IMF studies rather than just an important subset. All the code to download and wrangle the data and polish the plots is at the bottom of the post.

We see an interesting story in the chart above - a bit more mixed than the case for the richer countries, who uniformly had a revision downwards of growth estimates. The three Pacific countries in a "Compact of Free Association" with the USA (Palau, Marshall Islands, and Federated States of Micronesia) all see a material revision downwards in their expected GDP growth for 2025, as does Fiji (which stands out as the largest economy of the region). For other Pacific countries, however, the revision is small and/or in the other direction. Samoa, for example, has seen an increase in its expected growth (on IMF estimates) for 2025 from 4.2% to a pretty healthy 5.4%.

Also we can see in this chart the answer to my earlier question - the economic growth in question is real (constant prices), but not adjusted for population size.

To be sure of that last point, I re-created the BBC's chart for the G7 countries, adding in the four BRIC (Brazil, Russia, India and China) countries and again using the October 2024 edition as my comparison point. That gets us this next chart below. If you compare to the BBC original you will see the April 2025 estimates are identical, but of course the October 2024 estimates in my chart differ from the counterpart light blue bars in the BBC chart, which were based on January 2025 estimates.

<object type="image/svg+xml" data='/img/0288-selected-growth.svg' width='100%'><img src='/img/0288-selected-growth.png' width='100%'></object>

 OK, that's all very well for looking at a small number of countries at once, but what were the 2025 growth revisions for all countries in the world? And how does the Pacific compare to that? For this task we have too many countries for a bar chart and the best approach is a scatter plot, with the old forecast on the horizontal axis and the new one on the vertical axis. That gets us this chart:

<object type="image/svg+xml" data='/img/0288-pict-growth-scatter.svg' width='100%'><img src='/img/0288-pict-growth-scatter.png' width='100%'></object>

The diagonal line represents equal forecasts in both editions; it's not a line of best fit, which would be somewhat to the right of the line actually drawn. Consistent with all the public commentary, most countries have seen a revision downwards in their economic growth prospects. This has been most dramatic for Sudan and Southern Sudan, for [reasons not related to tariffs](https://www.bbc.com/news/world-africa-59035053).

### Overall growth trajectories

Revisions in growth forecasts are good for people looking for the very latest "market-moving" news, but probably more important is the big picture of economic growth trends. I doubt many of my readers knew that Palau's forecast economic growth for 2025 six months ago was 8.8%, so perhaps the news that it is downgraded to the (still pretty positive) 5.6% leaves you cold! 

More interesting for big picture people is perhaps some line charts showing GDP as a whole. For this purpose, I have chosen to emphasise GDP that is corrected for both changes in local prices (so constant prices) and cross-country differences in what can be purchased (purchasing power parity); and per capita. This is the best measure in this data of average economic situation of these countries.

To start with, here's the global picture with 200+ countries and regions (like "Sub-Saharan Africa"). I've used a logarithmic scale so equal growth rates would show up as equal slopes:

<object type="image/svg+xml" data='/img/0288-all-growth-line.svg' width='100%'><img src='/img/0288-all-growth-line.png' width='100%'></object>

This doesn't really tell us much, other than perhaps the familiar; there are some countries and regions that are very rich (greater than US$100,000 per capita GDP) and some that are very poor (less than US$1,000); generally the tendency is for an increase in GDP per capita over time, including the forecast period out to 2030.

What about for Pacific island countries? Here's the chart for just those:

<object type="image/svg+xml" data='/img/0288-pict-growth-line.svg' width='100%'><img src='/img/0288-pict-growth-line.png' width='100%'></object>

We see again there is considerable variation in GDP per capita by country from relatively poor Kiribati and Solomons to well off Fiji and Palau; growth is generally positive; and Covid-19 had a big impact in Fiji, Palau and Samoa.

Would we have seen a similar picture if we'd done this six months ago? This next chart overlays the October 2024 forecasts with those released in April 2025. While we know from the bar charts earlier that the growth forecasts for all countries were revised somewhat, the only one that is really visible here in this time series context is Marshall Islands.

<object type="image/svg+xml" data='/img/0288-pict-growth-comp-line.svg' width='100%'><img src='/img/0288-pict-growth-comp-line.png' width='100%'></object>

I was interested for professional reasons in these revisions, particularly the big one in the Marshall Islands, so dug into a bit. The working graphic that I used to do this is a bit complex and you'll want to click on the below to see it full screen. It shows the ratios of the April 2025 estimates to the October 2024 ones. What this highlights is that the big driver in the Marshall Islands revision of PPP GDP per capita is a big drop in population numbers. The census came out in this time period, with a material downwards revision in the population of the Marshall Islands, so this makes sense: 

<a href='/img/0288-pac-ratios.svg'><img src='/img/0288-pac-ratios.png' width='100%'></a>

Plus Tonga - some interesting revisions were clearly made there.

## Code

The IMF makes the whole World Economic Outlook available in a single SDMX file, very handy for this sort of use. Today's code is pretty unremarkable. There are probably two possible points of interest for people:

* downloading and parsing SDMX data
* polishing of plots (perhaps particularly of the first couple of bar charts, the most polished for this piece)

### Accessing data

{% highlight R lineanchors %}
library(readxl)
library(tidyverse)
library(rsdmx)
library(patchwork)

#----------------download sdmx version-------------------------
options(timeout=600)

#' Download and unzip a file, if not already done:
dluz <- function(url, destfile){
  if(!file.exists(destfile)){
    download.file(url, destfile = destfile, mode = "wb")
    unzip(destfile)
  }
}

# to see the outlooks where the whole database is available, all countries, see
# https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending

dluz("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOAPR2025-SDMXData.ashx",
              destfile = "weo2025-apr.zip")


dluz("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2024/October/WEOOCT2024-SDMXData.ashx",
              destfile = "weo2024-oct.zip")

#---------------processing---------------
if(!(exists("d2025") & exists("d2024"))){
  d2025 <- readSDMX("WEOAPR2025/xmlfile_APR2025.xml", isURL = FALSE) |> 
    # this parsing takes a long time:
    as_tibble()
  
  d2024 <- readSDMX("WEOOCT2024/WEO_PUB_OCT2024.xml", isURL = FALSE) |> 
    # this parsing takes a long time:
    as_tibble()
}

# classifications / metadata:
concept <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "CONCEPT", skip = 7)[, 1:2] |> 
  rename(CONCEPT = Code,
         concept = Description)

unit <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "UNITS", skip = 8, 
                  col_names = c("UNIT", "unit"))

ref_areas <-   read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "REF_AREA", skip = 7)[, 1:2] |> 
  rename(REF_AREA = Code,
         country = Description)

weo2025 <- d2025 |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything()) |> 
  mutate(type = if_else(year > as.numeric(LASTACTUALDATE), "Forecast", "Actual"),
         type = fct_relevel(type, "Forecast"),
         edition = "WEO April 2025")

filter(weo2025, is.na(value)) |> count(OBS_VALUE)
# just -- and n/a, so OK


weo2024 <- d2024 |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything()) |> 
  mutate(type = if_else(year > as.numeric(LASTACTUALDATE), "Forecast", "Actual"),
         type = fct_relevel(type, "Forecast"),
         edition = "WEO October 2024")

weo_both <- rbind(weo2024, weo2025)



regions <- c(
  "World",
  "Advanced Economies",
  "G7",
  "Other Advanced Economies (Advanced Economies excluding G7 and Euro Area countries)",
  "Euro area",
  "Emerging Market and Developing Economies",
  "Latin America and the Caribbean",
  "Middle East and Central Asia (MECA)",
  "Emerging and Developing Asia",
  "ASEAN-5",
  "Sub-Sahara Africa",
  "Emerging and Developing Europe",
  "European Union"
)

pacific <- c(
  "Solomon Islands",
  "Fiji",
  "Kiribati",
  "Nauru",
  "Vanuatu",
  "Papua New Guinea",
  "Samoa",
  "Tonga",
  "Marshall Islands",
  "Micronesia",
  "Tuvalu",
  "Palau"
)

stopifnot(all(pacific %in% ref_areas$country))
stopifnot(all(regions %in% ref_areas$country))
{% endhighlight %}

### Polishing plots

{% highlight R lineanchors %}
#=============================charts===========================

#-------------headline year on year growth rates------------

# from trial and error on which series ("CONCEPT") to use, we see the figures
# used for headline is real GDP growth so constant prices, but not per capita
growth_comps <- weo_both |> 
  filter(CONCEPT == "NGDP_R") |> 
  filter(year %in% 2024:2025) |> 
  group_by(country, edition) |> 
  summarise(growth = value[year == 2025] / value[year == 2024] - 1) |> 
  ungroup()

selected <- c(
  "United States",
  "Canada",
  "Japan",
  "United Kingdom",
  "Germany",
  "Italy",
  "France",
  "China",
  "India",
  "Russia",
  "Brazil"
)

bgcol <- "grey90"

growth_comps |> 
  filter(country %in% selected) |> 
  mutate(country = factor(country, levels = selected[length(selected):1])) |> 
  mutate(text_col = ifelse(growth < 0, "negative", "positive")) |> 
  ggplot(aes(x = growth, y = country, fill = edition)) +
  geom_col(position = "dodge", width = 0.75, colour = bgcol) +
  geom_text(aes(label = percent(growth, accuracy = 0.1), colour = text_col), 
            position = position_dodge(width = 0.7), hjust = 1, vjust = 0.5,
            fontface = "bold", size = 3) +
  scale_fill_manual(values = c("darkblue", "steelblue"), guide = guide_legend(reverse = TRUE)) +
  scale_colour_manual(values = c("positive" = "white", "negative" = "black")) +
  scale_x_continuous(label = percent) +
  geom_vline(xintercept = 0) +
  guides(colour = "none") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = NA),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.8, 0.8)) +
  labs(fill = "",
       x = "",
       y = "",
       title = "Projected real annual GDP growth in 2025 for selected nations",
       subtitle = "Forecasts for 2025, made in October 2024 and April 2025 in the IMF World Economic Outlook.
The update for some, but not all, countries in January 2025 is not considered here.",
       caption = "Source: IMF World Economic Outlooks. Growth rates based on constant prices, but are not per capita.")


growth_comps |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, -growth, .fun = mean)) |> 
  mutate(text_col = ifelse(growth < 0, "negative", "positive")) |> 
  ggplot(aes(x = growth, y = country, fill = edition)) +
  geom_col(position = "dodge", width = 0.75, colour = bgcol) +
  geom_text(aes(label = percent(growth, accuracy = 0.1), colour = text_col), 
            position = position_dodge(width = 0.7), hjust = 1, vjust = 0.4,
            fontface = "bold", size = 3) +
  scale_fill_manual(values = c("darkblue", "steelblue"), guide = guide_legend(reverse = TRUE)) +
  scale_colour_manual(values = c("positive" = "white", "negative" = "black")) +
  scale_x_continuous(label = percent) +
  geom_vline(xintercept = 0) +
  guides(colour = "none") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = NA),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.8, 0.8)) +
  labs(fill = "",
       x = "",
       y = "",
       title = "Projected real annual GDP growth in 2025 for Pacific IMF members",
       subtitle = "Forecasts for 2025, made in October 2024 and April 2025 in the IMF World Economic Outlook.",
       caption = "Source: IMF World Economic Outlooks. Growth rates based on constant prices, but are not per capita.")


gcw <- growth_comps |> 
  spread(edition, growth) |> 
  mutate(is_pict = ifelse(country %in% pacific, "Pacific", "Other"))

annot_col <- "darkorange"

gcw |> 
  ggplot(aes(x = `WEO October 2024`, y = `WEO April 2025`, colour = is_pict)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey75") +
  geom_point() +
  geom_text_repel(data = filter(gcw,
                                `WEO April 2025` > 0.10 |
                                  `WEO October 2024` < 0 |
                                  `WEO October 2024` > 0.08 | 
                                  is_pict == "Pacific"),
                  aes(label = country),
                  seed = 123) +
  theme(legend.position = "none") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  scale_colour_manual(values = c("Pacific" = "blue", "Other" = "grey60")) +
  coord_equal() +
  annotate("text", x = 0.2, y = 0.04, label = "Forecast lower in April 2025", 
           fontface = "italic", colour = annot_col) +
  annotate("text", x = 0, y = 0.12, label = "Forecast higher in April 2025", 
           fontface = "italic", colour = annot_col) +
  labs(x = "Forecast as at October 2024",
       y = "Forecast as at April 2025",
       title = "Changes in IMF forecasts of real annual GDP growth for 2025, by country",
       subtitle = "Pacific island countries highlighted. Diagonal line shows the same forecast in both 2024 and 2025.",
       caption = "Source: IMF World Economic Outlooks. Growth rates based on constant prices, but are not per capita.")


#--------------------growth time series--------------------------

weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(colour = country, linewidth = type)) +
  geom_smooth(se = FALSE, colour = "black", method = "gam") +
  theme(legend.position = "none") +
  scale_y_log10(label = dollar) +
  scale_linewidth_manual(values = c("Actual" = 0.9, "Forecast" = 0.6)) +
  labs(title = "Gross domestic product per capita, constant prices",
       subtitle = "Purchasing power parity; 2021 international dollar",
       x = "",
       y = "GDP per capita (logarithmic scale)",
       caption = "Source: IMF World Economic Outlook 'NGDPRPPPPC', April 2025")

weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, colour = type, linewidth = type)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita in the Pacific",
       subtitle = "Purchasing power parity, constant prices; 2021 international dollar",
       x = "", 
       y = "GDP per capita (original scale)",
       caption = "Source: IMF World Economic Outlook 'NGDPRPPPPC', April 2025") +
  scale_y_continuous(label = dollar) +
  scale_linewidth_manual(values = c("Actual" = 1.5, "Forecast" = 1)) +
  scale_colour_manual(values = c("Actual" = "darkblue", "Forecast" = "lightblue"))
# interesting here that many of the countries did not have the big covid-related
# dip in GDP that Fiji did (or at least, it doesn't show up in their stats)


# comparison of april 2025 and October 2024 forecasts
weo_both |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, colour = edition, linewidth = type)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position = "bottom") +
  labs(title = "Gross domestic product per capita in the Pacific",
       subtitle = "Purchasing power parity, constant prices; 2021 international dollars",
       x = "",
       y = "GDP per capita (logarithmic scale)",
       linewidth = "",
       colour = "") +
  scale_linewidth_manual(values = c("Actual" = 1.5, "Forecast" = 1)) +
  scale_y_log10(label = dollar_format(accuracy = 1))
# interesting here that many of the countries did not have the big covid-related
# dip in GDP that Fiji did (or at least, it doesn't show up in their stats)

#---------------digging in to what makes that change, particularly for Marshalls-------------

pac_revisions <- weo_both |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = fct_reorder(country, ratio, .fun = last, .na_rm = TRUE)) 

pac1 <- pac_revisions |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Revisions in economic expectations in the Pacific",
       subtitle = "GDP per capita, PPP constant prices, 2021 international dollars",
       x = "",
       y = "Revision atio")

pac2 <- weo_both |> 
  filter(CONCEPT == "LP") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = " ",
       subtitle = "Population",
       x = "",
       y = "Revision ratio")


pac3 <- weo_both |> 
  filter(CONCEPT == "NGDP" & unit == "National currency") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(size = 1.2, colour = "steelblue") +
  labs(title = "  ",
       subtitle = "GDP in local currency, current prices",
       x = "",
       y = "Revision ratio")

pac5 <- weo_both |> 
  filter(CONCEPT == "NGDPD" & unit == "U.S. dollars") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(size = 1.2, colour = "steelblue") +
  labs(title = "  ",
       subtitle = "GDP in US dollars, current prices",
       x = "",
       y = "Revision ratio")
# not used in blog

pac4 <- weo_both |> 
  filter(CONCEPT == "PPPEX") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "  ",
       subtitle = "Implied purchasing power parity conversion rate",
       x = "",
       y = "Revision ratio",
       caption = "Ratio of IMF estimates in April 2025 to those October 2024 (higher than 1.0 means the estimate was revised upwards)")

pac1 + pac2 + pac3 + pac4 + plot_layout(ncol = 2)
{% endhighlight %}

