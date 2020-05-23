library(tidyverse)
library(scales)

counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

georgia5 <- counties %>%
  filter(state == "Georgia" & county != "Unknown") %>%
  group_by(county) %>%
  summarise(cases = max(cases)) %>%
  arrange(desc(cases)) %>%
  slice(1:5) %>%
  pull(county)



d <- counties %>%
  filter(county %in% georgia5 & state == "Georgia") %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  ungroup()

fill_palette <- c(
  "Cobb" = "#5954b0",
  "DeKalb" = "#238a8f",
  "Fulton" = "#98863c",
  "Gwinnett" = "#965b31",
  "Hall" = "#2460ac"
)

stroke_palette <- c(
  "Cobb" = "#827cf8",
  "DeKalb" = "#42d1e1",
  "Fulton" = "#e5cd63",
  "Gwinnett" = "#e28f4d",
  "Hall" = "#3da2f2"
)


theme_set(  theme_minimal() +
              theme(panel.background = element_rect(fill = "#0f3051", colour = NA),
                    panel.grid = element_blank(),
                    plot.background = element_rect(fill = "#0f3051"),
                    text = element_text(colour = "grey70"),
                    axis.text = element_text(colour = "grey70"),
                    axis.line = element_line(colour = "grey90"),
                    strip.text = element_text(colour = "grey80"),
                    legend.position = "top")
)

the_caption <- "Source: analysis by http://freerangestats.info with county-level COVID-19 case data from New York Times"
title <- "Top 5 Counties in Georgia with the Greatest Number of Confirmed COVID-19 Cases"
st <- str_wrap("Note that this chart is to illustrate poor visual design choices and does not include 
the most current data. It uses different data from the original 
               from the Georgia Department of Public Health.", 120)

p1 <- d %>%
  filter(date >= as.Date("2020-04-27")) %>%
  filter(date <= as.Date("2020-05-09")) %>%
  mutate(date = fct_reorder(as.character(format(date, "%d%b%Y")), -new_cases, .fun = sum),
         # reorder county for use for colour and legend:
         county = fct_reorder(county, -new_cases),
         # a new "county2" is reordered within date
         county2 = tidytext::reorder_within(county, -new_cases, within = date)) %>%
  ggplot(aes(x = date, weight = new_cases, fill = county, colour = county)) +
  geom_bar(position = "dodge", aes(group = county2)) +
  scale_colour_manual(values = stroke_palette) +
  scale_fill_manual(values = fill_palette) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = title,
       subtitle = st,
       colour ="",
       fill = "",
       x = "",
       y = "New cases per day",
       caption = the_caption)

p2 <- d %>%
  ggplot(aes(x = date, y = new_cases, colour = county)) +
  geom_point(alpha = 0.5) +
  geom_smooth(size = 1.5, se = FALSE, span = 0.5, method = "loess") +
  scale_colour_manual(values = stroke_palette) +
  facet_wrap(~county) +
  theme(legend.position = "none",
        panel.grid.major = element_line(colour = "#0f3081")) +
  labs(title = title,
       subtitle = "Improved visual presentation showing original data and trend, and using full period of data available.",
       x = "",
       y = "New cases per day",
       caption = the_caption)

svg_png(p1, "../img/0184-bad-bar", w = 9)
svg_png(p2, "../img/0184-good-line")
