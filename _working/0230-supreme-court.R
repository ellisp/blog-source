

library(tidyverse)
library(rvest)
library(lubridate)
library(scales)
library(glue)
library(patchwork)
library(kableExtra)
library(mgcv)

url <- "https://www.senate.gov/legislative/nominations/SupremeCourtNominations1789present.htm"

the_caption = glue("\n\n\nAnalysis by freerangestats.info copying analysis from fivethirtyeight; data from {url}")

d <- read_html(url)

d2 <- html_table(d)[[1]]

d3 <- d2[-(1:5), ] %>%
  filter(X2 == "" & X1 != "") %>%
  select(nominee = X1,
         to_replace = X3,
         nominated_date = X5,
         vote = X7,
         result = X9) %>%
  # add result labels:
  mutate(result = case_when(
    result == "C" ~ "Confirmed",
    result == "D" ~ "Declined",
    result == "N" ~ "No action",
    result == "P" ~ "Postponed",
    result == "R" ~ "Rejected",
    result == "W" ~ "Withdrawn",
    TRUE ~ "Not yet decided"
  )) %>%
  # remove footnotes:
  mutate(nominee = gsub("[0-9]", "", nominee)) %>%
  # fix dates
  mutate(nominated_date = mdy(nominated_date)) %>%
  # clean up vote count
  separate(vote, sep = "-", into = c("vote_for", "vote_against"), remove = FALSE, fill = "right") %>%
  mutate(vote_for = as.numeric(str_extract(vote_for, "[0-9]+")),
         vote_against = as.numeric(str_extract(vote_against, "[0-9]+"))) %>%
  # if the original vote was by voice, we call that 1-0
  mutate(vote_for = if_else(vote == "V", 1, vote_for),
         method = case_when(
           vote == "V" ~ "Voice", 
           result == "Withdrawn" ~ "Withdrawn",
           TRUE ~ "Vote"),
         result_lumped = if_else(result == "Confirmed", "Confirmed", "Other"),
         denominator = replace_na(vote_for, 0) + replace_na(vote_against, 0),
         prop_for = vote_for / denominator,
         prop_including_withdrawn = ifelse(is.na(prop_for) & result == "Withdrawn", 0, prop_for))

count(d3, result) %>% kable() 

p0 <- d3  %>%
  ggplot(aes(x = nominated_date, y = prop_for)) +
  geom_point(aes(colour = result_lumped, shape = method), size = 2) +
  labs(colour = "Result:",
       shape = "Method:",
       x = "Nomination date",
       y = "Proportion voting for confirmation",
       title = "Changing average vote for Supreme Court justice nominations") +
  scale_y_continuous(label = percent) 

p1 <- p0 +
  geom_smooth(method = "gam", se = FALSE, colour = "grey50", size = 2) +
  labs(subtitle = "Generalized additive model")
  

p1a <- p1 + aes(y = prop_including_withdrawn) + labs(y = "Proportion voting for confirmation (withdrawn = 0%)") 

p2 <- p0 +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 3)", se = FALSE, colour = "grey50", size = 2) +
  labs(subtitle = "Cubic polynomial regression")

p2a <- p2 + aes(y = prop_including_withdrawn) + labs(y = "Proportion voting for confirmation (withdrawn = 0%)")

p3 <- p0 +
  geom_smooth(method = "loess", se = FALSE, colour = "grey50", size = 2) +
  labs(subtitle = "LOESS")

p3a <- p3 + aes(y = prop_including_withdrawn) + labs(y = "Proportion voting for confirmation (withdrawn = 0%)")


svg_png(
  p1 + 
    p3 + labs(title = "") + theme(legend.position = "none")  + 
    p2 + labs(title = "", caption = the_caption) + theme(legend.position = "none"),
  file = "../img/0230-as-original", w = 19, h = 6)

svg_png(
  p1a + 
    p3a + labs(title = "Things don't change much if we include 'withdrawn' as 0%") + theme(legend.position = "none")  + 
    p2a + labs(title = "", caption = the_caption) + theme(legend.position = "none"),
  file = "../img/0230-with-withdrawn", w = 19, h = 6)

svg_png(p1a, file = "../img/0230-gam-alone-with-withdrawn")

model <- gam(prop_for ~ s(as.numeric(nominated_date)), data = d3)

