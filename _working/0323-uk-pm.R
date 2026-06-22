library(rvest)
library(tidyverse)
library(janitor)
library(slider) # for rolling sum
library(scales)

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
  slice(-1) |> 
  # find the PMs' names - everything up to the first [
  mutate(pm = str_extract(pm , ".*?\\["),
         pm = str_replace(pm, "\\[", ""),
        ) |> 
  # strip all the ootnotes and stuff from the dates
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
 
pm_table

pm_table |> 
  distinct(pm) |> 
  pull(pm)

#--------------Draw plots-------------
the_title <- "Prime Ministers of the United Kingdom, 1721 to 2026"

p1 <- pm_table |> 
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

svg_png(p1, "../img/0323-gannt", w = 10, h = 6)


p2 <- pm_table |> 
  ggplot(aes(x = duration)) +
  geom_density() +
  geom_rug() +
  scale_x_continuous(label = comma) +
  labs(x = "Duration in days",
       title = the_title)

svg_png(p2, "../img/0323-density", w = 10, h = 6)


p3 <- pm_table |> 
  ggplot(aes(x = start, y = duration)) +
  geom_smooth(method = "gam") +
  geom_point() +
  scale_y_sqrt(breaks = c(0.5, 1, 1:4 * 2) * 1000, label = comma) +
  labs(x = "Starting date of premiership",
       y = "Duration in days",
       title = the_title)

svg_png(p3, "../img/0323-over-time", w = 10, h = 6)


cumulative_pms <- pm_table |> 
  mutate(start_year = year(start)) |> 
  count(start_year) |> 
  full_join(tibble(start_year = 1721:2026)) |> 
  mutate(n = replace_na(n, 0)) |> 
  arrange(start_year) |> 
  mutate(rolling_starts = slide_sum(n, before = 9),
         rolling_pms = ifelse(start_year > 1730, 
                              rolling_starts + 1, 
                              rolling_starts)) 



p4 <- cumulative_pms |> 
  ggplot(aes(x = start_year, y = rolling_pms)) +
  geom_line(colour = "grey80") +
  geom_point(size = 0.8) +
  scale_y_continuous(breaks = 0:max(cumulative_pms$rolling_pms)) +
  labs(x = "",
       y = "Number of prime ministers in past 10 years",
       title = the_title)

svg_png(p4, "../img/0323-rolling-pms", w = 10, h = 6)
