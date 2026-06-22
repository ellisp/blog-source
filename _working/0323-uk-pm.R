library(rvest)
library(tidyverse)
library(janitor)
library(slider) # for rolling sum
library(scales)
library(ggrepel)

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
the_title <- "Prime ministers of the United Kingdom, 1721 to 2026"

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

svg_png(p1, "../img/0323-gantt", w = 10, h = 6)


p2 <- pm_table |> 
  ggplot(aes(x = duration)) +
  geom_density(colour = "steelblue") +
  geom_rug(colour = "steelblue") +
  scale_x_continuous(label = comma) +
  labs(x = "Duration in days",
       title = the_title)

svg_png(p2, "../img/0323-density", w = 10, h = 6)


p3 <- pm_table |> 
  ggplot(aes(x = start, y = duration)) +
  geom_smooth(method = "gam", colour = "white") +
  geom_point(colour = "steelblue") +
  geom_text_repel(data = filter(pm_table, duration < 120 | duration > 3000), 
                   aes(label = pm), size = 2.8) +
  scale_y_sqrt(breaks = c(0.5, 1, 1:4 * 2) * 1000, label = comma) +
  labs(x = "Starting date of premiership",
       y = "Duration in days",
       title = the_title,
      subtitle = "Durations shown are of individual periods in office, not lifetime totals.")

svg_png(p3, "../img/0323-over-time", w = 10, h = 6)


cumulative_pms <- pm_table |> 
  full_join(tibble(start = seq(from = min(pm_table$start), 
                               to = max(pm_table$end), 
                               by = "1 day"))) |> 
  arrange(start) |> 
  mutate(starting_pms = if_else(is.na(pm), 0 , 1),
         rolling_pms = pmax(1, slide_sum(starting_pms, before = 3653)))


p4 <- cumulative_pms |> 
  ggplot(aes(x = start, y = rolling_pms)) +
  geom_line(colour = "steelblue") +
  scale_y_continuous(breaks = 0:max(cumulative_pms$rolling_pms)) +
  labs(x = "",
       y = "Number of prime ministers in past 10 years",
       title = the_title,
       subtitle = "Peak prime ministers per decade was in the 1830s")

svg_png(p4, "../img/0323-rolling-pms", w = 10, h = 6)

arrange(cumulative_pms, desc(rolling_pms))

