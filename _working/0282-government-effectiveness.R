library(WDI)
library(tidyverse)
library(Cairo)

WDIsearch("effectiveness") |>
  as_tibble()

d <- WDI(indicator = "GE.EST") |>
  as_tibble()

picts <- c("Vanuatu", "Fiji", "Solomon Islands",
           "Micronesia, Fed. Sts.", "Marshall Islands",
           "Samoa", "Papua New Guinea", "Kiribati",
           "Tonga", "French Polynesia", "Cook Islands",
           "Niue", "Tokelau", "Tuvalu", "Palau",
           "Nauru", "New Caledonia", "Wallis and Futuna",
           "Northern Mariana Islands", "Guam", "American Samoa", "Pitcairn")
stopifnot(length(picts) == 22)

d2 <- d |>
  group_by(year) |>
  summarise(GE.EST = mean(GE.EST, na.rm = TRUE)) |>
  drop_na() |>
  mutate(country = "World average")

picts[!picts %in% d$country]

unique(d$country)
#CairoWin()
d |>
  filter(country %in% picts) |>
  drop_na() |>
  mutate(country = fct_reorder(country, GE.EST, .fun = mean)) |>
  ggplot(aes(x = year, y = GE.EST)) +
  facet_wrap(~country, ncol = 4) +
  geom_hline(yintercept = 0, colour = "orange") +
  geom_line(colour = "steelblue") +
  labs(title = "Government effectiveness in the Pacific",
       subtitle = "World Bank Government Effectiveness Index, countries listed in order of average value",
       y = "Index value (0 = average)",
       x = "")
