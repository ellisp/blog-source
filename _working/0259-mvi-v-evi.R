library(tidyverse)
library(readxl)
library(janitor)

download.file("https://www.un.org/development/desa/dpad/wp-content/uploads/sites/45/LDC_data.xlsx",
              destfile = "LDC_data.xlsx", mode = "wb")


ldc <- read_excel("LDC_data.xlsx", sheet = "2021") |>
  clean_names() |>
  filter(!is.na(ccode)) |>
  rename(country = countries_indicators) |>
  mutate(is_pict = country %in% picts)

picts <- c(
  "Fiji",
  "Micronesia (Federated States of)",
  "Kiribati",
  "Marshall Islands",
  "Nauru",
  "Palau",
  "Papua New Guinea",
  "Samoa",
  "Solomon Islands",
  "Tonga",
  "Tuvalu",
  "Vanuatu"
)

stopifnot(all(picts %in% ldc$country))

# EVI is half the Economic vulnerability index and half the environmental ulnerability index

ldc |>
  group_by(is_pict) |>
  summarise(median(evi))

ldc |>
  summarise(median(evi))

ldc |>
  mutate(is_pict = country %in% picts) |>
  filter(is_pict) |>
  select(country, evi, economic_vulnerability_index, environmental_vulnerability_index) |>
  arrange(desc(evi))

ldc |>
  ggplot(aes(x = economic_vulnerability_index, y = environmental_vulnerability_index, colour = is_pict)) +
  geom_vline(xintercept = median(ldc$economic_vulnerability_index), colour = "grey80") +
  geom_hline(yintercept = median(ldc$environmental_vulnerability_index), colour = "grey80") +
  geom_point() +
  geom_text_repel(data = filter(ldc, is_pict), aes(label = country), alpha = 0.5) +
  scale_colour_manual(values = c("grey", "blue")) +
  theme(panel.grid = element_blank())


d <- mvi   |>
  select(country = Country, new_mvi = `MVI - Score`) |>
  full_join(select(ldc, country, evi, economic_vulnerability_index, environmental_vulnerability_index, is_pict), 
            by = "country") |>
  drop_na()

library(Cairo)
CairoWin()

#------------find a line that divides the data into two---------------
# We want a backwards diagonal line that goes through the medians and has
# half the countries on each side - so we can split into those that are
# 'more poor than vulnerable' and vice versa
center <- (c(median(d$evi, na.rm = TRUE), median(d$new_mvi, na.rm = TRUE)))

test_slope <- function(b){
  a <- center[2] - b * center[1]
  more_poor <- mean((a + b * d$evi) < d$new_mvi)
  return(abs(0.5 - more_poor))
}
test_slope(1)

# optim doesn't work here so we just use brute force, try a big range of plausible vlaues
tests <- tibble(possible_slopes = seq(from = 0.5, to = 2, length.out = 1000)) |>
  mutate(disc = Vectorize(test_slope)(possible_slopes)) |>
  arrange(disc)

# get the actual slope and intercept that work best
b <- tests[1, ]$possible_slopes
a <- center[2] - b * center[1]

d |>
  ggplot(aes(x = evi, y = new_mvi, colour = is_pict)) +
  geom_abline(intercept = a, slope = b) +
  geom_vline(xintercept = median(d$evi, na.rm = TRUE), colour = "grey80") +
  geom_hline(yintercept = median(d$new_mvi, na.rm = TRUE), colour = "grey80") +
  geom_point() +
  geom_text_repel(data = filter(d, is_pict), aes(label = country), alpha = 0.5) +
  scale_colour_manual(values = c("grey", "blue")) +
  theme(panel.grid = element_blank()) +
  labs(x = "Existing Economic and environmental vulnerability index",
       y  = "New proposed multidimensional vulnerability index")

names(ldc)
