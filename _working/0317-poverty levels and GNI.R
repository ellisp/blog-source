# This is an exploratory plot comparing poverty headcount at absolute poverty levels with GNI
# I don't think this has ever been used for anything

source("setup.R")
library(WDI)

# What 
# WDIsearch("gni") |> View()

# Poverty headcount ratio at $5.50 a day (2011 PPP) (%
hc <- WDI(indicator = c(gni = "NY.GNP.MKTP.PC.CD", pov = "SI.POV.UMIC")) |>
  as_tibble()

# we don't have any years where we have both gni and poverty headcount so have
# to get the latest GNI and match it on country, ignoring year
latest_gni <- hc |>
  filter(!is.na(gni)) |>
  group_by(country) |>
  arrange(desc(year)) |>
  slice(1) |>
  ungroup() |>
  select(country, gni)

hc |>
  select(-gni) |>
  left_join(latest_gni, by = "country") |>
  drop_na() |>
  group_by(country) |>
  arrange(desc(year)) |>
  slice(1) |>
  ungroup() |>
  ggplot(aes(x = gni, y = pov)) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_point() +
  # not sure what this red point is! some arbitrary country I wanted to compare I think ti see if its
  # people in poverty was what we'd expect from gdp
  geom_point(data = tibble(gni = 10144 * .67, pov = 21.5), size = 3, colour = "red") +
  scale_x_log10(label = dollar) +
  scale_y_continuous(label = percent_format(scale = 1)) +
  labs(x = "Latest value of GNI per capita (logarithmic scale)",
       y = "Latest value of proportion of people at $5.50 a day or less",
       title = "Income per capita and basic needs poverty headcount") +
  theme_bw()
  
