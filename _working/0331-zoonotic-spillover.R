library(tidyverse)
library(janitor)

# Concentric is the biosecurity and public health arm of Ginkgo Bioworks, a US
# biotechnology company. It has worked with organisations including the USDA,
# Texas A&M, Penn State and CGIAR/ILRI on pathogen surveillance and zoonotic
# disease monitoring projects.

# In their paper they argue twothings
# spillover events are becoming more frequent;
# spillover events are becoming more severe.

# this data is only ell known, larger spillover events
url <- "https://raw.githubusercontent.com/concentricbyginkgo/zoonotic_spillover_trend/master/data/spillover_data.csv"

spillover <- read_csv(url, show_col_types = FALSE) |>
  clean_names()

count(spillover, pathogen)

spillover |>
  ggplot(aes(x = event_start_year, y = reported_deaths)) +
  facet_wrap(~pathogen) +
  geom_point() +
  scale_y_sqrt()

spillover_annual <- spillover |>
  group_by(event_start_year) |>
  summarise(deaths = sum(reported_deaths), events = n()) |>
  right_join(
    tibble(event_start_year = 1963:max(spillover$event_start_year)),
    by = "event_start_year"
  ) |>
  mutate(
    deaths = replace_na(deaths, 0),
    events = replace_na(events, 0),
    no_obs = ifelse(deaths == 0, "No observed events", "At least one event")
  )

spillover_annual |>
  ggplot(aes(x = event_start_year, y = deaths)) +
  geom_smooth(method = "gam", colour = "black") +
  geom_point(aes(colour = no_obs)) +
  scale_y_sqrt() +
  labs(
    y = "Reported deaths (sqrt-transformed scale)",
    caption = "Source: Concentric, Ginkgo Bioworks",
    colour = ""
  )
# what if these was redone as deaths as a proportion of the growing world population?


spillover_annual |>
  ggplot(aes(x = event_start_year, y = events)) +
  geom_smooth(method = "gam", colour = "black") +
  geom_point(aes(colour = as.logical(deaths == 0), size = deaths)) +
  labs(
    y = "Reported deaths (sqrt-transformed scale)",
    colour = "",
    caption = "Source: Concentric, Ginkgo Bioworks"
  )

# Copilot says (rightly): That immediately raises the question: did zoonotic
# spillovers suddenly start increasing around 1990, or did surveillance, case
# definitions, laboratory diagnostics and international reporting systems
# improve around that time?The second explanation seems quite plausible.

# If fitting this independently, confidence would be much higher in the statement:
# "the historical record of documented high-consequence zoonotic outbreaks shows an upward trend"
# than in
# "the true rate of zoonotic spillover has been demonstrated to be increasing."
