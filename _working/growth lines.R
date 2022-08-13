library(tidyverse)

d <- tibble(id = 1:100,
            `2020` = rpois(100, 10) + 5,
            `2025` = rpois(100, 11) + 6)

years_wanted <- expand_grid(id = d$id,
                            year = 2021:2024)

d_with_interp <- d %>%
  pivot_longer(cols = `2020`:`2025`, names_to = "year") %>%
  mutate(year = as.integer(year)) %>%
  bind_rows(years_wanted) %>%
  group_by(id) %>%
  mutate(growth = (value[year == 2025] / value[year == 2020]) ^ (1/5),
         steady_growth_value = value[year == 2020] * growth ^ (year - 2020),
         linear_growth_value = (value[year == 2025] - value[year == 2020]) / 5 * (year - 2020) + value[year == 2020]) %>%
  ungroup()


head(d_with_interp)
tail(d_with_interp)

set.seed(123)
d_with_interp %>%
  filter(id %in% sample(1:100, 9)) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = steady_growth_value)) +
  geom_line(aes(y = linear_growth_value), colour = "blue", alpha = 0.5) +
  geom_point(aes(y = value)) +
  facet_wrap(~id)
