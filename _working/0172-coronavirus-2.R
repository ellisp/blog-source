#------------setup---------------

# 59% cases male
20686 / (20686 + 14378)

# 71% deaths men (no boys)
2139 / (2139 + 890)

library(tidyverse)
library(scales)
library(wpp2019)

# colours for male and female used by Washington Post 2017; see https://blog.datawrapper.de/gendercolor/
sex_cols <- c(Male = "#F4BA3B", Female =  "#730B6D")


#---------------------Italian fatality rates---------

italy_rates <-tibble(
  age_grp = rep(c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+'), 2),
  sex = rep(c("Male", "Female"), each = 10),
  cfr = c(0, 0, 0, 0.6, 0.7,   1.7, 6.0, 17.8, 26.4, 32.5,
          0, 0, 0, 0.2,   0.4, 0.6, 2.8,  10.7, 19.1,   22.3) / 100,
  age_midpt = rep(c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95), 2)
)


p1 <- italy_rates %>%
  ggplot(aes(x = age_midpt, y = cfr, colour = sex)) +
  geom_point() +
  geom_text(data = filter(italy_rates, cfr > 0.01),
            aes(label = percent(cfr), y = cfr + 0.012), size = 3) +
  geom_line() +
  scale_x_continuous(breaks = italy_rates$age_midpt, labels = italy_rates$age_grp) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_colour_manual(values = sex_cols) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Age group", colour = "", y = "Observed case fatality rate",
       title = "Observed fatality rate of diagnosed COVID-19 cases in Italy to 19 March 2020",
       subtitle = "20,686 men and boys with case fatality rate of 10.3%; 14,378 women and girls with case fatality rate of 6.2%",
       caption = "Source: Istituto Superiore di Sanità, Roma")

svg_png(p1, "../img/0172-italy-rates", w = 8, h = 5)


#----------------Population rates ------------------

data(popF)
data(popM)

selected_countries <- c("Australia", "Italy", "Timor-Leste", "United States of America", "World",
                        "China", "Brazil", "Japan", "Germany")

age_lu <- tibble(age = unique(popF$age),
                 age_grp = c(rep(unique(italy_rates$age_grp), each = 2), "90+")) %>%
  mutate(age_grp = factor(age_grp, levels = unique(age_grp)))

# Visual check that this shorthand worked ok
# View(age_lu)

pop_2020 <- popF %>%
  mutate(sex = "Female") %>%
  rbind(mutate(popM, sex = "Male")) %>%
  select(country = name, age, pop = `2020`, sex) %>%
  left_join(age_lu, by = "age") %>%
  group_by(country, age_grp, sex) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  filter(country %in% selected_countries) %>%
  mutate(country = fct_drop(country)) %>%
  group_by(country) %>%
  mutate(prop = pop / sum(pop)) %>%
  ungroup()

# check no misspellings in countries
stopifnot(sum(!selected_countries %in% unique(pop_2020$country)) == 0)

p2 <- pop_2020 %>%
  ggplot(aes(x = as.numeric(age_grp), y = prop, colour = sex)) +
  geom_line() +
  facet_wrap(~country) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:10, labels = levels(pop_2020$age_grp)) +
  scale_colour_manual(values = sex_cols) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Age group",
       y = "",
       colour = "",
       title = "Estimated proportion of the population in 2020",
       subtitle = "By age group and sex",
       caption = "Source: UN World Population Prospects 2019")

svg_png(p2, "../img/0172-country-pops", w = 8, h = 6)


#----------Combine fatality rate with population--------------------

the_caption = "Source: Italian case fatality rates to 19 March 2020 from Istituto Superiore di Sanità, Roma, combined with UN World Population Prospects 2019"

projected_cfr <- pop_2020 %>%
  mutate(age_grp = as.character(age_grp)) %>%
  left_join(italy_rates, by = c("age_grp", "sex")) %>%
  group_by(country) %>%
  summarise(cfr = sum(cfr * prop) /  sum(prop)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, -cfr))

xlabel <- "Case fatality rate if rates observed in Italy applied to each country's total age and sex profile.\n
Do not treat these as forecasts of actual case fatality rate."

p3 <- projected_cfr %>%
  ggplot(aes(y = country, x = cfr)) +
  geom_point(colour = "steelblue") +
  geom_text(aes(label = percent(cfr, accuracy = 0.1)), nudge_x = 0.0015, size = 3) +
  geom_segment(aes(yend = country, xend = 0), colour = "steelblue") +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(subtitle = xlabel,
       y = "",
       title = "Different age profiles can make a big difference to overall fatality rates, based on Italian data",
       x = "Note that in observed situations (eg Italy 8.5% to 19 March 2020), raw case fatality rates are more than double
those shown here, suggesting younger and female cases are either not diagnosed or not occurring.",
       caption = the_caption)

svg_png(p3, "../img/0172-country-rates", w = 10, h = 5)


p4 <- projected_cfr %>%
  mutate(cfr_adj = cfr / cfr[country == "Italy"] * 0.085) %>%
  ggplot(aes(y = country, x = cfr_adj)) +
  geom_point(colour = "steelblue") +
  geom_text(aes(label = percent(cfr_adj, accuracy = 0.1)), nudge_x = 0.003, size = 3) +
  geom_segment(aes(yend = country, xend = 0), colour = "steelblue") +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(subtitle = xlabel,
       y = "",
       title = "Different age profiles can make a big difference to overall fatality rates, based on Italian data",
       x = "Estimates have been scaled to match Italy's raw case fatality rate to 19 March, to
reflect likely patterns in younger people's case rate and diagnosis.",
       caption = the_caption)

svg_png(p4, "../img/0172-country-rates-adj", w = 10, h = 5)

