library(tidyverse)
library(ggrepel)
library(scales)

# data fro https://www.abc.net.au/news/2021-08-27/how-australia-could-become-world-most-covid-vaccinated-country/100410504

the_caption = "Source: ABC News; data collected from Our World in Data and Department of Health and Ageing"

d <- tibble(location = 
         c("Malta", 
            "United Arab Emirates", 
            "Singapore", 
            "Uruguay", 
            "Denmark", 
            "Qatar", 
            "Chile", 
            "Belgium", 
            "Spain", 
            "Portugal", 
            "Spain", 
            "Canada", 
            "Australia", 
            "NSW",
           "NSW 90% target"),
       `% fully vaccinated` = 
         c(79.7, 74.3, 74.3, 71.5, 70.1, 69.6, 69.6, 68.4, 67.7, 67.6, 67.3, 
           66.1, 24.8, 26.5, 72), 
       `Population (millions)` = c(0.5, 9.9, 5.9, 3.5, 5.8, 2.9, 19.1, 11.6, 46.8, 10.2, 
                      46.8, 37.7, 25.5, 8.2, 8.2)
) %>%
  # remove one duplicate location, presumably an old observation:
  filter(!(location == "Spain" & round(`% fully vaccinated`, 1) == 67.3)) %>%
  mutate(location = factor(location, levels = location),
         location = fct_rev(location)) %>%
  mutate(is_australia = ifelse(location %in% c("Australia", "NSW", "NSW 90% target"),
                               "Australia",
                               "Other"))
 
# Replicate the ABC's chart

p1 <- d %>%
  gather(variable, value, -location, -is_australia) %>%
  mutate(labx = ifelse(value > 5, 1.5, value + 1),
         labcol = ifelse(value > 5, "white", "grey")) %>%
  ggplot(aes(x = value, y = location)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_col(fill = "grey97", aes(x = pmax(value, Inf))) +
  geom_col(width = 0.7, aes(fill = variable)) +
  geom_text(aes(x = labx,
                colour = labcol,
                label = round(value, 2)), 
            hjust = 0, 
            size = 2.8,
            family = "Roboto") +
  theme_minimal(base_family = "Roboto") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0),
        strip.text = element_text(hjust = 0.05),
        plot.title.position = "plot",
        plot.caption = element_text(colour = "grey50")) +
  scale_colour_manual(values = c(
    white = "white", grey = "grey30"
  ), guide = "none") +
  scale_fill_manual(values = c(
    `% fully vaccinated` = "darkslateblue",
    `Population (millions)` = "steelblue"
  ), guide = "none") +
  labs(x = "", 
       y = "",
       caption = the_caption,
       title = "Malta leads the world in vaccination rates.",
       subtitle = "Proportion of total population fully vaccinated to date.")


# Alternative scatterplot

p2 <- d %>%
  ggplot(aes(x = `Population (millions)`, y = `% fully vaccinated`, label = location, colour = is_australia)) +
  geom_point() +
  geom_text_repel(seed = 123, size = 3) +
  scale_y_continuous(label = percent_format(scale = 1)) +
  scale_colour_manual(values = c(Australia = "darkslateblue",
                                 Other = "steelblue"), guide = "none") +
  labs(title = "Malta leads the world in vaccination rates.",
       subtitle = "'NSW 905% target' refers to 90% of adults; vertical axis shows percentage of full population.",
       caption = the_caption)

svg_png(p1, "../img/0203-bars")
svg_png(p2, "../img/0203-scatter")

