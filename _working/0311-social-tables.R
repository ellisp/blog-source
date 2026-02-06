
library(tidyverse)
library(ggtext)

d1 <- tribble(~population, ~income, ~class, ~class_detail,
              48, 0.5, "Workers",            "Agricultural labourers",
              22, 0.6, "Workers",            "Manufacturing low-skill workers",
              6, 0.8, "Self-employed",       "Self-employed in viticulture",
              4, 2.3, "Self-empoyed",        "Artisans and crafsmen in manufacturing",
              8, 2.7, "Capitalists",         "Tenant farmers",
              12, 2.3, "The Elite",          "Landlords, clergy, government administrators"
              ) |> 
  mutate(country = "France",
         period = "1763",
         class = factor(class,levels = unique(class)),
         class_detail = str_wrap(class_detail, 25),
         class_detail = factor(class_detail, levels = class_detail),
         pop_prop = population / max(population),
         inc_prop = income / max(income),
         pop_ratio = max(population),
         inc_ratio = max(income))


stopifnot(sum(d1$population) == 100)

inc_col <- "red"
pop_col <- "blue"

p1 <- d1 |> 
  ggplot(aes(x = class_detail)) +
  geom_col(aes(y = pop_prop), fill = pop_col, alpha = 0.5) +
  geom_line(aes(x = as.numeric(class_detail), y = inc_prop), colour = inc_col) +
  geom_point(aes(x = as.numeric(class_detail), y = inc_prop), colour = inc_col) +
  geom_hline(yintercept = c(0, 1/2.7, d1$inc_prop), colour = inc_col, alpha = 0.1) +
  geom_hline(yintercept = c(0, d1$pop_prop), colour = pop_col, alpha = 0.1) +
  annotate("text", x = 4.2, y = 0.8, label = "Relative income (right axis)", colour = inc_col, hjust = 0) +
  annotate("text", x = 1.6, y = 0.8, label = "Population share (left axis)", colour = pop_col, hjust = 0) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.1),
                     breaks = c(0, d1$pop_prop), labels = c(0, d1$population),
                     sec.axis = dup_axis(breaks = c(0, 1/2.7, d1$inc_prop), 
                                         labels = c(0, "1.0", d1$income), 
                                         name = "Income relative to the mean")) +
  labs(x = "",
       y = "Percentage of poulation",
       title = "Income inequality in France in the time of Louis XV",
       subtitle = "Factoral income distribution in *La philosophie rurale* by Mirabeau and Quesnay, 1763",
       caption = "Data from Table 1.1 of Milanovic's *Visions of Inequality*, and plot style adapted from the same book.") +
  theme_minimal(base_family = "Roboto") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.line.y.right = element_line(colour = inc_col),
        axis.line.y.left = element_line(colour = pop_col),
        axis.title.y.left = element_text(colour = pop_col),
        axis.title.y.right = element_text(colour = inc_col),
        axis.text.y.left = element_text(colour = pop_col),
        axis.text.y.right = element_text(colour = inc_col),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(colour = "grey50"),
        plot.title = element_markdown(family = "Sarala"))

svg_png(p1, "../img/0311-quesnay-bar", w = 10, h = 6)
        