#==================setup==================
library(tidyverse)
library(ggtext)
library(ggrepel)


# some general graphics parameters:
inc_col <- "red"
pop_col <- "blue"

theme_set(
  theme_minimal(base_family = "Roboto") +
    theme(
      panel.grid.minor = element_blank(),
      plot.subtitle = element_markdown(),
      plot.caption = element_markdown(colour = "grey50"),
      plot.title = element_markdown(family = "Sarala")
    )
)


#==============Quesnay France 1763=====================

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


#-------------dual axis bar and line chart-----------------
# as per Milanovic's style

p1 <- d1 |> 
  ggplot(aes(x = class_detail)) +
  # we want the 'gridlines' to be coloured, match the values, and behind the columns:
  geom_hline(yintercept = c(0, 1/2.7, d1$inc_prop), colour = inc_col, alpha = 0.1) +
  geom_hline(yintercept = c(0, d1$pop_prop), colour = pop_col, alpha = 0.1) +
  # columns for population:
  geom_col(aes(y = pop_prop), fill = pop_col, alpha = 0.7) +
  # lines and points for income:
  geom_line(aes(x = as.numeric(class_detail), y = inc_prop), colour = inc_col) +
  geom_point(aes(x = as.numeric(class_detail), y = inc_prop), colour = inc_col) +
  # annotated labels, also colour coded:
  annotate("text", x = 4.2, y = 0.8, label = "Relative income (right axis)", colour = inc_col, hjust = 0) +
  annotate("text", x = 1.6, y = 0.6, label = "Population share (left axis)", colour = pop_col, hjust = 0) +
  # two different sets of labels for the different variables:
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.1),
                     breaks = c(0, d1$pop_prop), labels = c(0, d1$population),
                     sec.axis = dup_axis(breaks = c(0, 1/2.7, d1$inc_prop), 
                                         labels = c(0, "1.0", d1$income), 
                                         name = "Income relative to the mean (1.0)")) +
  labs(x = "",
       y = "Percentage of poulation",
       title = "Contemporary understanding of income inequality in France in the time of Louis XV",
       subtitle = "Class-based income distribution in *La philosophie rurale* by Mirabeau and Quesnay, 1763. Gini estimated to be between 49 and 55.",
       caption = "Quesnay's original estimates, reproduced in Table 1.1 of Milanovic's *Visions of Inequality*, and plot style adapted from Milanovic's.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        axis.line.y.right = element_line(colour = inc_col),
        axis.line.y.left = element_line(colour = pop_col),
        axis.title.y.left = element_text(colour = pop_col),
        axis.title.y.right = element_text(colour = inc_col),
        axis.text.y.left = element_text(colour = pop_col),
        axis.text.y.right = element_text(colour = inc_col))

svg_png(p1, "../img/0311-quesnay-bar", w = 10, h = 6)
        
#---------------scatter plot--------------

p2 <- d1 |> 
  ggplot(aes(x = population, y = income, label = class_detail)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey80") +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  annotate("text", x = 30, y = 1.1, label = "Average income", colour = "grey80") +
  scale_x_continuous(breaks = c(0, d1$population), limits = c(0, 50), expand = c(0,0 )) +
  scale_y_continuous(breaks = c(1, d1$income), limits = c(0, 3), expand = c(0,0 )) +
  labs(y = "Income relative to the mean",
     x = "Percentage of population",
     title = "Contemporary understanding of income inequality in France in the time of Louis XV",
     subtitle = "Class-based income distribution in *La philosophie rurale* by Mirabeau and Quesnay, 1763. Gini estimated to be between 49 and 55.",
     caption = "Quesnay's original estimates, reproduced in Table 1.1 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))

svg_png(p2, "../img/0311-quesnay-scatter", w = 10, h = 6)

#==============Adam Smith's time 1759==========

# Allen, Robert C. “Class Structure and Inequality during the Industrial
# Revolution: Lessons from England’s Social Tables, 1688-1867.” The Economic
# History Review 72, no. 1 (2019): 88–125.

#page 105 of Allen for % ofpopulation but I am using Milanovic's labels from his Figure 2.1
# page 106 for income in pounds

d2 <- tribble(~population, ~income, ~class,
              1.5, 452.78, "Landed aristocracy",
              4.2, 145.37,  "Capitalists",
              9.4, 27.17, "Shop owners",
              18.9,  21.57, "Peasants",
              56.4, 13.58, "Workers",
              9.6, 3.62  , "Paupers"
              ) |> 
  mutate(year = 1759)

avg_inc <- 23.14
# note, not the same as:
mean(d2$income)


bry <- round(d2$income)[c(1:3, 6)]
brx <- c(0, round(d2$population, 1))[c(1:6)]

p3 <- d2 |> 
  ggplot(aes(x = population, y = income, label = class)) +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  scale_x_continuous(breaks = brx, limits = c(0, 65), expand = c(0, 0)) +
  scale_y_continuous(breaks = bry, limits = c(0, 800), expand = c(0, 0)) +
  labs(y = "Income in pounds",
       x = "Percentage of population",
       title = "Modern understanding of income inequality in England and Wales in 1759",
       subtitle = "Average income by earner in pounds per year, as estimated in 2019. Gini index between 45 and 51.",
       caption = "Robert Allen, “Class Structure and Inequality during the Industrial
Revolution: Lessons from England’s Social Tables, 1688-1867.”<br>*The Economic
History Review 72*, no. 1 (2019): 88–125, reproduced in Figure 2.1 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))

svg_png(p3, "../img/0311-smith-scatter", w = 10, h = 6)


p4 <- d2 |> 
  ggplot(aes(x = population, y = income, label = class)) +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  scale_x_continuous(breaks = brx, limits = c(0, 65), expand = c(0, 0)) +
  scale_y_log10(breaks = round(d2$income),  limits = c(1, 800), expand = c(0, 0)) +
  labs(y = "Income in pounds (log scale)",
       x = "Percentage of population",
       title = "Modern understanding of income inequality in England and Wales in 1759",
       subtitle = "Average income by earner in pounds per year, as estimated in 2019. Gini index between 45 and 51.",
       caption = "Robert Allen, “Class Structure and Inequality during the Industrial
Revolution: Lessons from England’s Social Tables, 1688-1867.”<br>*The Economic
History Review 72*, no. 1 (2019): 88–125, reproduced in Figure 2.1 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))

svg_png(p4, "../img/0311-smith-scatter-log", w = 10, h = 6)

#--------------------time of ricardo-------------

d3 <- tribble(~population, ~income, ~class,
              1.3, 756, "Landed aristocracy",
              3.2, 525,  "Capitalists",
              8.6, 65, "Shop owners",
              10.8,  49, "Peasants",
              61.1, 23, "Workers",
              14.9, 4  , "Paupers"
) |> 
  mutate(year = 1801)

bry <- round(d3$income)[c(1:6)]
brx <- c(0, round(d3$population, 1))[c(1:7)]

p5 <- d3 |> 
  ggplot(aes(x = population, y = income, label = class)) +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  scale_x_continuous(breaks = brx, limits = c(0, 65), expand = c(0, 0)) +
  scale_y_continuous(breaks = bry, limits = c(0, 800), expand = c(0, 0)) +
  labs(y = "Income in pounds",
       x = "Percentage of population",
       title = "Modern understanding of income inequality in England and Wales in 1801",
       subtitle = "Average income by earner in pounds per year, as estimated in 2019. Gini index of around 52.",
       caption = "Robert Allen, *Revising England’s Social Tables Once Again* 2016, reproduced in Table 3.1 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))

svg_png(p5, "../img/0311-ricardo-scatter", w = 10, h = 6)


#-----------------------1831 France--------------
# From Milanovic's Marx chapter


d4 <- tribble(~employment, ~income, ~class,
              3.4, 8.6, "Employers",
              5.1, 3, "Large farmers",
              1.1, 1.8, "High-level civil servants",
              13.9, 1, "Blue-collar employees",
              2, 0.9, "White collar employees",
              13.4, 0.7, "Self-employed",
              1.1, 0.6, "Low-level civil servants",
              31.4, 0.5, "Small farmers",
              28.5, 0.45, "Agricultural workers and servants"
) |> 
  mutate(year = 1831)



bry <- sort(round(d4$income, 1))[c(1,3,6:9)]
brx <- sort(c(0, round(d4$employment, 1)))[c(1:2, 3,4,5,6,8,9, 10)]

p6 <- d4 |> 
  ggplot(aes(x = employment, y = income, label = class)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey80") +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue", seed = 123, hjust = 0) +
  scale_x_continuous(breaks = brx, limits = c(0, 35), expand = c(0, 0)) +
  scale_y_continuous(breaks = bry, limits = c(0, 10), expand = c(0, 0)) +
  labs(y = "Relative income (average = 1.0)",
       x = "Percentage of employed persons",
       title = "Modern understanding of income inequality in France in 1831",
       subtitle = "Average income by earner relative to overall mean.",
       caption = "Christian Morrison, and Wayne Snyder. “The Income Inequality of France in Historical Perspective.” 
       <br>*European Review of Economic History* 4, no. 1 (2000): 59–83.
, reproduced in Table 4.4 of Milanovic's *Visions of Inequality*.") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "grey80"))

svg_png(p6, "../img/0311-marx-scatter", w = 10, h = 6)
