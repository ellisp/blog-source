library(palmerpenguins)
library(tidyverse)
library(scales)

penguins2 <- drop_na(penguins) %>%
  mutate(row_id = 1:n())

m1 <- penguins2 %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) 

pc1 <- prcomp(m1, scale = TRUE)
penguins2$pc1 <- rescale(predict(pc1)[, 1])

p1 <- penguins2 %>%
  ggplot(aes(x = body_mass_g, y = pc1, colour = species)) +
  geom_point() +
  labs(x = "Body mass in grams",
       y = "First principal component\n(rescaled from zero to 1)",
       caption = "Example data: Palmer Penguins",
       colour = "Species:",
       title = "First principal component of four-dimensional data",
       subtitle = "Fit to data in its original row order")


set.seed(123)
m2 <- penguins2 %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, row_id) %>%
  sample_n(n())

pc2 <- prcomp(select(m2, -row_id), scale = TRUE)

penguins3 <- m2 %>%
  mutate(pc2 = rescale(predict(pc2)[, 1])) %>%
  select(row_id, pc2) %>%
  inner_join(penguins2, by = "row_id") 


p2 <- penguins3 %>%
  ggplot(aes(x = body_mass_g, y = pc2, colour = species)) +
  geom_point() +
  labs(x = "Body mass in grams",
       y = "First principal component\n(rescaled from zero to 1)",
       caption = "Example data: Palmer Penguins",
       colour = "Species:",
       title = "First principal component of four-dimensional data",
       subtitle = "Fit to data in a different row order")

p2
p3 <- penguins3 %>%
  ggplot(aes(x = pc1, y = pc2, colour = species)) +
  geom_point() +
  labs(x = "When fit to data in original row order",
       y = "When fit to data in a different row order",
       title = "The direction of a principal component can vary based on the data's arrangement",
       subtitle = "Comparison of the first principal component, scaled from 0 to 1, fit to the same data",
       caption = "Example data: Palmer Penguins",
       colour = "Species:") +
  coord_equal() +
  theme(plot.title.position = "plot")


svg_png(p1, "../img/0202-pc1")
svg_png(p2, "../img/0202-pc2")  
svg_png(p3, "../img/0202-pc-comparison", w = 10, h = 6)  
