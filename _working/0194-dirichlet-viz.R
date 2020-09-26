library(tidyverse)
library(ggtern)
library(gtools)

d <- expand.grid(p1 = 1:99 /100, p2 = 1:99 / 100) %>%
  mutate(p3 = 1 - p1 - p2) %>%
  filter(p3 >= 0.01) 

alpha = c(1, 2, 0.5) 
d$dens <- ddirichlet(d[, 1:3], alpha = alpha)

d %>%
  ggplot(aes(x = p1, y = p2, z = p3, weight = dens, fill = ..level..)) +
  coord_tern() +
  theme_custom(tern.plot.background = "white", tern.panel.background = "white") +
  theme_hidetitles() +
  theme_showarrows() +
  theme(legend.position = "none") +
  stat_density_tern(geom = 'polygon', contour = TRUE) +
  scale_fill_gradient2(low = "white", high = "black") +
  scale_T_continuous(labels = 0:5 * .20) +
  scale_L_continuous(labels = 0:5 * .20) +
  scale_R_continuous(labels = 0:5 * .20) +
  theme(axis.title.x = element_blank())


d %>%
  as_tibble() %>%
  arrange(desc(dens))


data('Feldspar')
head(Feldspar)
