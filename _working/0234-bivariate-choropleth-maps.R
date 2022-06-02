
library(biscale)
library(absmapsdata)
library(Census2016.DataPack)
library(tidyverse)
library(scales)
library(janitor)
library(sf)
library(cowplot) # ggdraw etc
library(patchwork)
library(mgcv)
library(glue)

the_gcc <- "Greater Melbourne"

d <- sa22016 |>
  st_sf() |>
  rename(SA2_NAME16 = sa2_name_2016) |>
  filter(gcc_name_2016 == the_gcc) |>
  inner_join(SA2__medianRent, by = "SA2_NAME16") |>
  left_join(SA2__medianTotalPersonalIncome, by = "SA2_NAME16") |>
  clean_names() |>
  bi_class(x = median_total_personal_income, y = median_rent, style = "quantile", dim = 3)

blank_theme_elements <- theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid =  element_blank()
)

the_caption <- "Source: ABS, Census 2016"
the_title <- glue("Annual Income and Rent in {the_gcc}, Australia")

#-----------Bivariate choropleth approach-------------

bvm <- ggplot(d) +
  geom_sf(aes(fill = bi_class), 
          color = NA, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = the_title,
    caption = the_caption
  ) +
  blank_theme_elements
  

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Income ",
                    ylab = "Higher Rent ",
                    size = 8)

bvm_plot <- ggdraw() +
  draw_plot(bvm, 0, 0, 1, 1) +
  draw_plot(legend, 0.6, .75, 0.2, 0.2)

svg_png(bvm_plot, "../img/0234-bivariate", w = 11, h = 8)

#-----------------3 map and scatter plot approach--------------

dol <- dollar_format(scale = 1/1000, suffix = "k")

m1 <- ggplot(d) +
  geom_sf(aes(fill = median_total_personal_income),
          color = NA) +
  scale_fill_viridis_c(option = "A", label = dol, breaks = c(30, 50, 70) * 1000) +
  labs(fill = "Income") +
  blank_theme_elements


m2 <- ggplot(d) +
  geom_sf(aes(fill = median_rent),
          color = NA) +
  scale_fill_viridis_c(option = "A", label = dol) +
  labs(fill = "Rent") +
  blank_theme_elements


p1 <- d |>
  select(median_total_personal_income, median_rent) |>
  drop_na() |>
  ggplot(aes(x = median_total_personal_income, y = median_rent)) +
  geom_smooth(method = "gam") +
  geom_point() +
  scale_x_continuous(label = dol) +
  scale_y_continuous(label = dol) +
  labs(x = "Median personal income",
       y = "Median rent")

model <- gam(median_rent ~ s(median_total_personal_income), data = d)

m3 <- d |>
  mutate(res = median_rent - predict(model, newdata = d)) |>
  ggplot() +
  geom_sf(aes(fill = res),
          color = NA) +
  scale_fill_viridis_c(option = "D", label = dol) +
  blank_theme_elements +
  labs(fill = "Rent unexplained by income")
  

multi_plot <- m1 +  m2 + p1 + m3 + 
  plot_layout(nrow = 2) +
  plot_annotation(title = the_title,
                  caption = the_caption)

svg_png(multi_plot, "../img/0234-multi-plot", 12, 9)
