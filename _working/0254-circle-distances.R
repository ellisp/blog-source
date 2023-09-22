# analytical answer is here 
# https://www.wolframalpha.com/input?i=%28x-a%29%5E2%2B%28y-r%29%5E2%3Dr%5E2%2C+y%3Dx%5E2%2C+%28x-a%29%2F%28x%29%3D-2%28y-r%29%2F1%2C+a%5E2%2Br%5E2%3D%281-r%29%5E2%2C+a%3E0

library(tidyverse)

d1 <- tibble(x = seq(from = 0, to = 1, length.out = 1000)) |>
  mutate(line1_y = sqrt(x),
         line2_y  = sqrt(1 -x ^2 )) 

d1_fine <- tibble(x = seq(from = 0, to = 1, length.out = 5000)) |>
  mutate(line1_y = sqrt(x),
         line2_y  = sqrt(1 -x ^2 )) 


d2 <- filter(d1, x < 0.6)
d2_fine <- filter(d1_fine, x < 0.6)


get_dc <- function(x_from, x_to, y_from, y_to, d2, length = 100){
  possibilities <- expand_grid(can_x = seq(from = x_from, to = x_to, length.out = length),
                               can_y = seq(from = y_from, to = y_to, length.out = length))
  
  distances1 <- expand_grid(d2[, 1:2], possibilities) |>
    mutate(distance_1 = sqrt((x - can_x) ^ 2 + (line1_y - can_y) ^ 2)) |>
    group_by(can_x, can_y) |>
    arrange(distance_1) |>
    slice(1) |>
    ungroup() |>
    select(can_x, can_y, distance_1)
  
  distances2 <- expand_grid(d2[, c(1, 3)], possibilities) |>
    mutate(distance_2 = sqrt((x - can_x) ^ 2 + (line2_y - can_y) ^ 2)) |>
    group_by(can_x, can_y) |>
    arrange(distance_2) |>
    slice(1) |>
    ungroup() |>
    select(can_x, can_y, distance_2)
  
  dc <- distances1 |>
    left_join(distances2, by = c("can_x", "can_y")) |>
    mutate(distance_3 = can_x,
           smallest = pmin(distance_1, distance_2, distance_3)) |>
    arrange(desc(smallest)) |>
    slice(1:100)
}

# coarse search:
dc1 <- get_dc(0, 0.5, 0.5, 1, d2)
dc1
range(dc1$can_x)

# more precise search:
dc2 <- get_dc(min(dc1$can_x), max(dc1$can_x), min(dc1$can_y), max(dc1$can_y), 
              d2 = d2_fine, length = 200)

dc2[1, ]$smallest

gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

lw <- 1

p <- d1 |>
  ggplot(aes(x = x)) +
  geom_line(aes(y = line1_y), colour = "red", size = lw) +
  geom_line(aes(y = line2_y), colour = "purple", size = lw) +
  geom_vline(xintercept = 0, colour = "black", size = lw) +
  gg_circle(xc = dc2[1, ]$can_x, yc = dc2[1, ]$can_y, r = dc2[1, ]$can_x, 
            color = "orange", fill = "orange", alpha = 0.5) +
  coord_equal() +
  labs(x = "", y = "")

svg_png(p, "../img/0254-circles", w = 8, h = 7)
