# analytical answer is here 
# https://www.wolframalpha.com/input?i=%28x-a%29%5E2%2B%28y-r%29%5E2%3Dr%5E2%2C+y%3Dx%5E2%2C+%28x-a%29%2F%28x%29%3D-2%28y-r%29%2F1%2C+a%5E2%2Br%5E2%3D%281-r%29%5E2%2C+a%3E0

library(tidyverse)

# Define some values of the red line (y = sqrt(x)) and purple line (unit circle)
# for 1000 values of x:
d1 <- tibble(x = seq(from = 0, to = 1, length.out = 1000)) |>
  mutate(line1_y = sqrt(x),
         line2_y  = sqrt(1 -x ^2 )) 

# As above but finer grid, for 5000 values of x
d1_fine <- tibble(x = seq(from = 0, to = 1, length.out = 5000)) |>
  mutate(line1_y = sqrt(x),
         line2_y  = sqrt(1 -x ^2 )) 

# Cut down versions of that for just the visually plausible part on the left of the chart
d2 <- filter(d1, x < 0.6)
d2_fine <- filter(d1_fine, x < 0.6)

# Function to search a rectangle (defined by first four parameters), calculate the
# distance of each point in the rectangle from 3 lines (vertical at 0, y = sqrt(x),
# and unit circle) and return the 100 points with the highest value of the
# lowest of the differences from the three lines
get_dc <- function(x_from, x_to, y_from, y_to, d2, length = 100){
  possibilities <- expand_grid(can_x = seq(from = x_from, to = x_to, length.out = length),
                               can_y = seq(from = y_from, to = y_to, length.out = length))
  
  # Distances from the points on the y = sqrt(x) line
  distances1 <- expand_grid(d2[, 1:2], possibilities) |>
    mutate(distance_1 = sqrt((x - can_x) ^ 2 + (line1_y - can_y) ^ 2)) |>
    group_by(can_x, can_y) |>
    arrange(distance_1) |>
    slice(1) |>
    ungroup() |>
    select(can_x, can_y, distance_1)
  
  # Distances from the points in the unit circle
  distances2 <- expand_grid(d2[, c(1, 3)], possibilities) |>
    mutate(distance_2 = sqrt((x - can_x) ^ 2 + (line2_y - can_y) ^ 2)) |>
    group_by(can_x, can_y) |>
    arrange(distance_2) |>
    slice(1) |>
    ungroup() |>
    select(can_x, can_y, distance_2)
  
  dc <- distances1 |>
    left_join(distances2, by = c("can_x", "can_y")) |>
    # add the distance to the vertical black line
    mutate(distance_3 = can_x,
           # which is the smallest of the three distances:
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

#----------------plot----------------

# See Trevor's answer at
# https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# for this convenient function for drawing circles in a ggplot2 chart:
gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

lw <- 1 # linewidth

p <- d1 |>
  ggplot(aes(x = x)) +
  geom_line(aes(y = line1_y), colour = "red", linewidth = lw) +
  geom_line(aes(y = line2_y), colour = "purple", linewidth = lw) +
  geom_vline(xintercept = 0, colour = "black", linewidth = lw) +
  gg_circle(xc = dc2[1, ]$can_x, yc = dc2[1, ]$can_y, r = dc2[1, ]$can_x, 
            color = "orange", fill = "orange", alpha = 0.5) +
  coord_equal() +
  labs(x = "", y = "", title = "Find the radius of the brown circle") +
  annotate("text", x = 0.8, y = 0.95, label = "y==sqrt(x)", parse = TRUE, colour = "red") +
  annotate("text", x = 0.8, y = 0.70, label = "x^2 + y^2 == 1", parse = TRUE, colour = "purple")


svg_png(p, "../img/0254-circles", w = 8, h = 7)
