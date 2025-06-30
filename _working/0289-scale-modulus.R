


library(tidyverse)

n <- 100
d <- tibble(`x 1` = exp(rnorm(n)) * runif(n, -1, 1),
       y = exp(rnorm(n)) * runif(n, -1, 1))
d[1, ]$`x 1` <- NA
d[2, ]$y <- NA

# works ok, even with coord_equal, and with labels, and with missing values:
d |> 
  ggplot(aes(x = `x 1`, y = y)) +
  geom_point() +
  scale_x_continuous(transform = transform_modulus(p = 0), label = dollar) +
  scale_y_continuous(transform = transform_modulus(p = 0), label = percent) +
  coord_equal()

