library(primes)
library(tidyverse)
library(glue)

for(k in c(30, 100, 300)){
squares <- (1:k) ^ 2
primes <- tibble(p = generate_primes(max = k^2))

# This is the part that gets slower with larger k as you make k^3 combinations
s3s <- expand_grid(s1 = squares, s2 = squares, s3 = squares) |>
  filter(s2 >= s1 & s3 >= s2) |> 
  mutate(sum_3_sq = s1 + s2 + s3)

# example:
filter(s3s, sum_3_sq == 397)

s3s_sum <- s3s |>
  group_by(sum_3_sq) |>
  summarise(number_3_square_sums = n())

# example:
s3s_sum |>
  filter(number_3_square_sums == 3) |>
  slice(1) |>
  left_join(s3s, by = "sum_3_sq")

res <- primes |>
  left_join(s3s_sum, by = c("p" = "sum_3_sq")) |>
  mutate(number_3_square_sums = replace_na(number_3_square_sums, 0))

p <- ggplot(res, aes (x = p, y = number_3_square_sums)) +
  geom_point() +
  annotate("point", colour = "red", shape = 1, size = 4, x = 397, y = 1) +
  annotate("text", colour = "red", label = "397", x = 500, y = 1, hjust = 0) +
  scale_x_continuous(label = comma) +
  labs(x = "Prime number",
       y = "Number of ways",
       title = "Number of ways to make a prime number as sum of three positive squares of integers",
       subtitle = glue("397 (circled) is the largest with exactly one way, of primes up to {comma(k^2)}."))

if(max(res$number_3_square_sums) < 11){
  p <- p + 
    scale_y_continuous(breaks = 0:10) +
    theme(panel.grid.minor.y = element_blank())
}

svg_png(p, glue("../img/0280-primes-squares-k{k}"), w = 9, h = 7)
}