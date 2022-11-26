library(MASS)
library(tidyverse)
library(GGally)

set.seed(42)

sig <- matrix(c(1, 0.2, 0.4, 0.2, 1, -0.3, 0.4, -0.3, 1),
              byrow = TRUE, nrow = 3)
n <- 100
m1 <- exp(mvrnorm(n, mu = c(0,0,0), Sigma = sig))

m1[runif(n * 3) < 0.05] <- 0

summary(m1)
# need to make some of m1 real zeroes

colnames(m1) <- c("x", "y", "z")

p1 <- function(){print(
  ggpairs(as_tibble(m1))
)}
svg_png(p1, "../img/0244-exp-data")

m2 <- m1 / rowSums(m1)

# check that all the rows add up to 1.0000:
stopifnot(all(unique(round(rowSums(m2), 4)) == 1))


p2 <- function(){print(
  ggpairs(as_tibble(m2))
)}
svg_png(p2, "../img/0244-simplex-data")


#' ILR (Isometric log-ratio) transformation.
#' see https://stats.stackexchange.com/questions/259208/how-to-perform-isometric-log-ratio-transformation#:~:text=The%20ILR%20(Isometric%20Log%2DRatio,time%20spent%20in%20various%20activities.
#' @param `x`  an `n` by `k` matrix of positive observations with k >= 2.
#' @param p Box-Cox parameter
ilr <- function(x, p = 0) {
  if (any(x < 0, na.rm = TRUE)) {
    stop("x must be only positive values.")
  }
  if (abs(p) < 1e-07) {
    y <- log(x)
  } else {
    y <- (x ^ p - 1) / p
  }       
  y <- y - rowMeans(y, na.rm = TRUE)            # Recentered values
  k <- dim(y)[2]
  H <- contr.helmert(k)                       # Dimensions k by k-1
  H <- t(H) / sqrt((2:k) * (2:k - 1))         # Dimensions k-1 by k
  z <- y %*% t(H)                             # Rotated/reflected values
  if(!is.null(colnames(x))){                   # (Helps with interpreting output)
    colnames(z) <- paste0(colnames(x)[-k], "_ilr")
  }
  return(z)                          
}


#' convenience function for euclidean distance between 2 or 3 dimensional points
euc_dist <- function(d1, d2 = 0, d3 = 0){
  d <- sqrt(d1 ^ 2 + d2 ^ 2 + d3 ^ 2)
}

distances <- function(p = 0){
  d <- m2  |>
    # two dimensional version
    cbind(ilr(m2, p = p)) |>
    as_tibble() |>
    mutate(id = 1:n())
  
  d |>
    rename(x1 = x, 
           y1 = y,
           z1 = z,
           x_ilr1 = x_ilr,
           y_ilr1 = y_ilr,
           id1 = id) |>
    left_join(d, by = join_by(id1 > id)) |>
    filter(!is.na(id)) |>
    mutate(d1 = x - x1,
           d2 = y - y1,
           d3 = z - z1) |>
    mutate(
      d_xy = euc_dist(d1, d2),
      d_xz = euc_dist(d1, d3),
      d_yz = euc_dist(d2, d3),
      d_xyz = euc_dist(y-y1,x-x1, z-z1),
      d_ilr = euc_dist(x_ilr1 - x_ilr, y_ilr1 - y_ilr)
    )
}

p3 <- function(){print(
  distances() |>
    select(d_xy:d_xyz) |>
    ggpairs() +
    labs(title = "Different methods of comparing pairwise differences of compositional data",
         subtitle = "Comparing choices of two of the original dimensions with use of all three")
)}

svg_png(p3, "../img/0244-distances")


p4 <- function(){print(
  distances(p = 1) |>
    select(d_xy:d_ilr) |>
    ggpairs() +
    labs(title = "Different methods of comparing pairwise differences of compositional data",
         subtitle = "d_ilr is isometric logarithm ratio transformation without the logarithm")
)}
svg_png(p4, "../img/0244-distances-ilr1")

p5 <- function(){print(
  distances(p = 0) |>
    select(d_xy:d_ilr) |>
    ggpairs() +
    labs(title = "Different methods of comparing pairwise differences of compositional data",
         subtitle = "d_ilr is isometric logarithm ratio transformation") 
)}
svg_png(p5, "../img/0244-distances-ilr0")

p6 <- function(){print(
  distances(p = 0.5) |>
    select(d_xy:d_ilr) |>
    ggpairs() +
    labs(title = "Different methods of comparing pairwise differences of compositional data",
         subtitle = "d_ilr is isometric logarithm ratio transformation with a Box-Cox (0.5) transformation instead of logarithm")
)}
svg_png(p6, "../img/0244-distances-ilr0.5")

  
# discrepancies <- distances |>
#   mutate(discrepancy = abs(d_xyz - d_xy),
#          number_zeroes = 
#            (x1 == 0) +
#            (y1 == 0) +
#            (z1 == 0) +
#            (x == 0) +
#            (y == 0) +
#            (z == 0))|>
#   arrange(desc(discrepancy)) 
# 
# 
# discrepancies |>
#   select(x1:z1, x:z, d_xy:d_xyz, number_zeroes, discrepancy) 
# 
# discrepancies |>
#   ggplot(aes(fill = as.ordered(number_zeroes), discrepancy)) +
#   geom_density(alpha = 0.5)
# 
# discrepancies |>
#   group_by(number_zeroes) |>
#   summarise(number_comparisons = n(),
#             mean_discrepancy = mean(discrepancy),
#             stdev = sd(discrepancy),
#             sterr = stdev / (number_comparisons - 1))
# 
# 
