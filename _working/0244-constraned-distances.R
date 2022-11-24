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
ggpairs(as_tibble(m1))

m2 <- m1 / rowSums(m1)

# check that all the rows add up to 1.0000:
stopifnot(unique(round(rowSums(m2), 4)) == 1)



ggpairs(as_tibble(m2))


d <- as_tibble(m2) |>
  mutate(id = 1:n())

geom_mean <- function(...){
  if(min(...) <= 0){
    gm <- prod(...) ^ (1 / length( ... ))
  } else {
    gm <- exp(mean(log(...)))    
  }
  return(gm)
}

geom_mean(1:3)
x <- c(-3, -2, -3, -4)
geom_mean(x)
geom_mean(abs(x))




euc_dist <- function(d1, d2 = 0, d3 = 0){
  d <- sqrt(d1 ^ 2 + d2 ^ 2 + d3 ^ 2)
}

distances <- d |>
  rename(x1 = x, 
         y1 = y,
         z1 = z,
         id1 = id) |>
  left_join(d, by = join_by(id1 > id)) |>
  filter(!is.na(id)) |>
  mutate(d1 = x - x1,
         d2 = y - y1,
         d3 = z - z1) |>
  mutate(
    dxy = euc_dist(d1, d2),
    dxz = euc_dist(d1, d3),
    dyz = euc_dist(d2, d3),
    dxyz = euc_dist(y-y1,x-x1, z-z1)
  )

distances |>
  select(dxy:dxyz) |>
  ggpairs()

distances |>
  select(d1:d3, dxy:dxyz) |>
  ggpairs()

# see https://stats.stackexchange.com/questions/259208/how-to-perform-isometric-log-ratio-transformation#:~:text=The%20ILR%20(Isometric%20Log%2DRatio,time%20spent%20in%20various%20activities.

# ILR (Isometric log-ratio) transformation.
# `x` is an `n` by `k` matrix of positive observations with k >= 2.
#
ilr <- function(x, p = 0, offset = 0) {
  if (any((x + offset) < 0, na.rm = TRUE)) {
    stop("x must be only positive values.")
  }
  if (abs(p) < 1e-07) {
    y <- log(x + offset)
  } else {
    y <- ((x + offset) ^ p - 1) / p
  }       
  y <- y - rowMeans(y, na.rm = TRUE)            # Recentered values
  k <- dim(y)[2]
  H <- contr.helmert(k)                       # Dimensions k by k-1
  H <- t(H) / sqrt((2:k) * (2:k - 1))         # Dimensions k-1 by k
  z <- y %*% t(H)                             # Rotated/reflected values
  if(!is.null(colnames(x))){                   # (Helps with interpreting output)
    colnames(z) <- paste0(colnames(x)[-1], ".ILR")
  }
  return(z)                          
}

ilr(m1, p = 0.1)
