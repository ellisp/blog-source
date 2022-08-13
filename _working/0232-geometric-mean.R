



gm1 <- function(x){
  prod(x) ^ (1 / length(x))
}


gm2 <- function(x){
  exp(mean(log(x)))
}

x <- 1:10

gm1(x)
gm2(x)

set.seed(123)
y <- exp(rnorm(10000, 10, 1))

ggplot(data.frame(y), aes(x = y)) +
  geom_density(fill = "grey", alpha = 0.5, colour = NA) +
  xlim(0, 250000)


gm1(y)
gm2(y)
mean(y)
