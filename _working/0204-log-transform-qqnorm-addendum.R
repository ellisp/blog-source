

x <- list()
n <- 300
x[[1]] <- rnorm(n)
x[[2]] <- exp(rnorm(n))
x[[3]] <- -exp(rnorm(n))

qqnorm_demo <- function(){
par(mfrow = c(2,3), bty = "l", family = "Roboto")

qqnorm(x[[1]], main = "Normal")
qqnorm(x[[2]], main = "Right-skewed")
qqnorm(x[[3]], main = "Left-skewed")
lapply(x, function(x){plot(density(x), main = "")})
}

svg_png(qqnorm_demo, "../img/0204-qqnorm-demo", w = 10, h = 6)
