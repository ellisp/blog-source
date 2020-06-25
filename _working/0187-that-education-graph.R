

# by eye, the Deloitteline is about y = 19000 -9.5x




19000 + 1000 * b1 = 9500
b1= (9500 - 19000) / 1000 = -9.5

so target values for b0 and b1 are 19000, -9.5

n <- 100
set.seed(123)
x <- exp(rnorm(n * 10, 3, 2))
ggplot(data.frame(x = x), aes(x = x)) + geom_rug() + geom_density()

x <- c(sample(x[x <400], 98, replace = TRUE), 450, 1150)

n <- 100 



fn <- function(par){

  sigma <- par[1]
  log_b0 <- par[2]
  log_b1 <- par[3]
  
  set.seed(123)
  x <- exp(rnorm(n * 10, 3, 2))
  x <- c(sample(x[x <400], n - 2, replace = TRUE), 450, 1150)
  
  y <- exp(log_b0 + log_b1 * log(x) + rnorm(n, sigma))
  y[c(n, n-1)] <- c(14000, 9700)
  
  mod <- lm(y ~ x)
  how_bad <- (coef(mod)[1]/1000 - 9.500) ^ 2 + 
    (coef(mod)[2] - -9.5) ^ 2 + 
    (max(y) / 5000 - 10) ^ 2  + 
    (quantile(y, 0.05) / 1000 - 10) ^ 2

  return(how_bad)
}

best <- optim(c(1, 10, -2), fn = fn)
best
set.seed(123)
x <- exp(rnorm(n * 10, 3, 2))
x <- c(sample(x[x <400], 98, replace = TRUE), 450, 1150)
y <- exp(best$par[2] + best$par[3] * log(x) + rnorm(n, best$par[1]))
y[c(n, n-1)] <- c(14000, 9700)

mod <- lm(y ~ x)

summary(mod)
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_smooth(method = "lm") +
  geom_point()
