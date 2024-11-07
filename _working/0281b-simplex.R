
# regression where the coefficients are constrained to be a 
# simplex

library(glmnet)
library(MASS)   # for eqscplot
library(pracma) # for lsqlincon
library(rstan)
options(mc.cores = parallel::detectCores())
par(bty = "l", family = "Roboto")
mypar <- par()

detach("package:conflicted", unload=TRUE)

n <- 1000
X <- cbind(
  x1 = rnorm(n , 10, 2),
  x2 = rnorm(n, 7, 1),
  x3 = rnorm(n, 15, 3),
  x4 = runif(n, 5, 20),
  x5 = rnorm(n, 8, 1.5)
)
# note true data generating process coefficients add up
# to more than 1 and have a negative, to make it interesting
true_coefs <- c(0.2, 0.6, -0.1, 0, 0.4)

y <- X %*% true_coefs + rnorm(n, 0, 2)

#-------------method 1 - unconstrained regression------
# This is the best way to recover the true DGP
mod1 <- lm(y ~ X - 1)
coefs1 <- coef(mod1)
round(coefs1, 2)
sum(coefs1)
f <- function(newx, main = ""){
  eqscplot(true_coefs, newx, bty = "l", 
           xlab = "Coefficients from original data generating process",
           ylab = "Coefficients from model",
           main = main) 
  abline(0, 1, col = "grey")
}

f2 <- function(){f(coefs1, "OLS")}
svg_png(f2, "../img/0281b-mod1", w = 8, h = 7)

#-----------method 2 - transform-----------
# This way will get you coefficients that add up to 1,
# but might not be in (0, 1). This is suggested by whuber in 
# his comment on Elvis' solution at:
# https://stats.stackexchange.com/questions/21565/how-do-i-fit-a-constrained-regression-in-r-so-that-coefficients-total-1
X2 <- X[, 1:4] - X[, 5]
y2 <- y - X[, 5]

mod2 <- lm(y2 ~ X2 - 1)
coefs2 <- c(coef(mod2), 1 - sum(coef(mod2)))
round(coefs2, 2)
sum(coefs2)

f2 <- function(){f(coefs2, "Transformation")}
svg_png(f2, "../img/0281b-mod2", w = 8, h = 7)


#----------method 3 - transform, regularise and constrain-----------
# This method is guaranteed to get you coefficients that add up to 1,
# and they will also be in the (0, 1) range, plus they will be regularised

# alpha = 0 is ridge regression, but could use 1 for lasso (or anything in between)
mod3 <- cv.glmnet(X2, y2, intercept = FALSE, alpha = 0,
                  lower.limits = 0, upper.limits = 1)

# Coefficients from glmnet include the intercept even though we forced it to be zero
# which is why we need the [-1] in the below
coefs3 <- c(as.vector(coef(mod3)[-1]), 1 - sum(coef(mod3)))
round(coefs3, 2)
sum(coefs3)

f2 <- function(){f(coefs3, "Transform and ridge")}
svg_png(f2, "../img/0281b-mod3", w = 8, h = 7)

#---------------method 4 - quadratic programming-----------------
# This is based on dariober's solution, which itself is Elvis' solution
# but using the easier API from pracma::lsqlincon
# lsqlincon "solves linearly constrained linear least-squares problems"

# Equality constraint: We want the sum of the coefficients to be 1.
# I.e. Aeq x == beq  
Aeq <- matrix(rep(1, ncol(X)), nrow= 1)
beq <- c(1)

# Lower and upper bounds of the parameters, i.e [0, 1]
lb <- rep(0, ncol(X))
ub <- rep(1, ncol(X))

# And solve:
coefs4 <- lsqlincon(X, y, Aeq= Aeq, beq= beq, lb= lb, ub= ub)
round(coefs4, 2)
sum(coefs4)

f2 <- function(){f(coefs4, "Quadratic programming")}
svg_png(f2, "../img/0281b-mod4", w = 8, h = 7)




#----------------method 5 - Bayesian model with a Dirichlet distribution------
stan_data <- list(
  n = nrow(X),
  k = ncol(X),
  X = X,
  y = as.vector(y)
)
  
mod5 <- stan("0281b-dirichlet-coefs.stan", data = stan_data)


coefs5 <- apply(extract(mod5, "b")$b,2, mean)

round(coefs5, 2)
sum(coefs5)

f2 <- function(){f(coefs5, "Dirichlet prior")}
svg_png(f2, "../img/0281b-mod5", w = 8, h = 7)

#---------------comparisons------------
norm_coefs <- pmax(0, true_coefs) # remove negative values
norm_coefs <- norm_coefs / sum(norm_coefs) # force to ad dup to one

pairs(cbind('Actual data\ngenerating process' = true_coefs, 
            'Normalised to\nadd to one and > 0' = norm_coefs, 
            "Ordinary least squares,\nwon't add to 1, not\nconstrained to (0,1)" = coefs1, 
            'Transform and OLS, add to 1\nbut can be negative' = coefs2, 
            'Transform and ridge regression,\nconstrain to within (0,1)' = coefs3, 
            'Quadratic programming' = coefs4, 
            'Dirichlet & Stan' = coefs5), 
      lower.panel = function(x, y){
        abline(0, 1, col = "grey")
        points(x, y, pch = 19)
      }, 
      upper.panel = function(x, y){
        text(mean(range(x)), mean(range(y)), round(cor(x, y), 2), 
             col = "steelblue", font = 2)
        },
  main = "Comparison of different methods for creating a weighted average with weights adding up to one")

# basically the Stan and quadratic programming approaches give near-identical
# results, and they are the best results in terms of capturing the proportions
# in the original DGP while respecting the constraints to add to 1, be in (0,1).
# The worst approach is the glmnet one (which is sad as I quite like that one)


#-----------scribble-------------------


stan_code <- 
"
data {
  int<lower = 1> K;
  real<lower = 0> alpha;
}
generated quantities {
  vector[K] theta = dirichlet_rng(rep_vector(alpha, K));
}
"

stan(model_code = stan_code, data = list(K = 10, alpha = 0.001))
