
# regression where the coefficients are constrained to be a 
# simplex

library(glmnet)
library(MASS) # for eqscplot
library(pracma) # for lsqlincon
library(rstan)
options(mc.cores = parallel::detectCores())


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
# This is the best way to recover the trued DGP
mod1 <- lm(y ~ X - 1)
coefs1 <- coef(mod1)
coefs1
sum(coefs1)
par(bty = "l")
eqscplot(true_coefs, coefs1); abline(0, 1)

#-----------method 2 - transform-----------
# This way will get you coefficients that add up to 1,
# but might not be in (0, 1). This is suggested by whuber in 
# his comment on Elvis' solution at:
# https://stats.stackexchange.com/questions/21565/how-do-i-fit-a-constrained-regression-in-r-so-that-coefficients-total-1
X2 <- X[, 1:4] - X[, 5]
y2 <- y - X[, 5]

mod2 <- lm(y2 ~ X2 - 1)
coefs2 <- c(coef(mod2), 1 - sum(coef(mod2)))
coefs2
sum(coefs2)
eqscplot(true_coefs, coefs2); abline(0, 1)

#----------method 3 - transform, regularise and constrain-----------
# This method is guaranteed to get you coefficients that add up to 1,
# and they will also be in the (0, 1) range, plus they will be regularised

# alpha = 0 is ridge regression, but could use 1 for lasso (or anything in between)
mod3 <- cv.glmnet(X2, y2, intercept = FALSE, alpha = 0,
                  lower.limits = 0, upper.limits = 1)

# Coefficients from glmnet include the intercept even though we forced it to be zero
# which is why we need the [-1] in the below
coefs3 <- c(as.vector(coef(mod3)[-1]), 1 - sum(coef(mod3)))
coefs3
sum(coefs3)
eqscplot(true_coefs, coefs3); abline(0, 1)

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
coefs4
sum(coefs4)

eqscplot(true_coefs, coefs4); abline(0, 1)
eqscplot(coefs3, coefs4); abline(0, 1)




#----------------method 5 - Bayesian model with a Dirichlet distribution------
stan_data <- list(
  n = nrow(X),
  k = ncol(X),
  X = X,
  y = as.vector(y)
)
  
mod5 <- stan("0281b-dirichlet-coefs.stan", data = stan_data)


coefs5 <- apply(extract(mod5, "b")$b,2, mean)
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
  main = "hi")
# unfortunately



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
