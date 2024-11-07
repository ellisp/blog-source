// Stan program for a regression where the coefficients are a simplex
// Probable use case is when y is some kind of weighted average of the X
// and you want to guarantee the weights add up to 1 (note - note sure I think
// this is a good idea but it is out there)
//
// Peter Ellis November 2024



data {
  int<lower=0> n;   // number of data items must match number rows of X
  int<lower=0> k;   // number of predictors must match number columns of X
  matrix[n, k] X;   // predictor matrix. Might include intercept (column of 1s)
  vector[n] y;      // outcome vector
}
parameters {
  simplex[k] b;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
  real<lower=0, upper = 1> alpha; // parameter for dirichlet distribution
}
model {
  alpha ~ beta(2, 2);
  b ~ dirichlet(rep_vector(alpha, k));
  y ~ normal(X * b, sigma);  // likelihood
}
