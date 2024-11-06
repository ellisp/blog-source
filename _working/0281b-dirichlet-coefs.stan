data {
  int<lower=0> n;   // number of data items
  int<lower=0> k;   // number of predictors
  matrix[n, k] X;   // predictor matrix
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
