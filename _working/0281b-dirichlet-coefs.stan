data {
  int<lower=0> n;   // number of data items
  int<lower=0> k;   // number of predictors
  matrix[n, k] X;   // predictor matrix
  vector[n] y;      // outcome vector
}
parameters {
  simplex[k] b;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
model {
  // b ~ beta(1, 37); // prior
  b ~ dirichlet(rep_vector(1, k));
  y ~ normal(X * b, sigma);  // likelihood
}
