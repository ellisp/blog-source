
data {
  int<lower=0> N;
  int<lower=0> y[N];
}

parameters {
  vector<lower=0>[N] mu;
  real<lower=0> sigma;           // sd of innovations
}

model {
  // priors
  sigma ~ normal(0.001, 0.001); // prior for innovation sd
  mu[1] ~ normal(20, 5);
  
  
  // state model
  mu[2:N] ~ student_t(4, mu[1:(N-1)], sigma);
  
  // measurement model
  y ~ poisson(mu);
}

