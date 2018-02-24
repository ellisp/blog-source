// oz-polls-1a.stan

data {
  int<lower=1> n_days;           // number of days
  real mu_start;                 // value at starting election
  real mu_finish;                // value at final election
}
parameters {
  vector[n_days] mu;               // underlying state of vote intention, on logit scale
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.001);
  
  mu[2:n_days] ~ normal(mu[1:(n_days - 1)], 0.01);
  
  // measurement model
  // 1. Election result
  mu_finish ~ normal(mu[n_days], 0.001);
  
}
