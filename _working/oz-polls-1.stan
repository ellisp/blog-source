// oz-polls-1.stan

data {
  int<lower=1> n_days;           // number of days
  real mu_start;                 // value at starting election
  real mu_finish;                // value at final election
}
parameters {
  real<lower=0,upper=100> mu[n_days];               // underlying state of vote intention
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.01);
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 0.25);
      
  // measurement model
  // 1. Election result
  mu[n_days] ~ normal(mu_finish, 0.01);
  
}
