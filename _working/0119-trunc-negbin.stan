

data {
  int n;
  int lower_limit;
  int <lower = lower_limit> x[n];
  real mu_prior_mean;
  real mu_prior_sd;
  real phi_prior_mean;
  real phi_prior_sd;
}

parameters {
  real<lower=0>mu;
  real<lower=0>phi;
}

model {
  mu ~ normal(mu_prior_mean, mu_prior_sd);
  phi ~ normal(phi_prior_sd, phi_prior_sd);
  
  for(i in 1:n){
    x[i] ~ neg_binomial_2(mu, phi) T[lower_limit, ];
  }
  
}
