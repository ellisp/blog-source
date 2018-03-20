

data {
  int n;
  int lower_limit;
  int <lower = lower_limit> x[n];
  real lambda_start_mu;
  real lambda_start_sigma;
}

parameters {
  real<lower=0>lambda;
}

model {
  lambda ~ normal(lambda_start_mu, lambda_start_sigma);
  
  for(i in 1:n){
    x[i] ~ poisson(lambda) T[lower_limit, ];
  }
  
}
