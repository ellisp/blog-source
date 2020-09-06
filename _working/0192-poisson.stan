// Draws heavily on https://mc-stan.org/docs/2_24/stan-users-guide/summing-out-the-responsibility-parameter.html

data {
  int<lower=0> N;
  int x[N];
  int<lower=1> K; // number of mixture components
}

parameters {
  positive_ordered[K] lambda; // arbitrarily ordered so lower value is first, otherwise unidentified
  simplex[K] p;
}

model {
  vector[K] log_p = log(p); // cached log calculation
  lambda ~ gamma(1.5, 0.1); // prior for the lambdas
  
  for (n in 1:N){
    vector[K] lps = log_p;
    for (k in 1:K)
      lps[k] += poisson_lpmf(x[n] | lambda[k]);
    target += log_sum_exp(lps);
  }
  
}

