// Draws heavily on https://mc-stan.org/docs/2_24/stan-users-guide/summing-out-the-responsibility-parameter.html

data {
  int<lower=0> N;
  int x[N];
  int<lower=1> K; // number of mixture components
}

parameters {
  positive_ordered[K] mu; // arbitrarily ordered so lower value is first, otherwise unidentified
  real<lower=0> phi[K];   // dispersion parameter for the negative binomials
  simplex[K] p;
}

model {
  vector[K] log_p = log(p); // cached log calculation
  mu ~ gamma(1.5, 0.1); // prior for the means of the negative binomial distribution
  phi ~ gamma(2, 1);     // prior for the phi (dispersion) of the neg bin distribution
  for (n in 1:N){
    vector[K] lps = log_p;
    for (k in 1:K)
      lps[k] += neg_binomial_2_lpmf(x[n] | mu[k], phi[k]);
    target += log_sum_exp(lps);
  }
  
}

