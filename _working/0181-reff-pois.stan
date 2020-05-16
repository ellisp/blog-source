
data {
  int<lower=0> N;         // Number of days of data
  int<lower=0> y[N];      // Observed number of sick people
}

parameters {
  real<lower=0>r;                   // Effective Reproduction Number
  real<lower=0, upper=1>p[N];      // probability of actually being observed
  real<lower=0> w[N];               // Genuine number of sick people
  real<lower=0> sigma;             // standard deviation of change in p over time
}

model {
  // priors
  r ~ normal(1, 0.1);
  p ~ beta(2, 5);
  sigma ~ normal(0.01, 0.01);
  
  // state model
  for(i in 1: (N-5)){
    // ideally w would have a Poisson distribution but we can't have discrete parameters, so no discrete
    // latent variables either. So we make this a normal distribution with the same variance a Poisson would have:
    w[i+5] ~ normal(1 + w[i] * r, sqrt(1 + w[i] * r));
  } ;
  
  // measurement model
  for(j in 1:N){
    // The proportion of the people we see follows an unseen path over time:
    p[2:N] ~ normal(p[1:(N-1)], sigma);
    
    // The number of people we see:
    y[j] ~ poisson(w[j] * p[j]);  
  }
  
 
 
}

