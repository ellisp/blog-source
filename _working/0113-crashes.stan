// save as 0113-crashes.stan

// Simple Bayesian model for traffic deaths in New Zealand at six monthly intervals
// Peter Ellis, 15 October 2017

// State space model with random walk in the latent variable and actual measurements
// are log normal - Poisson combination.
// Adapted from Thomas Lumley's model for the same data (but annual) at 
// https://gist.github.com/tslumley/b73d54d1505a3d4478998c79ef271d39


data {
  int n;          // number of six-monthly observations
  int deaths[n];  // deaths (actual count), six month period
  real vkt[n];    // vehicle kilometres travelled in billions
}

parameters {
  real mu1;            // value of mu in the first period
  real delta[n - 1];  // innovation in mu from period to period
  vector[n] epsilon;         // the 'bonus' deviation in each period
  real <lower=0> tau;   // variance of delta
  real <lower=0> sigma; // variance of epsilon
  
}

transformed parameters {
  real mu[n];         // the amount to multiply km travelled by to get lambda.  In other words, death per billion vkt
  real lambda[n];    // lambda is the expected mean death count in any year
  
  mu[1] = mu1;
  for(i in 2:n) mu[i] = mu[i - 1] * exp(delta[ i - 1] * tau);
  
  for(i in 1:n)  lambda[i] = mu[i] * vkt[i] * exp(epsilon[i] * sigma);
  
}

model {
  // priors
  mu1 ~ gamma(2, 1.0 / 10);
  tau ~ gamma(1, 1);
  sigma ~ gamma(1, 1);
  
  // innovation each period:
  delta ~ normal(0, 1);
  
  // bonus deviation each period:
  epsilon ~ normal(0, 1);
  
  //measurement
  deaths ~ poisson(lambda);
}
