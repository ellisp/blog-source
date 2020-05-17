
  
  
data {
  int<lower=0> N;         // Number of days of data
  int<lower=0> y[N];      // Observed number of sick people
}

parameters {
  vector<lower=0>[N]          r;                   // Effective Reproduction Number
  real<lower=0, upper=1> p;                   // probability of actually being observed
  vector<lower=0>[N]          w;                   // Genuine number of sick people
  vector<lower=0>[N]          new_cases_from_today;
  //real<lower=0>               sigma_p;             // standard deviation of change in p over time
  real<lower=0>               sigma_r;             // standard deviation of change in R over time
}

model {
  // priors. These are a bit arbitrary and impact on how smoothed the end results are:
  p ~ beta(2, 5);
  r[1] ~ normal(1, 0.4);
  sigma_r ~ normal(0.02, 0.01); // how much can R change from day to day. A smoothing parameter.
  
  // state model. This is intrinsically iterative so has to be in a loop.
  for(i in 1: (N-5)){
    
    // ideally we would have a Poisson distribution but we can't have discrete parameters, so no discrete
    // latent variables either. So we make this a normal distribution with the same variance a Poisson would have:
    w[i+5] ~ normal(w[i] * r[i], sqrt( w[i] * r[i]));
    
    // The reproduction number follows a unseen random walk over time, smoothness governed by sigma_r
    r[2:N] ~ normal(r[1:(N-1)], sigma_r);
  } ;
  
  // measurement model. This is less complicated (no iterations) so is vectorized:
  y ~ poisson(w * p)  ;

  
 
 
}

