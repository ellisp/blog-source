// oz-polls-3.stan

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election
  real mu_finish;                 // value at final election
  
  // change the below into 5 matrixes with 3 columns each for values, days, standard error
  int y1_n;                     // number of polls
  int y2_n;
  int y3_n;
  int y4_n;
  int y5_n;
  real y1_values[y1_n];       // actual values in polls
  real y2_values[y2_n];       
  real y3_values[y3_n];       
  real y4_values[y4_n];       
  real y5_values[y5_n];       
  int y1_days[y1_n];          // the number of days since starting election each poll was taken
  int y2_days[y2_n]; 
  int y3_days[y3_n]; 
  int y4_days[y4_n]; 
  int y5_days[y5_n]; 
  real y1_se[y1_n];             // the standard errors of the polls
  real y2_se[y2_n];           
  real y3_se[y3_n];           
  real y4_se[y4_n];           
  real y5_se[y5_n];           
}
parameters {
  real<lower=0,upper=100> mu[n_days];               // underlying state of vote intention
  real d[5];                                        // polling effects
  real<lower=0> sigma;                              // sd of innovations
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.01); // starting point
  
  // Jackman used uniform(0, 1) for sigma, but this seems to be the cause
  // of a lot of problems with the estimation process.
  // https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  // recommends not using a uniform, but constraining sigma to be positive
  // and using an open ended prior instead.  So:
  sigma ~ normal(0.5, 0.5);              // prior for innovation sd.  
  
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], sigma);
      
  // measurement model
  // 1. Election result
  mu[n_days] ~ normal(mu_finish, 0.01);
  
  // 2. Polls
  d ~ normal(0, 7.5); // ie a fairly loose prior for house effects
  
  for(t in 1:y1_n)
      y1_values[t] ~ normal(mu[y1_days[t]] + d[1], y1_se[t]);
  for(t in 1:y2_n)
      y2_values[t] ~ normal(mu[y2_days[t]] + d[2], y2_se[t]);
  for(t in 1:y3_n)
      y3_values[t] ~ normal(mu[y3_days[t]] + d[3], y3_se[t]);
  for(t in 1:y4_n)
      y4_values[t] ~ normal(mu[y4_days[t]] + d[4], y4_se[t]);
  for(t in 1:y5_n)
      y5_values[t] ~ normal(mu[y5_days[t]] + d[5], y5_se[t]);
}
