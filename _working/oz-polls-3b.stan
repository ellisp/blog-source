// <source>./_working/oz-polls-3b.stan
// Run this Stan script from <source>./_working/0102b-oz-polls.R

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election, on [0,1] scale
  real mu_finish;                 // value at final election, on [0,1] scale
  real inflator;                  // amount by which to inflate the standard error of polls
  
  // information on five polls from different houses
  int y1_n;                     // number of polls by house 1
  int y2_n;
  int y3_n;
  int y4_n;
  int y5_n;
  vector[y1_n] y1_values;       // actual values in polls, in 0-1 scale
  vector[y2_n] y2_values;       
  vector[y3_n] y3_values;       
  vector[y4_n] y4_values;       
  vector[y5_n] y5_values;       
  int y1_days[y1_n];          // the number of days since starting election each poll was taken
  int y2_days[y2_n]; 
  int y3_days[y3_n]; 
  int y4_days[y4_n]; 
  int y5_days[y5_n]; 
  vector[y1_n] y1_se;             // the sampling errors of the polls
  vector[y2_n] y2_se;           
  vector[y3_n] y3_se;           
  vector[y4_n] y4_se;           
  vector[y5_n] y5_se;           
}
parameters {
  vector[n_days] epsilon;         // 
  real d[5];                      // polling effects
  real<lower=0> sigma;            // sd of innovations
  real alpha;                     // moving average term
}

transformed parameters {
  vector[n_days] mu;
  mu[1] = mu_start;
  for(i in 2:n_days)
    mu[i] = mu[i-1] + epsilon[i] * sigma + alpha * epsilon[i-1] * sigma; // MA(1) process
}


model {
  // priors 
  sigma ~ normal(0.001, 0.001);     // prior for innovation sd.  
  d ~ normal(0, 0.05); // ie a fairly loose prior for house effects (on scale of [0,1])
  
  
  // state model.  The trick to doing this by the innovation, rather than something 
  // centered at mu, is to make the scale below 1, and just put sigma into the
  // transformed parameters block.
  epsilon ~ student_t(4, 0, 1);

  // measurement model
  // 1. Election results
  // mu_start ~ normal(mu[1], 0.0001);
  mu_finish ~ normal(mu[n_days], 0.0001);
  
  // 2. Polls
  y1_values ~ normal(mu[y1_days] + d[1], y1_se * inflator);
  y2_values ~ normal(mu[y2_days] + d[2], y2_se * inflator);
  y3_values ~ normal(mu[y3_days] + d[3], y3_se * inflator);
  y4_values ~ normal(mu[y4_days] + d[4], y4_se * inflator);
  y5_values ~ normal(mu[y5_days] + d[5], y5_se * inflator);

}
