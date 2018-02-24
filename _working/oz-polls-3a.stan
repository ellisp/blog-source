// oz-polls-3a.stan

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
  vector[y1_n] y1_se;             // the sample errors of the polls
  vector[y2_n] y2_se;           
  vector[y3_n] y3_se;           
  vector[y4_n] y4_se;           
  vector[y5_n] y5_se;           
}
parameters {
  vector<lower=0,upper=1>[n_days] mu;         // 
  real d[5];                      // polling effects
  real<lower=0> sigma;            // sd of innovations
}


model {
  // priors 
  sigma ~ normal(0.001, 0.001);     // prior for innovation sd.  
  mu[1] ~ beta(2, 2);               // starting state space
  d ~ normal(0, 0.05); // ie a fairly loose prior for house effects (on scale of [0,1])
  
  
  // state model
  mu[2:n_days] ~ student_t(4, mu[1:(n_days - 1)], sigma);

  // measurement model
  // 1. Election results
  mu_start ~ normal(mu[1], 0.0001);
  mu_finish ~ normal(mu[n_days], 0.0001);
  
  // 2. Polls
  y1_values ~ normal(mu[y1_days] + d[1], y1_se * inflator);
  y2_values ~ normal(mu[y2_days] + d[2], y2_se * inflator);
  y3_values ~ normal(mu[y3_days] + d[3], y3_se * inflator);
  y4_values ~ normal(mu[y4_days] + d[4], y4_se * inflator);
  y5_values ~ normal(mu[y5_days] + d[5], y5_se * inflator);

}
