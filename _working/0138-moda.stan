//Generalized linear model with poisson response for a cross tab of lions, tigers and bears with A, B, C, D



data {
  int<lower=0> n;          // number of cells in the table
  int<lower=0> y[n];       // observed counts
  int<lower=0> p;          // number of slopes in the linear predictor to estimate

  // Covariates / model matrix, which come in as dummy 1/0 variables
  matrix[n, p] X;   
}

parameters {
  vector[p] beta; // there are p slopes in linear predictor to estimate 
}

transformed parameters {
  vector[n] mu;
  mu = exp(X * beta);
  }

model {
  // priors:
  beta[1] ~ cauchy(0,10); //prior for the intercept following Gelman 2008

  for(i in 2:p)
   beta[i] ~ cauchy(0,2.5);//prior for the slopes following Gelman 2008
   
   
  // main model
  y ~ poisson(mu);
}
