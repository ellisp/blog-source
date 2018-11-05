//Generalized linear model with poisson response for a cross tab of lions, tigers and bears with A, B, C, D
// with two missing values
//
// Currently doesn't work.  Change to follow 
// https://stackoverflow.com/questions/47756538/marginalize-over-missing-discrete-response-data-in-stan


data {
  int<lower=0> n;          // number of cells in the table
  int<lower=0> y[n];       // observed counts
  int<lower=0> p;          // number of slopes in the linear predictor to estimate

  // Covariates / model matrix, which come in as dummy 1/0 variables
  matrix[n, p] X;   
}

parameters {
  vector[p] beta; // slopes in linear predictor 
  
  // we'd like these to be integers, but parameters can't be integers
  real<upper=5> missing_tigers_b;
  real<upper=5> missing_bears_b; 
}

transformed parameters {
  // z is going to be a combination of the observed y and the two missing parameters.
  // note that their positions are hard coded in:
  vector[n] z;
  vector[n] mu;
  vector[n] sigma;
  
  for(i in 1:4)
    z[i] = y[i];
    
  for(i in 7:12)
    z[i] = y[i];
  
  z[5] = missing_tigers_b;
  z[6] = missing_bears_b;
  
  mu = exp(X * beta);
  sigma = sqrt(mu);
  
  }

model {
  // priors:
  beta[1] ~ cauchy(0,10); //prior for the intercept following Gelman 2008
  missing_tigers_b ~ gamma(4, 2);
  missing_bears_b ~ gamma(4, 2);

  for(i in 2:p)
   beta[i] ~ cauchy(0,2.5); //prior for the slopes following Gelman 2008
   
   
  // main model
  z ~ normal(mu, sigma);
}
