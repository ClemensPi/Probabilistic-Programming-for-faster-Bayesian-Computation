data {
  int<lower=0> N;  // Number of observations
  int<lower=0> P; 
  matrix[N,P] X;  // Predictor
  vector[N] Y;  // Response
}

transformed data {
  real Syy;
  row_vector[P] Syx;
  matrix[P,P] Sxx;
  Syy=Y'*Y;
  Syx=Y'*X;
  Sxx=crossprod(X);
}

parameters {
  vector[P] beta;          // Slope
  real<lower=0> sigma; // Standard deviation
}

model {
  // Priors:
  beta ~ normal(0, 1);
  sigma ~ cauchy(0, 1);
  // Likelihood:
    target += -N*log(sigma)-(Syy-2*Syx*beta+beta'*Sxx*beta)/(2*sigma^2);

    
}
/*
generated quantities {
  vector[N] y_new;  // New predicted values for the same X
  
  for (n in 1:N){
    y_new[n] = normal_rng(X[n,]*beta, sigma);
  }
}
*/

