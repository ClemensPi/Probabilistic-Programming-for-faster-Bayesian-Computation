data {
  int<lower=0> N;  // Number of observations
  int<lower=0> P; 
  matrix[N,P] X;  // Predictor
  vector[N] Y;  // Response
}

parameters {
  vector[P] beta;          // Slope
  real<lower=0> sigma; // Standard deviation
  real Intercept;  // temporary intercept for centered predictors

}

transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 3.7);
  lprior += student_t_lpdf(sigma | 3, 0, 3.7)
    - 1 * student_t_lccdf(0 | 3, 0, 3.7);
}

model {
  // likelihood including constants
    target += normal_id_glm_lpdf(Y | X, Intercept, beta, sigma);
  // priors including constants
  target += lprior;
}
/*
generated quantities {
  vector[N] log_lik;
  vector[N] y_new;  // New predicted values for the same X
  
  for (n in 1:N){
    log_lik[n] = normal_lpdf(Y[n] | X[n, ] * beta, sigma);
    y_new[n] = normal_rng(X[n,]*beta, sigma);
  } 
}
*/
