data {
  int<lower=1> N;         // Total number of observations
  vector[N] Y;            // Response variable
  int<lower=1> K;         // Number of population-level effects
  matrix[N, K] X;         // Design matrix for predictors
}

parameters {
  vector[K] beta;          // Regression coefficients
  real<lower=0> sigma;     // Standard deviation
  real Intercept;          // Intercept for centered predictors
}

transformed parameters {
  real lprior = 0;         // Prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 3.7);
  lprior += student_t_lpdf(sigma | 3, 0, 3.7)
    - student_t_lccdf(0 | 3, 0, 3.7);
}

model {
  // Likelihood including constants
  target += normal_id_glm_lpdf(Y | X, Intercept, beta, sigma);
  // Priors including constants
  target += lprior;
}


