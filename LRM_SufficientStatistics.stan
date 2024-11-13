data {
  int<lower=1> N;         // Total number of observations
  vector[N] Y;            // Response variable
  int<lower=1> K;         // Number of population-level effects
  matrix[N, K] X;         // Design matrix for predictors
}

transformed data {
  real Syy;                  // Sum of squares for Y
  row_vector[K] Syx;         // Cross-product of Y and X
  matrix[K, K] Sxx;          // Cross-product of X with itself

  Syy = Y' * Y;
  Syx = Y' * X;
  Sxx = crossprod(X);
}

parameters {
  vector[K] beta;          // Regression coefficients
  real<lower=0> sigma;     // Standard deviation
}

model {
  // Priors
  beta ~ normal(0, 1);
  sigma ~ cauchy(0, 1);

  // Likelihood
  target += -N * log(sigma) - (Syy - 2 * Syx * beta + beta' * Sxx * beta) / (2 * sigma^2);
}


