data {
  int<lower=1> N;            // total number of observations
  vector[N] Y;               // response variable
  int<lower=1> K;            // number of population-level effects
  matrix[N, K] X;            // population-level design matrix
  int<lower=1> Kc;           // number of population-level effects after centering
  int<lower=1> N_1;          // number of grouping levels
  int<lower=1> M_1;          // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
}

parameters {
  vector[K] b;               // regression coefficients
  real Intercept;            // overall intercept
  vector[N_1] u;             // random intercepts for groups
  real<lower=0> sigma;       // residual standard deviation
  real<lower=0> tau;         // standard deviation of group-level effects
}

model {
  // Priors
  b ~ normal(0, 1);          // prior for regression coefficients
  Intercept ~ normal(0, 1);  // prior for intercept
  u ~ normal(0, tau);        // prior for random intercepts
  tau ~ normal(0, 1);        // prior for group-level standard deviation
  sigma ~ normal(0, 1);      // prior for residual standard deviation

  // Likelihood
  for (n in 1:N) {
    Y[n] ~ normal(Intercept + X[n] * b + u[J_1[n]], sigma);
  }
}
