data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of fixed effects
  matrix[N, K] X;  // design matrix for fixed effects
  int<lower=1> N_1;  // number of groups
  array[N] int<lower=1> J_1;  // group indicators for random effects
  int<lower=1> M_1;  // number of random effects per group
}
parameters {
  vector[K] beta;  // fixed effects coefficients
  real<lower=0> sigma;  // residual standard deviation
  vector<lower=0>[M_1] sd_1;  // standard deviations for random effects
  cholesky_factor_corr[M_1] L_1;  // Cholesky factor of the correlation matrix
  matrix[M_1, N_1] z_1;  // standardized random effects
}
transformed parameters {
  matrix[M_1, N_1] u;  // random effects
  u = diag_pre_multiply(sd_1, L_1) * z_1;
}
model {
  // Priors
  beta ~ normal(0, 1);  // fixed effects priors
  sd_1 ~ normal(0, 1);  // priors for random effect standard deviations
  L_1 ~ lkj_corr_cholesky(1);  // prior for correlation matrix
  to_vector(z_1) ~ normal(0, 1);  // standard normal priors for z_1

  // Likelihood
  for (n in 1:N) {
    Y[n] ~ normal(X[n] * beta + u[1, J_1[n]], sigma);
  }
}
