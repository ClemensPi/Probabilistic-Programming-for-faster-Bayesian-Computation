data {
  int<lower=0> N;            // Number of observations
  int<lower=0> K;            // Number of predictors
  int<lower=1> N_1;          // Number of groups
  int<lower=1,upper=N_1> J_1[N]; // Group indicator for each observation
  matrix[N, K] X;            // Predictor matrix
  vector[N] Y;               // Response variable
}

transformed data {
  real Syy;                  // Y' * Y
  row_vector[K] Syx;         // Y' * X
  matrix[K, K] Sxx;          // X' * X
  vector[N_1] u_count;       // Number of observations in each group
  vector[N_1] u_sumY;        // Sum of Y for each group
  matrix[N_1, K] u_sumX;     // Sum of X for each group

  Syy = dot_self(Y);         // Equivalent to Y' * Y
  Syx = Y' * X;              // Equivalent to Y' * X
  Sxx = crossprod(X);        // Equivalent to X' * X
  
  u_count = rep_vector(0.0, N_1);
  u_sumY = rep_vector(0.0, N_1);
  u_sumX = rep_matrix(0.0, N_1, K);
  
  for (n in 1:N) {
    u_count[J_1[n]] += 1;
    u_sumY[J_1[n]] += Y[n];
    u_sumX[J_1[n], ] += X[n, ];
  }
}

parameters {
  vector[K] beta;            // Coefficients for the predictors
  real<lower=0> sigma;       // Standard deviation of the residuals
  real<lower=0> tau;         // Standard deviation of the group-level effects
  vector[N_1] u;             // Group-level random intercepts
}

model {
  // Priors
  real lprior = 0;
  lprior += normal_lpdf(beta | 0, 5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.8)
    - 1 * student_t_lccdf(0 | 3, 0, 2.8);
  lprior += normal_lpdf(tau | 0, 1);
  lprior += normal_lpdf(u | 0, tau);

  // Adjust sufficient statistics for the group-level effects
  real Syy_adjusted = Syy;
  row_vector[K] Syx_adjusted = Syx;

  for (g in 1:N_1) {
    Syy_adjusted -= 2 * u[g] * u_sumY[g] - u[g]^2 * u_count[g];
    Syx_adjusted -= u[g] * u_sumX[g, ];
  }

  // Likelihood using sufficient statistics
  target += -N*log(sigma) 
            - (Syy_adjusted - 2 * Syx_adjusted * beta + beta' * Sxx * beta) / (2 * sigma^2);
  target += lprior;
}


