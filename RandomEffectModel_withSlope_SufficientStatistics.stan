functions {
  /* Compute correlated group-level effects */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    return transpose(diag_pre_multiply(SD, L) * z);
  }
}

data {
  int<lower=1> N;            // Total number of observations
  vector[N] Y;               // Response variable
  int<lower=1> K;            // Number of predictors (includes intercept)
  matrix[N, K] X;            // Design matrix for predictors
  int<lower=1> N_1;          // Number of groups
  array[N] int<lower=1> J_1; // Group indicator for each observation
  
  int<lower=1> M_1;          // Number of coefficients per group
  int prior_only;            // Should the likelihood be ignored?
}

transformed data {
  real Syy;                  // Sum of squares for Y
  row_vector[K] Syx;         // Cross-product Y'X
  matrix[K, K] Sxx;          // Cross-product X'X
  
  // Group-level sufficient statistics
  vector[N_1] u_count;       // Observations per group
  vector[N_1] u_sumY;        // Sum of Y per group
  matrix[N_1, K] u_sumX;     // Sum of X per group
  
  // Initialize sufficient statistics
  Syy = dot_self(Y);
  Syx = Y' * X;
  Sxx = crossprod(X);
  
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
  vector[K] beta;               // Fixed effects
  real Intercept;                // Population-level intercept
  real<lower=0> sigma;           // Residual standard deviation
  vector<lower=0>[M_1] sd_1;     // Group-level standard deviations
  matrix[M_1, N_1] z_1;          // Standardized group-level effects
  cholesky_factor_corr[M_1] L_1; // Cholesky factor for correlation matrix
}

transformed parameters {
  matrix[N_1, M_1] r_1;          // Actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
}

model {
  // Priors
  real lprior = 0;
  lprior += student_t_lpdf(Intercept | 3, 0.5, 5.4);
  lprior += student_t_lpdf(sigma | 3, 0, 5.4)
            - 1 * student_t_lccdf(0 | 3, 0, 5.4);
  lprior += student_t_lpdf(sd_1 | 3, 0, 5.4)
            - M_1 * student_t_lccdf(0 | 3, 0, 5.4);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 1);
  
  // Likelihood using sufficient statistics
  if (!prior_only) {
    real Syy_adjusted = Syy;
    row_vector[K] Syx_adjusted = Syx;
    
    // Adjust for group-level effects
    for (g in 1:N_1) {
      Syy_adjusted -= 2 * r_1[g, 1] * u_sumY[g] - r_1[g, 1]^2 * u_count[g];
      Syx_adjusted -= r_1[g, 1] * u_sumX[g, ];
    }
    
    // Compute the log-likelihood
    target += -N * log(sigma) 
              - (Syy_adjusted - 2 * Syx_adjusted * beta + beta' * Sxx * beta) / (2 * sigma^2);
  }
  
  // Add priors to the target
  target += lprior;
  //target += std_normal_lpdf(to_vector(z_1));
}

