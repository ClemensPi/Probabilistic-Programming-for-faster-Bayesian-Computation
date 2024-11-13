data {
  int<lower=1> N;            // total number of observations
  vector[N] Y;               // response variable
  int<lower=1> K;            // number of population-level effects
  matrix[N, K] X;            // population-level design matrix
  int<lower=1> Kc;           // number of population-level effects after centering
  int<lower=1> N_1;          // number of grouping levels
  int<lower=1> M_1;          // number of coefficients per level (random effects)
  array[N] int<lower=1> J_1; // grouping indicator per observation
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  int<lower=1> NC_1;         // number of group-level correlations
  int prior_only;            // should the likelihood be ignored?
}

transformed data {
  // Sufficient statistics
  real Syy = dot_self(Y);                     // Y' * Y
  matrix[Kc, Kc] Sxx = crossprod(X[, 2:K]);   // Xc' * Xc
  row_vector[Kc] Syx = Y' * X[, 2:K];         // Y' * Xc

  // Group-specific sufficient statistics
  vector[N_1] u_count = rep_vector(0.0, N_1);
  vector[N_1] u_sumY = rep_vector(0.0, N_1);
  vector[N_1] u_sumZ1 = rep_vector(0.0, N_1);
  vector[N_1] u_sumZ2 = rep_vector(0.0, N_1);
  matrix[N_1, Kc] u_sumX = rep_matrix(0.0, N_1, Kc);

  for (n in 1:N) {
    u_count[J_1[n]] += 1;
    u_sumY[J_1[n]] += Y[n];
    u_sumZ1[J_1[n]] += Z_1_1[n];
    u_sumZ2[J_1[n]] += Z_1_2[n];
    u_sumX[J_1[n], ] += X[n, 2:K];
  }
}

parameters {
  vector[Kc] b;                         // regression coefficients
  real Intercept;                       // temporary intercept for centered predictors
  real<lower=0> sigma;                  // dispersion parameter
  vector<lower=0>[M_1] sd_1;            // group-level standard deviations
  matrix[M_1, N_1] z_1;                 // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;        // cholesky factor of correlation matrix
}

transformed parameters {
  matrix[N_1, M_1] r_1;                 // actual group-level effects
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  real lprior = 0;

  // Compute actual group-level effects using the Cholesky factor
  r_1 = (diag_pre_multiply(sd_1, L_1) * z_1)';
  
  // Extract the individual random effects
  r_1_1 = r_1[, 1];
  r_1_2 = r_1[, 2];

  // Prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 5.3, 4);
  lprior += student_t_lpdf(sigma | 3, 0, 4) - student_t_lccdf(0 | 3, 0, 4);
  lprior += student_t_lpdf(sd_1 | 3, 0, 4) - 2 * student_t_lccdf(0 | 3, 0, 4);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 1);
}

model {
  if (!prior_only) {
    // Adjust sufficient statistics for group-level effects
    real Syy_adjusted = Syy;
    row_vector[Kc] Syx_adjusted = Syx;

    for (g in 1:N_1) {
      Syy_adjusted -= 2 * (r_1_1[g] * u_sumZ1[g] + r_1_2[g] * u_sumZ2[g]) * u_sumY[g];
      Syy_adjusted += (r_1_1[g]^2 + r_1_2[g]^2) * u_count[g];
      Syx_adjusted -= r_1_1[g] * u_sumZ1[g] * u_sumX[g, ] + r_1_2[g] * u_sumZ2[g] * u_sumX[g, ];
    }

    // Likelihood using sufficient statistics
    target += -N * log(sigma)
              - (Syy_adjusted - 2 * Syx_adjusted * b + b' * Sxx * b) / (2 * sigma^2);
  }
  
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
}



