data {
   int<lower=1> N;        // Number of observations
   int<lower=1> P;        // Dimension of the covariance matrix
   matrix[N, P] y;        // Observed data matrix
   real<lower=0> r;       // Hyperparameter for the Gamma distribution
   real<lower=0> s;       // Hyperparameter for the Gamma distribution
}
transformed data {               // Precision matrix (inverse of the covariance matrix)
   matrix[P, P] Syy;             // Product of the observed data matrix
   Syy = y' * y;
}
parameters {
   vector[P] mu;                 // Group means
   cov_matrix[P] K;              // Correlation matrix
   real<lower=0> lambda;         // Hyperparameter for the prior
}

model {
   // Prior for lambda
   target += gamma_lpdf(lambda | r, s); 

   // Prior for precision matrix K (DE(ωij | λ) for off-diagonal elements, Exp(ωii | λ^2) for diagonal elements)
   for (i in 1:(P-1)) {
      for (j in (i + 1):P) {
         target += double_exponential_lpdf(K[i, j] | 0, 1 / lambda);
      }
   }

   target += exponential_lpdf(diagonal(K) | lambda / 2);
  

   // Likelihood term
   target += N * (log_determinant(K)) - trace(Syy * K);

   // Priors for mu
   target += student_t_lpdf(mu | 3, 0, 1);
}

