library(brms)
library(rstan)
library(microbenchmark)

set.seed(123)  # For reproducibility

# Parameters
N <- 1000  # Number of observations
K <- 5    # Number of predictors
G <- 10   # Number of grouping levels (N_1)
M_1 <- 2  # Number of coefficients per level (random effects)
Kc <- K - 1  # Number of predictors after centering
NC_1 <- M_1  # Number of group-level correlations

# Generate random predictors
X <- matrix(rnorm(N * K), N, K)

# Define true regression coefficients
beta <- c(1, -0.5, 0.3, rep(0, K - 3))  # Some coefficients set to zero for sparsity

# Generate group indicators
J_1 <- sample(1:G, N, replace = TRUE)

# Generate random group-level effects
sd_1_true <- runif(M_1, 0.5, 1.5)
L_1_true <- matrix(c(1, 0.3, 0.3, 1), M_1, M_1)  # Cholesky factor for correlations
z_1_true <- matrix(rnorm(G * M_1), M_1, G)
r_1_true <- (diag(sd_1_true) %*% L_1_true %*% z_1_true)  # Group effects

# Generate random effect predictors
Z_1_1 <- rnorm(N)
Z_1_2 <- rnorm(N)

# Generate group-level random effects for each observation
r_1_1 <- r_1_true[1, J_1]
r_1_2 <- r_1_true[2, J_1]

# Generate response variable
mu <- X %*% beta + r_1_1 * Z_1_1 + r_1_2 * Z_1_2
Y <- mu + rnorm(N, sd = 1)

# Center the predictors
means_X <- colMeans(X[, -1])
Xc <- sweep(X[, -1], 2, means_X)

# Prepare the data list for Stan
data_list <- list(
  N = N,
  Y = as.vector(Y),
  K = K,
  X = X,
  Kc = Kc,
  Xc = Xc,
  N_1 = G,
  M_1 = M_1,
  J_1 = as.integer(J_1),
  Z_1_1 = Z_1_1,
  Z_1_2 = Z_1_2,
  NC_1 = NC_1,
  prior_only = 0
)

# Display the structure of the generated data list
data_list

stan_BRMS <- 'C:/Users/Clemens/OneDrive/Dokumente/Dokumente/BA_models/MEM_BRMS2.stan'
stan_SS <- 'C:/Users/Clemens/OneDrive/Dokumente/Dokumente/BA_models/MEM_SS2.stan'


MEM_Benchmark<-microbenchmark(
  BRMS <- stan(file = stan_BRMS, data = data_list),
  Sufficient_statistics <- stan(file = stan_SS, data = data_list),
  times=1
)


MEM_Benchmark

