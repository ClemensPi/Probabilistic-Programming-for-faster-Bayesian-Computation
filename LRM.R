# Load necessary libraries
library(rstan)
library(microbenchmark)

# Generate synthetic data
set.seed(123)  # For reproducibility
n <- 1000
p <- 100

# Predictor matrix X
X <- matrix(rnorm(n * p), n, p)

# True regression coefficients
beta <- c(1.5, 2, 2.5, rep(0, p - 3))

# Generate response variable Y with noise
Y <- X[,1] * beta[1] + X[,2] * beta[2] + X[,3] * beta[3] + rnorm(n)
beta
# Center the predictors (excluding the intercept)
K <- p
Kc <- K - 1
means_X <- colMeans(X[, 2:K])  # Column means for centering
Xc <- sweep(X[, 2:K], 2, means_X)  # Centering columns 2 to K

# Prepare the data list for Stan
data_list <- list(
  N = n, 
  Y = as.vector(Y), 
  K = K, 
  Kc = Kc,
  X = X,
  Xc = Xc,
  means_X = means_X,
  prior_only = 0  # Set to 1 to ignore likelihood, 0 otherwise
)
getwd()

# Specify Stan model file
stan_BRMS<- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels/LRM_BRMS.stan"
stan_LRM_normal<- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels/LRM_BRMS.stan"
stan_LRM_SS<- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels/LRM_BRMS.stan"

testR <- stan(file = stan_BRMS, data = data_list)

stan_model_file <- "LRM_BRMS.stan"

# Compile and fit the model using rstan
fit <- stan(
  file = stan_model_file,
  data = data_list,
  iter = 1000,
  chains = 4,
  seed = 123
)

# Print summary of results
print(summary(fit))
