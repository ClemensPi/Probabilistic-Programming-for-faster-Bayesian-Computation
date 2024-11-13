library(brms)
library(rstan)
library(microbenchmark)
# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 100
# Number of predictors
p <- 5
# Number of groups
G <- 5

# Generate random predictors
X <- matrix(rnorm(n * p), n, p)

# Define true coefficients
beta <- c(1, 2, -1, rep(0, p - 3))  # Only some predictors are non-zero

# Generate group indicators
group <- sample(1:G, n, replace = TRUE)
# Generate group-level effects
group_intercepts <- rnorm(G, mean = 0, sd = 1)
random_effects <- group_intercepts[group]

# Generate response variable
Y <- X %*% beta + random_effects + rnorm(n)

# Prepare data for Stan
data_list <- list(
  Y = as.vector(Y),
  X = X,
  N = n,
  K = p,
  N_1 = G,
  M_1 = 1,  # Only 1 coefficient for the group-level effects
  J_1 = as.integer(group),
  prior_only = 0  # To include likelihood in the model
)


stan_BRMS <- 'C:/Users/Clemens/OneDrive/Dokumente/Dokumente/BA_models/MEM_BRMS.stan'
stan_SS <- 'C:/Users/Clemens/OneDrive/Dokumente/Dokumente/BA_models/MEM_SS.stan'


MEM_Benchmark<-microbenchmark(
  BRMS <- stan(file = stan_BRMS, data = data_list),
  Sufficient_statistics <- stan(file = stan_SS, data = data_list),
  times=10
)

MEM_Benchmark

BRMS
Sufficient_statistics

