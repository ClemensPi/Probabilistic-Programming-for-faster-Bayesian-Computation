my_dir <- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels"
library(microbenchmark)
library(rstan)
library(rstanarm)
rstan_options(auto_write = TRUE)

library(MASS)


setwd(my_dir)

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

# Generate group-level predictors
group_level_predictor <- rnorm(G, mean = 0, sd = 1)  # Group-level predictor
Z_1_1 <- group_level_predictor[group]  # Assign group-level predictor to observations

# Prepare data for Stan
data_list <- list(
  Y = as.vector(Y),
  X = X,
  N = n,
  K = p,
  Kc = p - 1,
  N_1 = G,
  M_1 = 1,  # Only 1 coefficient for the group-level effects
  J_1 = as.integer(group),
  Z_1_1 = Z_1_1,  # Include group-level predictor
  prior_only = 0  # To include likelihood in the model
)


REM_BRMS_stan= stan_model("RandomEffectModel_BRMS.stan")
REM_Simple_stan= stan_model("RandomEffectModel_Simple.stan")
REM_SS_stan= stan_model("RandomEffectModel_SS.stan")

benchmark_REM_N100<-microbenchmark(
  REM_BRMS <- rstan::sampling(REM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  REM_SS <- rstan::sampling(REM_SS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                            cores = 1, seed = 123 ),
  REM_Simple <- rstan::sampling(REM_Simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                cores = 1, seed = 123 ),
  times=10
  
  
)

benchmark_REM_N100

# Now for N=300

set.seed(123)

# Number of observations
n <- 300


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

# Generate group-level predictors
group_level_predictor <- rnorm(G, mean = 0, sd = 1)  # Group-level predictor
Z_1_1 <- group_level_predictor[group]  # Assign group-level predictor to observations

# Prepare data for Stan
data_list <- list(
  Y = as.vector(Y),
  X = X,
  N = n,
  K = p,
  Kc = p - 1,
  N_1 = G,
  M_1 = 1,  # Only 1 coefficient for the group-level effects
  J_1 = as.integer(group),
  Z_1_1 = Z_1_1,  # Include group-level predictor
  prior_only = 0  # To include likelihood in the model
)

benchmark_REM_N300<-microbenchmark(
  REM_BRMS <- rstan::sampling(REM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  REM_SS <- rstan::sampling(REM_SS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                            cores = 1, seed = 123 ),
  REM_Simple <- rstan::sampling(REM_Simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                cores = 1, seed = 123 ),
  times=10
  
  
)

benchmark_REM_N300

# Now for N=1000

set.seed(123)

# Number of observations
n <- 1000


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

# Generate group-level predictors
group_level_predictor <- rnorm(G, mean = 0, sd = 1)  # Group-level predictor
Z_1_1 <- group_level_predictor[group]  # Assign group-level predictor to observations

# Prepare data for Stan
data_list <- list(
  Y = as.vector(Y),
  X = X,
  N = n,
  K = p,
  Kc = p - 1,
  N_1 = G,
  M_1 = 1,  # Only 1 coefficient for the group-level effects
  J_1 = as.integer(group),
  Z_1_1 = Z_1_1,  # Include group-level predictor
  prior_only = 0  # To include likelihood in the model
)

benchmark_REM_N1000<-microbenchmark(
  REM_BRMS <- rstan::sampling(REM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  REM_SS <- rstan::sampling(REM_SS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                            cores = 1, seed = 123 ),
  REM_Simple <- rstan::sampling(REM_Simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                cores = 1, seed = 123 ),
  times=10
  
  
)

benchmark_REM_N1000


# Now for changing P
set.seed(123)

# Number of observations
n <- 100
# Number of predictors
p <- 10
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

# Generate group-level predictors
group_level_predictor <- rnorm(G, mean = 0, sd = 1)  # Group-level predictor
Z_1_1 <- group_level_predictor[group]  # Assign group-level predictor to observations

# Prepare data for Stan
data_list <- list(
  Y = as.vector(Y),
  X = X,
  N = n,
  K = p,
  Kc = p - 1,
  N_1 = G,
  M_1 = 1,  # Only 1 coefficient for the group-level effects
  J_1 = as.integer(group),
  Z_1_1 = Z_1_1,  # Include group-level predictor
  prior_only = 0  # To include likelihood in the model
)

benchmark_REM_P10<-microbenchmark(
  REM_BRMS <- rstan::sampling(REM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  REM_SS <- rstan::sampling(REM_SS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                            cores = 1, seed = 123 ),
  REM_Simple <- rstan::sampling(REM_Simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                cores = 1, seed = 123 ),
  times=10
  
  
)


set.seed(123)

# Number of observations
n <- 100
# Number of predictors
p <- 20
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

# Generate group-level predictors
group_level_predictor <- rnorm(G, mean = 0, sd = 1)  # Group-level predictor
Z_1_1 <- group_level_predictor[group]  # Assign group-level predictor to observations

# Prepare data for Stan
data_list <- list(
  Y = as.vector(Y),
  X = X,
  N = n,
  K = p,
  Kc = p - 1,
  N_1 = G,
  M_1 = 1,  # Only 1 coefficient for the group-level effects
  J_1 = as.integer(group),
  Z_1_1 = Z_1_1,  # Include group-level predictor
  prior_only = 0  # To include likelihood in the model
)

benchmark_REM_P20<-microbenchmark(
  REM_BRMS <- rstan::sampling(REM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  REM_SS <- rstan::sampling(REM_SS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                            cores = 1, seed = 123 ),
  REM_Simple <- rstan::sampling(REM_Simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                cores = 1, seed = 123 ),
  times=10
)