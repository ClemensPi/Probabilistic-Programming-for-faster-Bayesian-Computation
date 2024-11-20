# Load necessary libraries

my_dir <- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels"
library(microbenchmark)
library(rstan)
library(rstanarm)
rstan_options(auto_write = TRUE)

library(MASS)


setwd(my_dir)



LRM_Rethinking_stan = stan_model("UlamP10.stan")
LRM_simple_stan = stan_model("LRM_normal.stan")
LRM_SufficientStatistics_stan = stan_model("LRM_SufficientStatistics.stan")
LRM_BRMS_stan= stan_model("LRM_BRMS.stan")



# Generate synthetic data
set.seed(123)  # For reproducibility
n <- 100
p <- 10

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
# Function to create vectors X1, X2, ..., Xp from a matrix

create_vectors <- function(mat) {
  pre <- ncol(mat)  # Number of columns (predictors)
  for (i in 1:pre) {
    assign(paste0("X", i), mat[, i], envir = .GlobalEnv)
  }
}
mat <- matrix(rnorm(n * p), nrow = n, ncol = p)  # Create a random n x p matrix
create_vectors(mat)

data_ulam <- list(
  N=n,
  Y=Y,
  X1=X1,
  X2=X2,
  X3=X3,
  X4=X4,
  X5=X5,
  X6=X6,
  X7=X7,
  X8=X8,
  X9=X9,
  X10=X10
)

LRM_Rethinking <- rstan::sampling(LRM_Rethinking_stan, data_ulam, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 )

benchmark_LRM_N100<-microbenchmark(
  LRM_sampling <- rstan::sampling(LRM_simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_Rethinking <- rstan::sampling(LRM_Rethinking_stan, data_ulam, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_BRMS <- rstan::sampling(LRM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_SufficientStatistics <- rstan::sampling(LRM_SufficientStatistics_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_rstanarm <- stan_glm(
    Y ~ X,                 # Specify the formula
    data = data.frame(Y = Y, X = X),  # Provide the data in a data frame
    prior = hs(df = 1, global_scale = 0.5), # Horseshoe prior
    chains = 1,            # Number of MCMC chains
    iter = N_warmup+N_MCMC,           # Number of iterations per chain
    cores = 1,             # Number of cores to use
    seed = 123              # For reproducibility
  ),
  times = 10
  
)



set.seed(123)  # For reproducibility
n <- 1000

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
# Function to create vectors X1, X2, ..., Xp from a matrix

create_vectors <- function(mat) {
  pre <- ncol(mat)  # Number of columns (predictors)
  for (i in 1:pre) {
    assign(paste0("X", i), mat[, i], envir = .GlobalEnv)
  }
}
mat <- matrix(rnorm(n * p), nrow = n, ncol = p)  # Create a random n x p matrix
create_vectors(mat)

data_ulam <- list(
  N=n,
  Y=Y,
  X1=X1,
  X2=X2,
  X3=X3,
  X4=X4,
  X5=X5,
  X6=X6,
  X7=X7,
  X8=X8,
  X9=X9,
  X10=X10
)

benchmark_LRM_N1000<-microbenchmark(
  LRM_sampling <- rstan::sampling(LRM_simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_Rethinking <- rstan::sampling(LRM_Rethinking_stan, data_ulam, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                    cores = 1, seed = 123 ),
  LRM_BRMS <- rstan::sampling(LRM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  LRM_SufficientStatistics <- rstan::sampling(LRM_SufficientStatistics_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                              cores = 1, seed = 123 ),
  LRM_rstanarm <- stan_glm(
    Y ~ X,                 # Specify the formula
    data = data.frame(Y = Y, X = X),  # Provide the data in a data frame
    prior = hs(df = 1, global_scale = 0.5), # Horseshoe prior
    chains = 1,            # Number of MCMC chains
    iter = N_warmup+N_MCMC,           # Number of iterations per chain
    cores = 1,             # Number of cores to use
    seed = 123              # For reproducibility
  ),
  times = 1
  
)

set.seed(123)  # For reproducibility
n <- 10000

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
# Function to create vectors X1, X2, ..., Xp from a matrix

create_vectors <- function(mat) {
  pre <- ncol(mat)  # Number of columns (predictors)
  for (i in 1:pre) {
    assign(paste0("X", i), mat[, i], envir = .GlobalEnv)
  }
}
mat <- matrix(rnorm(n * p), nrow = n, ncol = p)  # Create a random n x p matrix
create_vectors(mat)

data_ulam <- list(
  N=n,
  Y=Y,
  X1=X1,
  X2=X2,
  X3=X3,
  X4=X4,
  X5=X5,
  X6=X6,
  X7=X7,
  X8=X8,
  X9=X9,
  X10=X10
)

benchmark_LRM_N10000<-microbenchmark(
  LRM_sampling <- rstan::sampling(LRM_simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_Rethinking <- rstan::sampling(LRM_Rethinking_stan, data_ulam, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                    cores = 1, seed = 123 ),
  LRM_BRMS <- rstan::sampling(LRM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  LRM_SufficientStatistics <- rstan::sampling(LRM_SufficientStatistics_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                              cores = 1, seed = 123 ),
  LRM_rstanarm <- stan_glm(
    Y ~ X,                 # Specify the formula
    data = data.frame(Y = Y, X = X),  # Provide the data in a data frame
    prior = hs(df = 1, global_scale = 0.5), # Horseshoe prior
    chains = 1,            # Number of MCMC chains
    iter = N_warmup+N_MCMC,           # Number of iterations per chain
    cores = 1,             # Number of cores to use
    seed = 123              # For reproducibility
  ),
  times = 10
  
)


benchmark_LRM_N100
benchmark_LRM_N1000
benchmark_LRM_N10000


baum<-microbenchmark(
  LRM_sampling <- rstan::sampling(LRM_simple_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  LRM_BRMS <- rstan::sampling(LRM_BRMS_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                              cores = 1, seed = 123 ),
  LRM_SufficientStatistics <- rstan::sampling(LRM_SufficientStatistics_stan, data_list, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                              cores = 1, seed = 123 ),
  times = 1
  
)
baum
