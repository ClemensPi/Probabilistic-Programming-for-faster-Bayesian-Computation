# Load necessary libraries
library(tidyverse)

my_dir <- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels"
library(microbenchmark)
library(rstan)
library(rstanarm)
rstan_options(auto_write = TRUE)

library(MASS)


setwd(my_dir)

set.seed(123)

# Load necessary libraries
library(brms)

# Set seed for reproducibility
set.seed(42)

# Parameters
n <- 100                 # Total number of observations
k <- 5                   # Number of groups
beta0 <- 0.5             # Fixed intercept
betas <- c(1.0, -1.5, 2.0, -2.5, 3.0) # Fixed slopes
sigma_u <- 1.0           # SD of random intercepts
sigma_epsilon <- 1.5     # Residual standard deviation

# Generate group assignments
group <- factor(rep(1:k, length.out = n))

# Generate random intercepts for each group
u <- rnorm(k, mean = 0, sd = sigma_u)
random_intercepts <- u[as.numeric(group)]

# Generate covariates
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rnorm(n, mean = 0, sd = 1)
x3 <- rnorm(n, mean = 0, sd = 1)
x4 <- rnorm(n, mean = 0, sd = 1)
x5 <- rnorm(n, mean = 0, sd = 1)

# Residual errors
epsilon <- rnorm(n, mean = 0, sd = sigma_epsilon)

# Response variable
y <- beta0 +
  betas[1] * x1 +
  betas[2] * x2 +
  betas[3] * x3 +
  betas[4] * x4 +
  betas[5] * x5 +
  random_intercepts +
  epsilon

# Combine into a data frame
simulated_data <- data.frame(
  y = y,
  x1 = x1,
  x2 = x2,
  x3 = x3,
  x4 = x4,
  x5 = x5,
  group = group
)

# Display the first few rows
head(simulated_data)

# Fit the Bayesian random effects model using brms
formula <- bf(y ~ x1 + x2 + x3 + x4 + x5 + (1+x1 + x2 + x3 + x4 + x5 | group))

# Fit the model
fit <- brm(
  formula = formula,
  data = simulated_data,
  family = gaussian(), # Assumes normal distribution for y
  chains = 4,          # Number of MCMC chains
  iter = 2000,         # Number of iterations per chain
  warmup = 1000,       # Number of warmup iterations
  cores = 2,           # Parallelization
  seed = 123            # Set seed for reproducibility
)

summary(fit)
stancode(fit)

library(rstan)

# Data preparation
N <- nrow(simulated_data)                     # Number of observations
K <- 6                                        # Number of population-level effects (Intercept + 5 predictors)
X <- as.matrix(cbind(1, simulated_data[, 2:6])) # Include intercept and predictors
Y <- simulated_data$y
group_idx <- as.integer(simulated_data$group) # Group assignments for random effects
N_1 <- length(unique(group_idx))              # Number of groups
M_1 <- K                                      # Number of coefficients per group (random slopes)

# Create Z matrices for group-level predictors
Z_1 <- X                                      # Assuming random slopes for all predictors

# Data list for Stan
stan_data <- list(
  N = N,
  Y = Y,
  K = K,
  X = X,
  Kc = K - 1,                                 # Without the intercept for centering
  N_1 = N_1,
  M_1 = M_1,
  J_1 = group_idx,
  Z_1_1 = Z_1[, 1], Z_1_2 = Z_1[, 2], Z_1_3 = Z_1[, 3], Z_1_4 = Z_1[, 4], Z_1_5 = Z_1[, 5], Z_1_6 = Z_1[, 6],
  NC_1 = (M_1 * (M_1 - 1)) / 2,               # Number of correlations
  prior_only = 0                              # Include data in likelihood
)

REM_BRMS_stan= stan_model("RandomEffectModel_withSlope_BRMS.stan")

REM_BRMS <- rstan::sampling(REM_BRMS_stan, stan_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                          cores = 1, seed = 123 )
