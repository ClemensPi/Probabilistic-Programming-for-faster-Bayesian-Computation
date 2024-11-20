library(rethinking)

# Simulate data
set.seed(123)

# Parameters for simulation
n_groups <- 10         # Number of groups
n_per_group <- 50      # Observations per group
n <- n_groups * n_per_group  # Total observations

# Group identifiers
cid <- rep(1:n_groups, each = n_per_group)

# Global parameters
a_bar <- 1.0           # Population mean intercept
b_bar <- 2.0           # Population mean slope
sigma_a <- 0.5         # SD of group intercepts
sigma_b <- 0.3         # SD of group slopes
sigma <- 0.8           # Residual SD

# Group-level parameters
a <- rnorm(n_groups, a_bar, sigma_a)  # Random intercepts
b <- rnorm(n_groups, b_bar, sigma_b)  # Random slopes

# Predictor variable
rugged_std_c <- rnorm(n)

# Response variable
log_gdp_std <- a[cid] + b[cid] * rugged_std_c + rnorm(n, 0, sigma)

# Combine into a data frame
simulated_data <- data.frame(
  log_gdp_std = log_gdp_std,
  rugged_std_c = rugged_std_c,
  cid = cid
)

# Prepare data list for ulam
data_list <- list(
  log_gdp_std = simulated_data$log_gdp_std,
  rugged_std_c = simulated_data$rugged_std_c,
  cid = simulated_data$cid,
  N = nrow(simulated_data),
  G = n_groups
)

# Fit the model
model_REM <- ulam(
  alist(
    # Likelihood
    log_gdp_std ~ dnorm(mu, sigma),
    
    # Linear predictor
    mu <- a[cid] + b[cid] * rugged_std_c,
    
    # Random effects
    a[cid] ~ dnorm(a_bar, sigma_a),   # Random intercepts
    b[cid] ~ dnorm(b_bar, sigma_b),   # Random slopes
    
    # Hyperpriors for group-level distributions
    a_bar ~ dnorm(0, 1),
    b_bar ~ dnorm(0, 1),
    sigma_a ~ dexp(1),
    sigma_b ~ dexp(1),
    
    # Residual SD
    sigma ~ dexp(1)
  ), data = data_list, chains = 4, cores = 4, iter = 2000, warmup = 1000
)

# Summarize results
precis(model, depth = 2)
stancode(model_REM)




library(rethinking)

# Simulate data
set.seed(42)
N <- 100
X1 <- rnorm(N, 0, 1)
X2 <- rnorm(N, 0, 1)
beta_0 <- 2
beta_1 <- 1.5
beta_2 <- -0.5
sigma <- 1
Y <- beta_0 + beta_1 * X1 + beta_2 * X2 + rnorm(N, 0, sigma)

# Standardize predictors
X1_std <- (X1 - mean(X1)) / sd(X1)
X2_std <- (X2 - mean(X2)) / sd(X2)


# Create the data list for ulam
data_list <- list(
  Y = Y,
  X1 = X1_std,
  X2 = X2_std,
  N = N
)

# Define the model
model <- ulam(
  alist(
    Y ~ dnorm(mu, sigma),  # Likelihood
    mu <- b0 + b1 * X1 + b2 * X2,  # Linear predictor
    b0 ~ dnorm(0, 1),  # Prior for intercept
    b1 ~ dnorm(0, 1),  # Prior for coefficient of X1
    b2 ~ dnorm(0, 1),  # Prior for coefficient of X2
    sigma ~ dexp(1)    # Prior for residual standard deviation
  ),
  data = data_list,
  chains = 4,
  cores = 4
)


# Print summary of results
precis(model)

# Extract posterior samples
posterior <- extract.samples(model)

# Check posterior means for coefficients
mean(posterior$b0)  # Intercept
mean(posterior$b1)  # Coefficient for X1
mean(posterior$b2)  # Coefficient for X2


stancode(model)




library(rethinking)
set.seed(42)

# Simulate data parameters
N <- 500  # Total number of observations
dept <- sample(1:6, N, replace = TRUE)  # Departments (1 to 6)
male <- rbinom(N, 1, 0.5)  # Gender: male (1) or female (0)
applications <- sample(10:50, N, replace = TRUE)  # Number of applications

# True values for global effects and random effects
v_mu <- c(0.5, -0.2)  # Global intercept and slope
sigma <- c(1, 0.5)  # Random effects standard deviations
rho <- 0.3  # Correlation between random intercepts and slopes
L_Rho <- chol(matrix(c(1, rho, rho, 1), 2, 2))  # Cholesky factor of correlation

# Generate random effects for departments
z <- matrix(rnorm(6 * 2, 0, 1), ncol = 2)  # Standard normal random effects
v <- t(diag(sigma) %*% L_Rho %*% t(z))  # Transform to include sigma and correlation

# Simulate admit probabilities and outcomes
logit_p <- v_mu[1] + v[dept, 1] + (v_mu[2] + v[dept, 2]) * male
p <- inv_logit(logit_p)
admit <- rbinom(N, applications, p)  # Generate admissions

# Prepare data list for ulam
data_list <- list(
  admit = admit,
  applications = applications,
  male = male,
  dept = dept,
  N = N,
  dept_count = length(unique(dept))
)

#This are example Codes I found in the internet:
# https://rpruim.github.io/s341/S21/from-class/notes/18-Multiple-Regression.html
#and
# https://www.rdocumentation.org/packages/rethinking/versions/2.13/topics/ulam

#For varying INtercept


#For varying slopes

#For varying Sigmas
# Define the model
m_glmm5 <- ulam(
  alist(
    admit ~ binomial(applications, p),  # Likelihood
    logit(p) <- v_mu[1] + v[dept, 1] + (v_mu[2] + v[dept, 2]) * male,  # Logit link
    matrix[dept_count, 2]: v <- t(diag_pre_multiply(sigma, L_Rho) * z),  # Random effects
    matrix[2, dept_count]: z ~ normal(0, 1),  # Standard normal random effects
    vector[2]: v_mu ~ normal(0, 4),  # Priors for global effects
    vector[2]: sigma ~ dexp(1),  # Priors for random effects SD
    cholesky_factor_corr[2]: L_Rho ~ lkj_corr_cholesky(2)  # Prior for correlation
  ),
  data = data_list,
  chains = 4,
  cores = 4
)

# Summarize the model results
precis(m_glmm5, depth = 2)

# Posterior samples
posterior <- extract.samples(m_glmm5)

# Inspect posterior means
posterior_means <- list(
  v_mu = colMeans(posterior$v_mu),
  sigma = colMeans(posterior$sigma),
  L_Rho = colMeans(posterior$L_Rho)
)
posterior_means
stancode(m_glmm5)




