library(brms)
library(rstan)
library(microbenchmark)
library(dplyr)
#Linear Regression Model
n = 100
p = 10
X = matrix(rnorm(n * p), n, p)
beta = c(1.5, 2, 2.5, rep(0,p-3))
Y = X[,1] * beta[1] + X[,2] * beta[2] + X[,3] * beta[3] + rnorm(n)
Y

formula <- bf(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)

# Create the data frame
data_paul <- data.frame(
  Y = Y,          # Response variable
  N = n,          # Number of observations
  P = p           # Number of predictors
)

for (i in 1:p) {
  col_name <- paste0("X", i)
  data_paul[[col_name]] <- X[, i]
}

fit <- brm(formula, data = data_paul)

stancode(fit)

#Mixed Effects model


# Parameters
n <- 100   # Number of observations
p <- 10    # Number of predictors
G <- 5     # Number of groups

# Generate random predictors
X <- matrix(rnorm(n * p), n, p)

# Define true coefficients
beta <- c(1.5, 2, 2.5, rep(0, p - 3))  # Only first 3 predictors are non-zero

# Generate group indicators
group <- factor(sample(1:G, n, replace = TRUE))

# Generate random group-level effects (random intercepts)
group_intercepts <- rnorm(G, mean = 0, sd = 1)
random_effects <- group_intercepts[group]

# Generate the response variable
Y <- X[, 1] * beta[1] + X[, 2] * beta[2] + X[, 3] * beta[3] + random_effects + rnorm(n)

# Create the data frame
data_mixed <- data.frame(
  Y = Y,
  group = group,
  N = n,
  P = p
)

# Add the predictors to the data frame
for (i in 1:p) {
  col_name <- paste0("X", i)
  data_mixed[[col_name]] <- X[, i]
}

# Define the mixed-effects formula
# Fixed effects for predictors and random intercept for groups
formula_mixed <- bf(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + (1 | group))

# Fit the mixed-effects model using `brms`
fit_mixed <- brm(formula_mixed, data = data_mixed)

# Display the Stan code for the fitted model
stancode(fit_mixed)




#Mixed Effect model with Correlation Matrix as Prior:

# Parameters
n <- 100  # Number of observations
p <- 5    # Number of predictors
G <- 10   # Number of groups

# Generate random predictors
X <- matrix(rnorm(n * p), n, p)

# Define true coefficients for the population-level effects
beta <- c(2.0, -1.5, 1.2, 0.8, -0.5)

# Generate group indicators
group <- factor(sample(1:G, n, replace = TRUE))

# Generate group-level random effects with random slopes
group_intercepts <- rnorm(G, mean = 0, sd = 1)
group_slopes <- matrix(rnorm(G * p, mean = 0, sd = 0.5), G, p)

# Generate the response variable with group-specific random effects
random_effects <- sapply(1:n, function(i) {
  group_intercepts[as.integer(group[i])] +
    sum(group_slopes[as.integer(group[i]), ] * X[i, ])
})

# Generate response variable with noise
Y <- X %*% beta + random_effects + rnorm(n, sd = 2)

# Create data frame for brms
data <- data.frame(
  Y = as.vector(Y),
  group = group
)

# Add the predictors to the data frame
for (i in 1:p) {
  col_name <- paste0("X", i)
  data[[col_name]] <- X[, i]
}

# Fit the mixed-effects model using brms
fit <- brm(
  formula = bf(Y ~ X1 + X2 + X3 + X4 + X5 + (1 + X1 + X2 + X3 + X4 + X5 | group)),
  data = data,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("cauchy(0, 5)", class = "sd"),
    set_prior("cauchy(0, 5)", class = "sigma")
  ),
  chains = 4, cores = 4, iter = 2000
)

# Display the summary of the fitted model
summary(fit)

# Display the underlying Stan code
stancode(fit)
