library(brms)
library(rstan)
library(microbenchmark)

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

