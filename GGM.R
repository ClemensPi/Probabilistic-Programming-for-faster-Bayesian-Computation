library(glasso)
library(BayesianGLasso)
library(rstan)
library(microbenchmark)
library(MASS)
library(Matrix)

GG_SS<- '/home/clemens/Documents/BA/finalModels/GG_S.stan'
GG_normal<- '/home/clemens/Documents/BA/finalModels/GG_normal.stan'

set.seed(1643725)
N=50
P=5
# Create a random positive definite matrix for the precision matrix
random_matrix <- matrix(rnorm(P^2), P, P)
precision_matrix <- crossprod(random_matrix)
precision_matrix
# Convert precision matrix to covariance matrix
covariance_matrix <- solve(precision_matrix)

# Generate multivariate normal data
y <- mvrnorm(n = N, mu = rep(0, P), Sigma = covariance_matrix)

# Display the true precision matrix for reference
print("True Precision Matrix:")
print(precision_matrix)

# Display the covariance matrix for reference
print("Covariance Matrix:")
print(covariance_matrix)

# Display the first few rows of the data
head(data)

p=5
y_50 <- mvrnorm(n = 50, mu = rep(0, p), Sigma = covariance_matrix)
y_100 <- mvrnorm(n = 100, mu = rep(0, p), Sigma = covariance_matrix)
y_200 <- mvrnorm(n = 200, mu = rep(0, p), Sigma = covariance_matrix)
y_1000 <- mvrnorm(n = 1000, mu = rep(0, p), Sigma = covariance_matrix)



r <- 1
s <- 0.01

# Create list for Stan

p5_data_50 <- list(
  N = 50,
  P = 5,
  y = y_P_5,
  r = r,
  s = s
)

p10_data_50 <- list(
  N = 50,
  P = 10,
  y = y_P_10,
  r = r,
  s = s
)

p20_data_50 <- list(
  N = 50,
  P = 20,
  y = y_P_20,
  r = r,
  s = s
)

p5_data_100 <- list(
  N = 100,
  P = P,
  y = y_100,
  r = r,
  s = s
)

p5_data_200 <- list(
  N = 200,
  P = P,
  y = y_200,
  r = r,
  s = s
)

p5_data_1000 <- list(
  N = 1000,
  P = P,
  y = y_1000,
  r = r,
  s = s
)
# first we test for constant p and changing N
benchmark_test_p_5_N_50<- microbenchmark(
  Normal <- stan(file=GG_normal, data = p5_data_50),
  Sufficient_Statistics <- stan(file=GG_SS, data = p5_data_50),
  times = 10
)

benchmark_test_p_5_N_100<- microbenchmark(
  Normal <- stan(file=GG_normal, data = p5_data_100),
  Sufficient_Statistics <- stan(file=GG_SS, data = p5_data_100),
  times = 10
)

benchmark_test_p_5_N_200<- microbenchmark(
  Normal <- stan(file=GG_normal, data = p5_data_200),
  Sufficient_Statistics <- stan(file=GG_SS, data = p5_data_200),
  times = 10
)

benchmark_test_p_5_N_1000<- microbenchmark(
  Normal <- stan(file=GG_normal, data = p5_data_1000),
  Sufficient_Statistics <- stan(file=GG_SS, data = p5_data_1000),
  times = 10
)

benchmark_test_p_5_N_50
benchmark_test_p_5_N_100
benchmark_test_p_5_N_200
benchmark_test_p_5_N_1000



benchmark_test_p_10_N_50<- microbenchmark(
  Normal <- stan(file=GG_normal, data = p5_data_50 ),
  Sufficient_Statistics <- stan(file=GG_SS, data = p5_data_50 ),
  times = 10
)

benchmark_test_p_20_N_50<- microbenchmark(
  Normal <- stan(file=GG_normal, data = p20_data_50 ),
  Sufficient_Statistics <- stan(file=GG_SS, data = p20_data_50 ),
  times = 10
)


#taking a look at the calculated Precision matrix
plot(Sufficient_Statistics, par=c("K"))

plot(Normal, par=c("K"))