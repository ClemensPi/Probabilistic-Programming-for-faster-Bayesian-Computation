my_dir <- "C:/Users/Clemens/OneDrive/Dokumente/Uni/BaA/finalModels"
library(microbenchmark)
library(rstan)
rstan_options(auto_write = TRUE)

library(MASS)


setwd(my_dir)



BayesianFactorAnalysis_stan = stan_model("BayesianFactorAnalysis.stan")
BayesianFactorAnalysis_SuffStat_stan = stan_model("BayesianFactorAnalysis_SuffStat.stan")
BayesianFactorAnalysis_SuffStat_Woodbury_stan = stan_model("BayesianFactorAnalysis_SuffStat_Woodbury.stan")


set.seed(42)
D <- 5
P <- 10 
N <- 300


Psi <- diag(rexp(P, 1))
l1 <- round(rnorm(P, 0, 0.5), 2)
l2 <- c(0.00, round(rnorm(P-1, 0, 0.5), 2))
l3 <- c(0.00, 0.00, round(rnorm(P-2, 0, 0.5), 2))
l4 <- c(0.00, 0.00, 0.00, round(rnorm(P-3, 0, 0.5), 2))
l5 <- c(0.00, 0.00, 0.00, 0.00, round(rnorm(P-4, 0, 0.5), 2))
L <- cbind(l1,l2,l3, l4, l5) # the loading matrix
# Needs to have upper triangle of 0

Theta <- mvrnorm(N, rep(0,D), diag(rep(1,D))) # sample factor scores
Epsilon <-mvrnorm(N, rep(0,P), Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data
L

BFA_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_Woodbury_data <- list(P=P, N=N, Y=Y, D=D)

benchmark_BFA_P <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)

benchmark_BFA_P

set.seed(42)
D <- 5
P <- 20 
N <- 300


Psi <- diag(rexp(P, 1))
l1 <- round(rnorm(P, 0, 0.5), 2)
l2 <- c(0.00, round(rnorm(P-1, 0, 0.5), 2))
l3 <- c(0.00, 0.00, round(rnorm(P-2, 0, 0.5), 2))
l4 <- c(0.00, 0.00, 0.00, round(rnorm(P-3, 0, 0.5), 2))
l5 <- c(0.00, 0.00, 0.00, 0.00, round(rnorm(P-4, 0, 0.5), 2))
L <- cbind(l1,l2,l3, l4, l5) # the loading matrix
# Needs to have upper triangle of 0

Theta <- mvrnorm(N, rep(0,D), diag(rep(1,D))) # sample factor scores
Epsilon <-mvrnorm(N, rep(0,P), Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data
L

BFA_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_Woodbury_data <- list(P=P, N=N, Y=Y, D=D)

benchmark_BFA_P <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)


set.seed(42)
D <- 5
P <- 40 
N <- 300


Psi <- diag(rexp(P, 1))
l1 <- round(rnorm(P, 0, 0.5), 2)
l2 <- c(0.00, round(rnorm(P-1, 0, 0.5), 2))
l3 <- c(0.00, 0.00, round(rnorm(P-2, 0, 0.5), 2))
l4 <- c(0.00, 0.00, 0.00, round(rnorm(P-3, 0, 0.5), 2))
l5 <- c(0.00, 0.00, 0.00, 0.00, round(rnorm(P-4, 0, 0.5), 2))
L <- cbind(l1,l2,l3, l4, l5) # the loading matrix
# Needs to have upper triangle of 0

Theta <- mvrnorm(N, rep(0,D), diag(rep(1,D))) # sample factor scores
Epsilon <-mvrnorm(N, rep(0,P), Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data
L

BFA_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_Woodbury_data <- list(P=P, N=N, Y=Y, D=D)

benchmark_BFA_P <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)
