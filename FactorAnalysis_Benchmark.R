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
N <- 100


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

benchmark_BFA_P10 <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 1
)

benchmark_BFA_P10
#316.09236 316.09236 316.09236 316.09236 316.09236 316.09236     1
#81.44477  81.44477  81.44477  81.44477  81.44477  81.44477     1
#25.89254  25.89254  25.89254  25.89254  25.89254  25.89254     1
set.seed(42)
D <- 5
P <- 20 
N <- 100


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

benchmark_BFA_P20 <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)
benchmark_BFA_P20

#min        lq      mean    median        uq       max neval
#713.92168 713.92168 713.92168 713.92168 713.92168 713.92168     1
#509.04523 509.04523 509.04523 509.04523 509.04523 509.04523     1
#60.63898  60.63898  60.63898  60.63898  60.63898  60.63898     1
set.seed(42)
D <- 5
P <- 40 
N <- 100


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

benchmark_BFA_P40 <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)
benchmark_BFA_P40

#Now for changing N
set.seed(42)
D <-3
P <- 10 
N <- 100


Psi <- diag(c(0.2079, 0.19, 0.1525, 0.20, 0.36, 0.1875, 0.1875, 1.00, 0.27, 0.27))
l1 <- c(0.99, 0.00, 0.25, 0.00, 0.80, 0.00, 0.50, 0.00, 0.00, 0.00)
l2 <- c(0.00, 0.90, 0.25, 0.40, 0.00, 0.50, 0.00, 0.00, -0.30, -0.30)
l3<-  c(0.00, 0.00, 0.85, 0.80, 0.00, 0.75, 0.75, 0.00, 0.80, 0.80)
L <- cbind(l1,l2,l3) # the loading matrix
# Needs to have upper triangle of 0

Theta <- mvrnorm(N, rep(0,D), diag(rep(1,D))) # sample factor scores
Epsilon <-mvrnorm(N, rep(0,P), Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data

N_warmup <- 1000
N_MCMC <- 10000

BFA_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_Woodbury_data <- list(P=P, N=N, Y=Y, D=D)


#N=100

benchmark_BFA_N100 <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)

benchmark_BFA_N100
#min      lq       mean     median   uq       max         neval cld
#80.76414 80.83233 80.96240 80.94381 81.05215 81.29094    10   c
#26.16015 26.43491 26.76596 26.76674 27.06925 27.27639    10  b 
#11.73076 11.97497 12.16475 12.13490 12.29475 12.64377    10 a


#N=300
N<-300
Theta <- mvrnorm(N, rep(0,D), diag(rep(1,D))) # sample factor scores
Epsilon <-mvrnorm(N, rep(0,P), Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data
BFA_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_Woodbury_data <- list(P=P, N=N, Y=Y, D=D)

benchmark_BFA_N300 <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)

benchmark_BFA_N300

#254.459580
#21.518893
#8.236699

#N=1000
N<-1000
Theta <- mvrnorm(N, rep(0,D), diag(rep(1,D))) # sample factor scores
Epsilon <-mvrnorm(N, rep(0,P), Psi) # sample error vector
Y<-Theta%*%t(L)+Epsilon# generate observable data
BFA_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_data <- list(P=P, N=N, Y=Y, D=D)


BFA_SuffStat_Woodbury_data <- list(P=P, N=N, Y=Y, D=D)

benchmark_BFA_N1000 <- microbenchmark(
  BFA_sampling <- rstan::sampling(BayesianFactorAnalysis_stan, BFA_data, iter=N_warmup+N_MCMC, warmup=N_warmup, chains = 1,
                                  cores = 1, seed = 123 ),
  BFA_SuffStat_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_stan, BFA_SuffStat_data, iter=N_warmup+N_MCMC,
                                            warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  BFA_SuffStat_Woodbury_sampling   = rstan::sampling(BayesianFactorAnalysis_SuffStat_Woodbury_stan, BFA_SuffStat_Woodbury_data,
                                                     iter=N_warmup+N_MCMC, warmup=N_warmup,  chains = 1, cores = 1, seed = 123),
  times = 10
)
benchmark_BFA_N1000
#781.236929
#26.184379
#7.970062