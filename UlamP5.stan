data {
    int N;             // Number of observations
    vector[N] Y;       // Response variable
    vector[N] X1;      // Predictor 1
    vector[N] X2;      // Predictor 2
    vector[N] X3;      // Predictor 3
    vector[N] X4;      // Predictor 4
    vector[N] X5;      // Predictor 5
    vector[N] X6;      // Predictor 6
    vector[N] X7;      // Predictor 7
    vector[N] X8;      // Predictor 8
    vector[N] X9;      // Predictor 9
    vector[N] X10;     // Predictor 10
}
parameters {
    real Intercept;                      // Intercept
    real b1;                      // Coefficient for X1
    real b2;                      // Coefficient for X2
    real b3;                      // Coefficient for X3
    real b4;                      // Coefficient for X4
    real b5;                      // Coefficient for X5
    real b6;                      // Coefficient for X6
    real b7;                      // Coefficient for X7
    real b8;                      // Coefficient for X8
    real b9;                      // Coefficient for X9
    real b10;                     // Coefficient for X10
    real<lower=0> sigma;          // Residual standard deviation
}
model {
    vector[N] mu;                 // Linear predictor

    // Priors
    sigma ~ exponential(1);       // Prior for sigma
    b1 ~ normal(0, 1);
    b2 ~ normal(0, 1);
    b3 ~ normal(0, 1);
    b4 ~ normal(0, 1);
    b5 ~ normal(0, 1);
    b6 ~ normal(0, 1);
    b7 ~ normal(0, 1);
    b8 ~ normal(0, 1);
    b9 ~ normal(0, 1);
    b10 ~ normal(0, 1);
    Intercept ~ normal(0, 1);

    // Linear predictor
    for (i in 1:N) {
        mu[i] = Intercept +
                b1 * X1[i] + b2 * X2[i] + b3 * X3[i] + b4 * X4[i] +
                b5 * X5[i] + b6 * X6[i] + b7 * X7[i] + b8 * X8[i] +
                b9 * X9[i] + b10 * X10[i];
    }

    // Likelihood
    Y ~ normal(mu, sigma);
}
