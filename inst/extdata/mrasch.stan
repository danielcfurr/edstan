data {
  int<lower=1> I;               // # questions
  int<lower=1> J;               // # persons
  int<lower=1> D;               // # dimensions
  int<lower=1> N;               // # observations
  int<lower=1, upper=I> ii[N];  // question for n
  int<lower=1, upper=J> jj[N];  // person for n
  int<lower=0, upper=1> y[N];   // correctness for n
  matrix[I,D] z;                // Random effects design matrix
}
transformed data {
  vector[D] zeroes;
  for (i in 1:D)
    zeroes[i] <- 0;
}
parameters {
  vector[I] beta;               // difficulty for item i
  vector[D] theta[J];           // ability for person j on dimension D
  corr_matrix[D] omega;         // Correlation matrix for ability dimensions
  vector<lower=0>[D] sigma;     // Standard deviations for ability dimensions
}
model {
  vector[N] eta;
  matrix[D,D] tau;
  tau <- quad_form_diag(omega,sigma);
  theta ~ multi_normal(zeroes,tau);
  beta ~ normal(0,10);
  sigma ~ lognormal(1,1);
  for (n in 1:N)
    eta[n] <- z[ii[n]]*theta[jj[n]] - beta[ii[n]];
  y ~ bernoulli_logit(eta);
}