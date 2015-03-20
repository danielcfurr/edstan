data {
  int<lower=1> I;               // # questions
  int<lower=1> J;               // # persons
  int<lower=1> N;               // # observations
  int<lower=1, upper=I> ii[N];  // question for n
  int<lower=1, upper=J> jj[N];  // person for n
  int<lower=0, upper=1> y[N];   // correctness for n
}
parameters {
  vector[I] beta;               // difficulty for item i
  vector[J] theta;              // ability for person j
  real<lower=0> sigma;          // ability standard deviation
}
model {
  vector[N] eta;
  beta ~ normal(0,10);
  theta ~ normal(0,1);
  sigma ~ lognormal(1,1);
  for (n in 1:N)
    eta[n] <- sigma * theta[jj[n]] - beta[ii[n]];
  y ~ bernoulli_logit(eta);
}