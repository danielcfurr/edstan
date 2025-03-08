data {
  int<lower=1> I; // # questions
  int<lower=1> J; // # persons
  int<lower=1> N; // # observations
  array[N] int<lower=1, upper=I> ii; // question for n
  array[N] int<lower=1, upper=J> jj; // person for n
  array[N] int<lower=0, upper=1> y; // correctness for n
  int<lower=1> K; // # person covariates
  matrix[J, K] W; // person covariate matrix
}
parameters {
  sum_to_zero_vector[I] beta;
  vector[J] theta;
  real<lower=0> sigma;
  vector[K] lambda;
}
model {
  beta ~ normal(0, 3);
  theta ~ normal(W * lambda, sigma);
  lambda ~ student_t(7, 0, 2.5);
  sigma ~ exponential(.1);
  y ~ bernoulli_logit(theta[jj] - beta[ii]);
}
