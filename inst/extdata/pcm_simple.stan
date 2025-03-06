functions {
  real pcm(int y, real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(y + 1 | probs);
  }
}
data {
  int<lower=1> I; // # items
  int<lower=1> J; // # persons
  int<lower=1> N; // # responses
  array[N] int<lower=1, upper=I> ii; // i for n
  array[N] int<lower=1, upper=J> jj; // j for n
  array[N] int<lower=0> y; // response for n; y = 0, 1 ... m_i
}
transformed data {
  int m; // # parameters per item (same for all items)
  m = max(y);
}
parameters {
  array[I] vector[m] beta;
  vector[J] theta;
  real<lower=0> sigma;
}
model {
  for (i in 1 : I) {
    beta[i] ~ normal(0, 3);
  }
  theta ~ normal(0, sigma);
  sigma ~ exponential(.1);
  for (n in 1 : N) {
    target += pcm(y[n], theta[jj[n]], beta[ii[n]]);
  }
}
