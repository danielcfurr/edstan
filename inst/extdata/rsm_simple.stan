functions {
  real rsm(int y, real theta, real beta, vector kappa) {
    vector[rows(kappa) + 1] unsummed;
    vector[rows(kappa) + 1] probs;
    unsummed = append_row(rep_vector(0, 1), theta - beta - kappa);
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
  array[N] int<lower=0> y; // response for n; y in {0 ... m_i}
}
transformed data {
  int m; // # steps
  m = max(y);
}
parameters {
  vector[I] beta;
  vector[m - 1] kappa_free;
  vector[J] theta;
  real<lower=0> sigma;
}
transformed parameters {
  vector[m] kappa;
  kappa[1 : m - 1] = kappa_free;
  kappa[m] = -1 * sum(kappa_free);
}
model {
  beta ~ normal(0, 3);
  target += normal_lpdf(kappa | 0, 3);
  theta ~ normal(0, sigma);
  sigma ~ exponential(.1);
  for (n in 1 : N) {
    target += rsm(y[n], theta[jj[n]], beta[ii[n]], kappa);
  }
}
