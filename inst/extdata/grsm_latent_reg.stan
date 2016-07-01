functions {
  real grsm(int r, real theta, real mu, real alpha, real beta, vector kappa) {
    vector[rows(kappa) + 1] unsummed;
    vector[rows(kappa) + 1] probs;
    unsummed = append_row(rep_vector(0, 1), alpha*(theta + mu - beta - kappa));
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(r | probs);
  }
}
data {
  int<lower=1> I;                // # items
  int<lower=1> J;                // # persons
  int<lower=1> N;                // # responses
  int<lower=1,upper=I> ii[N];    // i for n
  int<lower=1,upper=J> jj[N];    // j for n
  int<lower=0> y[N];             // response for n; y in {0 ... m_i}
  int<lower=1> K;                // # person covariates
  matrix[J,K] W;                 // person covariate matrix
}
transformed data {
  int r[N];                      // modified response; r in {1 ... m_i + 1}
  int m;                         // # steps
  m = max(y);
  for(n in 1:N)
    r[n] = y[n] + 1;
}
parameters {
  vector<lower=0>[I] alpha;
  vector[I-1] beta_free;
  vector[m-1] kappa_free;
  vector[J] theta;
  vector[K] lambda;
}
transformed parameters {
  vector[I] beta;
  vector[m] kappa;
  beta = append_row(beta_free, rep_vector(-1*sum(beta_free), 1));
  kappa = append_row(kappa_free, rep_vector(-1*sum(kappa_free), 1));
}
model {
  vector[J] mu;
  mu = W*lambda;
  target += lognormal_lpdf(alpha | 1, 1);
  target += normal_lpdf(beta_free | 0, 5);
  target += normal_lpdf(kappa_free | 0, 5);
  target += normal_lpdf(theta | 0, 1);
  for (n in 1:N)
    target += grsm(r[n], theta[jj[n]], mu[jj[n]], alpha[ii[n]],
                   beta[ii[n]], kappa);
}
