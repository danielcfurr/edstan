functions {
  real rsm(int r, real theta, real beta, vector kappa) {
    vector[rows(kappa) + 1] unsummed;
    vector[rows(kappa) + 1] probs;
    unsummed = append_row(rep_vector(0, 1), theta - beta - kappa);
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
}
transformed data {
  int r[N];                      // modified response; r in {1 ... m_i + 1}
  int m;                         // # steps
  m = max(y);
  for(n in 1:N)
    r[n] = y[n] + 1;
}
parameters {
  vector[I-1] beta_free;
  vector[m-1] kappa_free;
  vector[J] theta;
  real<lower=0> sigma;
  real lambda;
}
transformed parameters {
  vector[I] beta;
  vector[m] kappa;
  beta[1:(I-1)] = beta_free;
  beta[I] = -1*sum(beta_free);
  kappa[1:(m-1)] = kappa_free;
  kappa[m] = -1*sum(kappa_free);
}
model {
  beta_free ~ normal(0, 9);
  kappa_free ~ normal(0, 9);
  theta ~ normal(lambda, sigma);
  lambda ~ student_t(3, 0, 1);
  sigma ~ exponential(.1);
  for (n in 1:N)
    target += rsm(r[n], theta[jj[n]], beta[ii[n]], kappa);
}
