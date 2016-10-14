functions {
  real pcm(int r, real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
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
  int<lower=0> y[N];             // response for n; y = 0, 1 ... m_i
  int<lower=1> K;                // # person covariates
  matrix[J,K] W;                 // person covariate matrix
}
transformed data {
  int r[N];                      // modified response; r = 1, 2, ... m_i + 1
  int m[I];                      // # parameters per item
  int pos[I];                    // first position in beta vector for item
  vector[K] center;              // values used to center covariates
  vector[K] spread;              // values used to scale covariates
  matrix[J,K] W_adj;             // centered and scaled covariates

  m = rep_array(0, I);
  for(n in 1:N) {
    r[n] = y[n] + 1;
    if(y[n] > m[ii[n]]) m[ii[n]] = y[n];
  }
  pos[1] = 1;
  for(i in 2:(I))
    pos[i] = m[i-1] + pos[i-1];

  {
    real min_w;
    real max_w;
    int minmax_count;
    for(j in 1:J) W_adj[j,1] = 1;         // first column / intercept
    for(k in 2:K) {                       // remaining columns
      min_w = min(W[1:J, k]);
      max_w = max(W[1:J, k]);
      minmax_count = 0;
      for(j in 1:J) {
        minmax_count = minmax_count + W[j,k] == min_w || W[j,k] == max_w;
      }
      if(minmax_count == J) {             // if column takes only 2 values
        center[k] = mean(W[1:J, k]);
        spread[k] = (max_w - min_w);
      } else {                            // if column takes > 2 values
        center[k] = mean(W[1:J, k]);
        spread[k] = sd(W[1:J, k]) * 2;
      }
      for(j in 1:J)
        W_adj[j,k] = (W[j,k] - center[k]) / spread[k];
    }
  }

}
parameters {
  vector<lower=0>[I] alpha;
  vector[sum(m)-1] beta_free;
  vector[J] theta;
  vector[K] lambda_adj;
}
transformed parameters {
  vector[sum(m)] beta;
  beta[1:(sum(m)-1)] = beta_free;
  beta[sum(m)] = -1*sum(beta_free);
}
model {
  alpha ~ lognormal(1, 1);
  beta_free ~ normal(0, 9);
  theta ~ normal(W_adj*lambda_adj, 1);
  lambda_adj ~ student_t(3, 0, 1);
  for (n in 1:N)
    target += pcm(r[n], theta[jj[n]].*alpha[ii[n]],
                  segment(beta, pos[ii[n]], m[ii[n]]));
}
generated quantities {
  vector[K] lambda;
  lambda[2:K] = lambda_adj[2:K] ./ spread[2:K];
  lambda[1] = W_adj[1, 1:K]*lambda_adj[1:K] - W[1, 2:K]*lambda[2:K];
}
