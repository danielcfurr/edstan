data {
  int<lower=1> I;               // # questions
  int<lower=1> J;               // # persons
  int<lower=1> N;               // # observations
  int<lower=1, upper=I> ii[N];  // question for n
  int<lower=1, upper=J> jj[N];  // person for n
  int<lower=0, upper=1> y[N];   // correctness for n
  int<lower=1> K;               // # person covariates
  matrix[J,K] W;                // person covariate matrix
}
transformed data {
  vector[K] center;
  vector[K] spread;
  matrix[J,K] W_adj;
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
      for(j in 1:J) W_adj[j,k] = (W[j,k] - center[k]) / spread[k];
    }
  }
}
parameters {
  vector<lower=0>[I] alpha;
  vector[I-1] beta_free;
  vector[J] theta;
  vector[K] lambda_adj;
}
transformed parameters {
  vector[I] beta;
  beta[1:(I-1)] = beta_free;
  beta[I] = -1*sum(beta_free);
}
model {
  alpha ~ lognormal(1, 1);
  beta_free ~ normal(0, 3);
  lambda_adj ~ student_t(1, 0, 1);
  theta ~ normal(W_adj*lambda_adj, 1);
  y ~ bernoulli_logit(alpha[ii].*(theta[jj] - beta[ii]));
}
generated quantities {
  vector[K] lambda;
  lambda[2:K] = lambda_adj[2:K] ./ spread[2:K];
  lambda[1] = W_adj[1, 1:K]*lambda_adj[1:K] - W[1, 2:K]*lambda[2:K];
}
