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
  vector[K] center;              // values used to center covariates
  vector[K] spread;              // values used to scale covariates
  matrix[J,K] W_adj;             // centered and scaled covariates
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
  vector[I-1] beta_free;
  vector[J] theta;
  real<lower=0> sigma;
  vector[K] lambda_adj;
}
transformed parameters {
  vector[I] beta;
  beta[1:(I-1)] = beta_free;
  beta[I] = -1*sum(beta_free);
}
model {
  beta_free ~ normal(0, 3);
  theta ~ normal(W_adj*lambda_adj, sigma);
  lambda_adj ~ student_t(1, 0, 1);
  sigma ~ exponential(.1);
  y ~ bernoulli_logit(theta[jj] - beta[ii]);
}
generated quantities {
  vector[K] lambda;
  lambda[2:K] = lambda_adj[2:K] ./ spread[2:K];
  lambda[1] = W_adj[1, 1:K]*lambda_adj[1:K] - W[1, 2:K]*lambda[2:K];
}
