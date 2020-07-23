functions {

  real pcm(int y, real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(y + 1 | probs);
  }

  int pcm_rng(real theta, vector beta) {
    vector[rows(beta) + 1] unsummed;
    vector[rows(beta) + 1] probs;
    unsummed = append_row(rep_vector(0.0, 1), theta - beta);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_rng(probs) - 1;
  }

}
data {

  int<lower=2> I;                // # items
  int<lower=1> J;                // # persons
  int<lower=1> N;                // # responses
  int<lower=1,upper=I> ii[N];    // i for n
  int<lower=1,upper=J> jj[N];    // j for n
  int<lower=0> y[N];             // response for n; y = 0, 1 ... m_i
  int<lower=1> K;                // # person covariates
  matrix[J,K] W;                 // person covariate matrix

  int<lower=0> pos_s[I];         // index for first step par (0: no step par)
  int<lower=0> pos_e[I];         // index for last step par

  vector[2] prior_alpha;
  vector[2] prior_beta_base;
  vector[2] prior_beta_step;
  vector[2] prior_lambda[K];

  int<lower=0,upper=1> flag_varying_slope;
  int<lower=0,upper=1> flag_thresholds;
  int<lower=0,upper=1> flag_replicates;

}
transformed data {

  int S = max(pos_e);
  int m[I];

  for (i in 1:I) {
    m[i] = pos_s[i] ? pos_e[i] - pos_s[i] + 2 : 1;
  }

}
parameters {

  vector<lower=0>[flag_varying_slope ? I : 1] alpha;
  vector[I-1] beta_base_raw;
  vector[S] beta_step_raw;

  vector[J] theta_resid;
  vector[K] lambda;

}
transformed parameters {

  vector[J] theta = W*lambda + theta_resid;
  vector[I] beta_base;
  vector[S] beta_step;

  beta_base[1:(I-1)] = beta_base_raw;
  beta_base[I] = -1*sum(beta_base_raw);

  if (flag_thresholds) {
    beta_step = exp(beta_step_raw);
  } else {
    beta_step = beta_step_raw;
  }

}
model {

  vector[I] a;

  if (flag_varying_slope) {
    a = alpha;
  } else {
    a = rep_vector(alpha[1], I);
  }

  theta_resid ~ normal(0, 1);

  alpha ~ lognormal(prior_alpha[1], prior_alpha[2]);
  beta_base_raw ~ normal(prior_beta_base[1], prior_beta_base[2]);
  beta_step_raw ~ normal(prior_beta_step[1], prior_beta_step[2]);

  for (k in 1:K) {
    lambda[k] ~ normal(prior_lambda[k,1], prior_lambda[k,2]);
  }

  if (S == 0) {
    // Dichotomous model

    y ~ bernoulli_logit(theta[jj] .* a[ii] - beta_base[ii]);

  } else if (flag_thresholds) {
    // Threshold model

    for (n in 1:N) {

      vector[m[ii[n]]] b;

      if (m[ii[n]] > 1) {
        b = cumulative_sum(
          append_row(beta_base[ii[n]], beta_step[pos_s[ii[n]]:pos_e[ii[n]]])
          );
      } else {
        b[1] = beta_base[ii[n]];
      }

      y[n] + 1 ~ ordered_logistic(a[ii[n]] * theta[jj[n]], b);

    }

  } else {
    // Partial credit model

    for (n in 1:N) {

      vector[m[ii[n]]] b;

      if (m[ii[n]] > 1) {
        b = append_row(beta_base[ii[n]], beta_step[pos_s[ii[n]]:pos_e[ii[n]]]);
      } else {
        b[1] = beta_base[ii[n]];
      }

      target += pcm(y[n], a[ii[n]] * theta[jj[n]], b);

    }

  }

}
generated quantities {

  int y_rep[flag_replicates ? N : 0];

  if (flag_replicates) {

    vector[I] a;

    if (flag_varying_slope) {
      a = alpha;
    } else {
      a = rep_vector(alpha[1], I);
    }

    if (S == 0) {
      // Dichotomous model

      for (n in 1:N) {
        y_rep[n] =
          bernoulli_logit_rng(theta[jj[n]] * a[ii[n]] - beta_base[ii[n]]);
      }

    } else if (flag_thresholds) {
      // Threshold model

      for (n in 1:N) {

        vector[m[ii[n]]] b;

        if (m[ii[n]] > 1) {
          b = cumulative_sum(
            append_row(beta_base[ii[n]], beta_step[pos_s[ii[n]]:pos_e[ii[n]]])
            );
        } else {
          b[1] = beta_base[ii[n]];
        }

        y_rep[n] = ordered_logistic_rng(a[ii[n]] * theta[jj[n]], b) - 1;

      }

    } else {
      // Partial credit model

      for (n in 1:N) {

        vector[m[ii[n]]] b;

        if (m[ii[n]] > 1) {
          b = append_row(beta_base[ii[n]], beta_step[pos_s[ii[n]]:pos_e[ii[n]]]);
        } else {
          b[1] = beta_base[ii[n]];
        }

        y_rep[n] = pcm_rng(a[ii[n]] * theta[jj[n]], b);

      }

    }

  }

}
