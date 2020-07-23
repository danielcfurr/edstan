#' Fit an IRT model
#'
#' @export


irt_stan <- function(
  ii,
  jj,
  y,
  varying_slopes = TRUE,
  thresholds = TRUE,
  common_steps = FALSE,
  posterior_replicates = FALSE,
  covariates = matrix(1, nrow = max(jj), ncol = 1),
  prior_alpha = if (varying_slopes) c(0, 1) else c(0, 1),
  prior_beta_base = c(0, 3),
  prior_beta_step = if (thresholds) c(.5, 1) else c(0, 3),
  prior_lambda = matrix(c(0, 3), nrow = ncol(covariates), ncol = 2, byrow = TRUE),
  ...
) {

  I <- max(ii)
  J <- max(jj)
  N <- length(y)
  max_by_item <- tapply(y, ii, max)

  # Number of needed step parameters (not counting base) per item
  m <- ifelse(max_by_item > 0, max_by_item - 1, 0)

  if (common_steps) {

    starts <- rep(1, times = I)
    ends <- rep(max(m), times = I)

  } else {

    starts <- integer(I)
    ends <- integer(I)
    n <- 0

    for (i in 1:I) {
      if (m[i] == 0) {
        starts[i] <- 0
        ends[i] <- 0
      } else if (max(starts[1:i]) == 0) {
        n <- 1
        starts[i] <- n
        ends[i] <- n + m[i] - 1
        n <- ends[i] + 1
      } else {
        starts[i] <- n
        ends[i] <- n + m[i] -1
        n <- ends[i] + 1
      }
    }

  }

  dl <- list(
    N = N,
    I = I,
    J = J,
    ii = ii,
    jj = jj,
    y = y,
    K = ncol(covariates),
    W = covariates,
    pos_s = starts,
    pos_e = ends,
    prior_alpha = prior_alpha,
    prior_beta_base = prior_beta_base,
    prior_beta_step = prior_beta_step,
    prior_lambda = prior_lambda,
    flag_varying_slope = as.integer(varying_slopes),
    flag_thresholds = as.integer(thresholds),
    flag_replicates = as.integer(posterior_replicates)
  )

  fit <- rstan::sampling(stanmodels$edstan_model, data = dl, ...)

  return(list(data = dl, fit = fit))

}
