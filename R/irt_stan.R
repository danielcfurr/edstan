#' Fit an IRT model
#'
#' @export


irt_stan <- function(
  item,
  person,
  response,
  varying_slopes = TRUE,
  thresholds = TRUE,
  common_steps = FALSE,
  posterior_replicates = FALSE,
  covariates = NULL,
  prior_alpha = if (varying_slopes) c(mean=0, sd=1) else c(mean=0, sd=1),
  prior_beta_base = c(mean=0, sd=3),
  prior_beta_step = if (thresholds) c(mean=.5, sd=1) else c(mean=0, sd=3),
  prior_lambda = NULL,
  return_data_list = FALSE,
  ...
) {

  lookup_item <- make_lookup(item)
  ii <- lookup_item[as.character(item)]

  lookup_person <- make_lookup(person)
  jj <- lookup_person[as.character(person)]

  I <- max(ii)
  J <- max(jj)
  N <- length(response)
  max_by_item <- tapply(response, ii, max)

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

  if (is.null(covariates)) {
    W <- matrix(1, nrow = J, ncol = 1)
  } else {
    W <- covariates
  }

  if (is.null(prior_lambda) & !is.null(covariates)) {
    prior_sd <- apply(W, 2, function(x) {
      if (length(unique(x)) == 1) 5 else sd(x) * 2
    })
    prior_mean <- rep(0, times = length(prior_sd))
    prior_lambda <- cbind(prior_mean, prior_sd)
  } else if (is.null(prior_lambda)) {
    prior_lambda = matrix(c(0, 5), nrow = 1, ncol = 2)
    rownames(prior_lambda) <- "(Intercept)"
    colnames(prior_lambda) <- c("prior_mean", "prior_sd")
  }

  dl <- list(
    N = N,
    I = I,
    J = J,
    ii = ii,
    jj = jj,
    y = response,
    K = ncol(W),
    W = W,
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

  if (return_data_list) {

    return(dl)

  } else {

    fit <- rstan::sampling(
      stanmodels$edstan_model,
      data = dl,
      pars = c("alpha", "beta_base", "beta_step", "lambda", "theta"),
      ...
    )

    opt_names <- grep("^(pos|prior|flag)_", names(dl), value = TRUE)
    opts <- lapply(opt_names, function(i) dl[[i]])
    names(opts) <- opt_names
    opts$lookup_item <- lookup_item
    opts$lookup_person <- lookup_person
    opts$covariates <- colnames(W)

    fit <- as(fit, "edstanfit")
    fit@edstan_options <- opts

    return(fit)

  }

}
