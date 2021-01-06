#' Extract ability estimates
#'

extract_ability <- function(
  object,
  probs = c(.1, .9),
  rescale_by_alpha = FALSE
) {

  verified_rescale_by_alpha <- fit_va@edstan_options$flag_varying_slope == 0 &
    rescale_by_alpha

  theta <- rstan::extract(object, "theta", permuted = FALSE, inc_warmup = TRUE)

  if (verified_rescale_by_alpha) {
    alpha <- rstan::extract(object, "alpha[1]", permuted = FALSE, inc_warmup = TRUE)
    theta <- theta * array(alpha, dim = dim(theta))
  }

  warmup_iters <- sapply(object@stan_args, function(x) x["warmup"])
  warmup_iters <- unique(unlist(warmup_iters))

  df <- rstan::monitor(theta, warmup = warmup_iters, probs = probs, print = FALSE)

  last_column_to_keep <- grep("^Rhat$", names(df))
  df <- as.data.frame(df)[,1:last_column_to_keep]

  return(cbind(person = object@edstan_options$lookup_person, df))

}
