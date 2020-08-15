#' Fit an IRT model
#'
#' @export


irt_stan <- function(
  response_matrix = matrix(),
  item = integer(),
  person = integer(),
  response = integer(),
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

  if(identical(response_matrix, matrix())) {

    # Check that all long-form options are provided
    if(any(identical(response, integer()), identical(item, integer()),
           identical(person, integer()))) {
      stop("Options 'response', 'item', and 'person' are all required when response_matrix is not provided.")
    }

    # Check that all vectors have same length
    vec_lengths <- lengths(list(response, item, person))
    if(min(vec_lengths) != max(vec_lengths)) {
      stop("Vectors 'response', 'item', and 'person' must have the same length.")
    }

    # Check that there are no NAs
    if(sum(is.na(c(response, item, person))) > 0) {
      stop("Vectors 'response', 'item', and 'person' may not include NA.")
    }

  } else {

    if(!all(response == integer(), item == integer(), person == integer())) {
      warning("Options 'response', 'item', and 'person' are ignored when response_matrix is provided.")
    }

    # Assemble responses into a vector of non-missing values
    not_missing <- as.vector(t(!is.na(response_matrix)))
    response <- as.vector(t(response_matrix))[not_missing]

    # Assemble item ids into a vector of non-missing values
    if (is.null(colnames(response_matrix))) {
      item <- rep(1:ncol(response_matrix), times = nrow(response_matrix))
    } else {
      item <- rep(colnames(response_matrix), times = nrow(response_matrix))
    }
    item <- item[not_missing]

    # Assemble person ids into a vector of non-missing values
    if (is.null(rownames(response_matrix))) {
      person <- rep(1:nrow(response_matrix), each = ncol(response_matrix))
    } else {
      person <- rep(rownames(response_matrix), each = ncol(response_matrix))
    }
    person <- person[not_missing]

  }

  # Further checks on inputs

  min_scores <- tapply(response, item, min)
  max_scores <- tapply(response, item, max)
  n_cats <- tapply(response, item, function(x) length(unique(x)))

  if (any(min_scores > 0)) {
    warning(sum(min_scores > 0), " items have lowest response greater than zero. This may be okay if expected.")
  }

  if (any(n_cats == 1)) {
    warning(sum(n_cats > 0), " items have only one used response category. This may be okay if expected.")
  }

  missing_intermediate_cats <- max_scores - min_scores + 1 < n_cats
  if (any(missing_intermediate_cats)) {
    warning(sum(missing_intermediate_cats), " items have unused intermediate categories. This may be okay if expected.")
  }

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
    warning("Default priors are used for the latent regression coefficients. These may be terrible, especially for the intercept term.")
    prior_sd <- apply(W, 2, function(x) {
      # If intercept, prior sd = 5, else 2 times the sd
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
