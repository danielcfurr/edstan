#' Stan for item response theory
#'
#' \pkg{edstan} attempts to make easy the fitting of standard item response
#' theory models using \pkg{rstan}.
#'
#' A user will generally want to use the following functions (in order) to fit
#' a model:
#'
#' \enumerate{
#'   \item \code{\link{irt_data}} to format the data,
#'   \item \code{\link{irt_stan}} to fit a model, and
#'   \item \code{\link{print_irt_stan}} to view some results.
#' }
#'
#' Additionally, a fourth function, \code{\link{labelled_integer}}, is some
#' times helpful for data formatting. The package also includes six Stan models
#' (see \code{\link{irt_stan}} for a list) and two example datasets
#' (\code{\link{aggression}} and \code{\link{spelling}}).
#'
#' It is expected that once a user is comfortable fitting pre-defined
#' \pkg{edstan} models, they will write their own Stan models and fit them with
#' \code{\link[rstan]{stan}}, for which \code{\link{irt_stan}} is a wrapper.
"_PACKAGE"
#> [1] "_PACKAGE"


#' Create a Stan data list from an item response matrix or from long-form data.
#'
#' @param response_matrix An item response matrix.
#'   Columns represent items and rows represent persons.
#'   NA may be supplied for missing responses.
#'   The lowest score for each item should be 0, with exception to rating scale
#'   models.
#'   \code{y}, \code{ii}, and \code{jj} should not be supplied if a response
#'   matrix is given.
#' @param y A vector of scored responses for long-form data.
#'   The lowest score for each item should be 0, with exception to rating scale
#'   models.
#'   NAs are not permitted, but missing responses may simply be ommitted
#'   instead.
#'   Required if \code{response_matrix} is not supplied.
#' @param ii A vector indexing the items in \code{y}.
#'   This must consist of consecutive integers starting at 1.
#'   \code{\link{labelled_integer}} may be used to create a suitable vector.
#'   Required if \code{response_matrix} is not supplied.
#' @param jj A vector indexing the persons in \code{y}.
#'   This must consist of consecutive integers starting at 1.
#'   \code{\link{labelled_integer}} may be used to create a suitable vector.
#'   Required if \code{response_matrix} is not supplied.
#' @param W An optional matrix of person-covariates. If supplied, it must
#'   contain one row per person and should usually include an intercept.
#' @return A data list suitable for \code{\link{irt_stan}}.
#' @seealso See \code{\link{labelled_integer}} for a means of creating
#' appropriate inputs for \code{ii} and \code{jj}.
#' See \code{\link{irt_stan}} to fit a model to the data list.
#' @examples
#' # For a response matrix ("wide-form" data) with person covariates:
#' X <- spelling[, 2:5]
#' W <- cbind(intercept = 1, spelling[, "male"])
#' spelling_list <- irt_data(X, W = W)
#'
#' # If the data are in long-form:
#' agg_list <- irt_data(y = aggression$poly,
#'                      ii = aggression$item,
#'                      jj = aggression$person)
#'
#' # Add a latent regression and use labelled_integer() with the items
#' covars <- aggression[, c("person", "anger", "male")]
#' covars <- unique(covars)
#' covars <- covars[order(covars$person), ]
#' W <- cbind(intercept = 1, covars[, -1])
#' agg_list <- irt_data(y = aggression$dich,
#'                      ii = labelled_integer(aggression$description),
#'                      jj = aggression$person,
#'                      W = W)
#' @export
irt_data <- function(response_matrix = matrix(), y = integer(), ii = integer(),
                     jj = integer(), W = matrix()) {

  if(!identical(response_matrix, matrix())) {

    if(!all(y == integer(), ii == integer(), jj == integer())) {
      warning("Options y, ii, and jj are ignored when response_matrix is provided.")
    }

    not_missing <- as.vector(t(!is.na(response_matrix)))
    y <- as.vector(t(response_matrix))[not_missing]

    ii_unique <- 1:ncol(response_matrix)
    names(ii_unique) <- colnames(response_matrix)
    ii_expand <- rep(ii_unique, times = nrow(response_matrix))
    ii <- ii_expand[not_missing]

    jj_unique <- 1:nrow(response_matrix)
    names(jj_unique) <- rownames(response_matrix)
    jj_expand <- rep(jj_unique, times = ncol(response_matrix))
    jj <- jj_expand[not_missing]

  } else {

    if(any(identical(y, integer()), identical(ii, integer()),
           identical(jj, integer()))) {
      stop("Options y, ii, and jj are all required when response_matrix is not provided.")
    }

    vec_lengths <- lengths(list(y, ii, jj))
    if(min(vec_lengths) != max(vec_lengths)) {
      stop("Vectors y, ii, and jj must have the same length.")
    }

    if(sum(is.na(c(y, ii, jj))) > 0) {
      stop("y, ii, and jj may not include NA.")
    }

    is_consec_integer <- function(x) {
      is_integer <- all(x == as.integer(x))
      is_start_at_one <- min(x) == 1
      is_consecutive <- all(1:max(x) %in% unique(x))
      return(all(is_integer, is_start_at_one, is_consecutive))
    }

    if(!is_consec_integer(ii)) {
      stop("ii must consist of consecutive integers with lowest value of one.")
    }

    if(!is_consec_integer(jj)) {
      stop("jj must consist of consecutive integers with lowest value of one.")
    }

  }

  # Check suitability of y
  min_y <- tapply(y, ii, min)
  max_y <- tapply(y, ii, max)
  lu_y <- tapply(y, ii, function(x) length(unique(x)))
  if(any(min_y != 0)) {
    warning("One or more items have a minimum score not equal to zero. This is may be okay for rating scale models.")
  }
  if(any(max_y + 1 != lu_y)) {
    warning("One or more items have missing response categories. This is may be okay for rating scale models.")
  }

  # If K not provided, set it up to be the model intercept. If provided, check
  # that it has the correct number of rows and warn if no intercept in matrix.
  if(identical(W, matrix())) {
    W <- matrix(1, ncol = 1, nrow = max(jj))
  } else {
    if(nrow(as.matrix(W)) != max(jj)) {
      stop("W must have exactly one row per person.")
    }
    if(sum(apply(as.matrix(W), 2, function(x) all(x == 1))) != 1) {
      warning("In general one column of W should equal 1 to serve as a model intercept. Be sure you broke this rule on purpose.")
    }
  }

  data_list <- list(N = length(y), I = max(ii), J = max(jj), ii = ii, jj = jj,
                    y = y, K = ncol(W), W = W)

  return(data_list)

}


#' Estimate an item response model with Stan
#'
#' @param data_list A Stan data list created with \code{\link{irt_data}}.
#' @param model The file name for one of the provided .stan files, or
#'   alternatively, a user-created .stan file that accepts \code{data_list} as
#'   input data.
#'   The ".stan" file extension may be omitted.
#'   Defaults to either rasch_latent_reg.stan or pcm_latent_reg.stan.
#' @param ... Additional options passed to \code{\link[rstan]{stan}}. The
#'   usual choices are \code{iter} for the number of iterations and
#'   \code{chains} for the number of chains.
#'
#' @details
#' The following table lists the models inlcuded in \pkg{edstan} along with the
#' associated \emph{.stan} files. The file names are given as the \code{model}
#' argument.
#'
#' \tabular{ll}{
#'    \strong{Model}             \tab      \strong{File}           \cr
#'    Rasch                      \tab \emph{rasch_latent_reg.stan} \cr
#'    Partial credit             \tab \emph{pcm_latent_reg.stan}   \cr
#'    Rating Scale               \tab \emph{rsm_latent_reg.stan}   \cr
#'    Two-parameter logistic     \tab \emph{2pl_latent_reg.stan}   \cr
#'    Generalized partial credit \tab \emph{gpcm_latent_reg.stan}  \cr
#'    Generalized rating Scale   \tab \emph{grsm_latent_reg.stan}
#' }
#'
#' @return A \code{\link[rstan]{stanfit-class}} object.
#' @seealso See \code{\link[rstan]{stan}}, for which this function is a wrapper,
#'   for additional options.
#'   See \code{\link{irt_data}} and \code{\link{labelled_integer}} for functions
#'   that facilitate creating a suitable \code{data_list}.
#'   See \code{\link{print_irt_stan}} and \code{\link[rstan]{print}} for ways of
#'   getting tables summarizing parameter posteriors.
#' @examples
#' # List the Stan models included in edstan
#' folder <- system.file("extdata", package = "edstan")
#' dir(folder, "\\.stan$")
#'
#' # List the contents of one of the .stan files
#' rasch_file <- system.file("extdata/rasch_latent_reg.stan",
#'                           package = "edstan")
#' cat(readLines(rasch_file), sep = "\n")
#'
#' # Fit the Rasch and 2PL models on wide-form data with a latent regression
#' X <- spelling[, 2:5]
#' W <- cbind(intercept = 1, spelling[, "male"])
#' spelling_list <- irt_data(X, W = W)
#' rasch_fit <- irt_stan(spelling_list, iter = 200, chains = 4)
#' twopl_fit <- irt_stan(spelling_list, model = "2pl_latent_reg.stan",
#'                       iter = 200, chains = 4)
#'
#' # Print a summary of the parameter posteriors
#' print_irt_stan(rasch_fit, spelling_list)
#' print_irt_stan(twopl_fit, spelling_list)
#'
#' # Fit the rating scale and generalized partial credit models to long-form
#' # data without a latent regression
#' agg_list <- irt_data(y = aggression$poly,
#'                      ii = labelled_integer(aggression$description),
#'                      jj = aggression$person)
#' fit_rsm <- irt_stan(agg_list, model = "rsm_latent_reg.stan",
#'                     iter = 300, chains = 4)
#' fit_gpcm <- irt_stan(agg_list, model = "gpcm_latent_reg.stan",
#'                      iter = 300, chains = 4)
#'
#' # Print a summary of the parameter posteriors
#' print_irt_stan(fit_rsm, agg_list)
#' print_irt_stan(fit_gpcm, agg_list)
#' @export
irt_stan <- function(data_list, model = "", ... ) {

  max_y <- tapply(data_list$y, data_list$ii, max)
  is_polytomous <- any(max_y > 1)

  # Choose Stan model file if one not provided. If provided, add ".stan" to Stan
  # file if needed. Look for file first in working directory/given file path. If
  #not found, look up in package install folder.
  if(model == "") {
    stub <- ifelse(is_polytomous, "pcm_latent_reg.stan", "rasch_latent_reg.stan")
    stan_file <- file.path(system.file("extdata", package = "edstan"), stub)
  } else {
    stan_file <- ifelse(grepl("\\.stan$", model), model, paste0(model, ".stan"))
    if(!file.exists(stan_file)) {
      alt_file <- file.path(system.file("extdata", package = "edstan"), stan_file)
      if(file.exists(alt_file)) {
        stan_file <- alt_file
      } else {
        stop("Stan model file not found.")
      }
    }
  }

  message("Using ", file.path(stan_file), ".")
  fit <- stan(stan_file, data = data_list, ...)
  return(fit)

}


#' Transform a vector into consecutive integers
#'
#' @param x A vector, which may be numeric, string, or factor.
#' @return A vector of integers corresponding to entries in \code{x}.
#'   The lowest value will be 1, and the greatest value will equal the number of
#'   unique elements in \code{x}.
#'   The elements of the recoded vector are named according to the original
#'   values of \code{x}.
#'   The result is suitable for the \code{ii} and \code{jj} options for
#'   \code{\link{irt_data}}.
#' @examples
#' x <- c("owl", "cat", "pony", "cat")
#' labelled_integer(x)
#'
#' y <- as.factor(x)
#' labelled_integer(y)
#'
#' z <- rep(c(22, 57, 13), times = 2)
#' labelled_integer(z)
#' @export
labelled_integer <- function(x = vector()) {
  label_vector <- as.character(x)
  unique_labels <- unique(label_vector)
  unique_ids <- 1:length(unique_labels)
  names(unique_ids) <- unique_labels
  new_id_vector <- unique_ids[label_vector]
  return(new_id_vector)
}


#' View a table of parameter posteriors after using \code{irt_stan}
#'
#' @param fit A \code{stanfit-class} object created by \code{\link{irt_stan}}.
#' @param data_list A Stan data list created with \code{\link{irt_data}}.
#' @param probs A vector of quantiles for summarizing parameter posteriors.
#' @param print_opts Options passed to \code{\link[base]{print}} as a list.
#' @examples
#' # Make a suitable data list:
#' X <- spelling[, 2:5]
#' W <- cbind(1, spelling[, 1])
#' spelling_list <- irt_data(X, W = W)
#'
#' # Fit a latent regression  2PL
#' twopl_fit <- irt_stan(spelling_list, model = "2pl_latent_reg.stan",
#'                       iter = 200, chains = 4)
#'
#' # Get a table of parameter posteriors
#' print_irt_stan(twopl_fit, spelling_list)
#' @export
print_irt_stan <- function(fit, data_list, probs = c(.025, .25, .5, .75, .975),
                           print_opts = list(digits = 3)) {

  possible_pars <- c("alpha", "beta", "kappa", "lambda", "sigma")
  available = possible_pars %in% fit@model_pars
  names(available) <- possible_pars

  # Get number of beta parameters per item
  y <- data_list$y
  ii <- data_list$ii
  if(available["kappa"]) {
    m <- rep(1, times = max(ii))  # For rating scale models
  } else {
    m <- tapply(y, ii, max)       # For binary/partial credit models
  }

  # Prep item subtables
  out_list <- list(paste0("beta[", 1:m[1], "]"))
  for(i in 2:max(ii)) {
    out_list[[i]] <- paste0("beta[", (sum(m[1:(i-1)])+1):sum(m[1:i]), "]")
  }
  if(available["alpha"]) {
    for(i in 1:max(ii)) {
      out_list[[i]] <- c(paste0("alpha[", i, "]"), out_list[[i]])
    }
  }

  # Prep item subtable labels
  if(is.null(names(data_list$ii))) {
    out_labels <- paste("Item", unique(data_list$ii))
  } else {
    out_labels <- paste0("Item ", unique(data_list$ii), ": ",
                         unique(names(data_list$ii)))
  }

  # Prep kappa subtable and label
  if(available["kappa"]) {
    out_list[[length(out_list) + 1]] <- "kappa"
    out_labels[length(out_labels) + 1] <- "Rating scale step parameters"
  }

  # Prep ability subtable and label
  if(available["sigma"]) {
    out_list[[length(out_list) + 1]] <- c("lambda", "sigma")
  } else {
    out_list[[length(out_list) + 1]] <- "lambda"
  }
  out_labels[length(out_labels) + 1] <- "Ability distribution"

  # Print all the subtables
  for(i in 1:length(out_list)) {
    cat(out_labels[i], "\n")
    summary_to_print <- rstan::summary(fit, pars = out_list[[i]],
                                       probs = probs)[[1]]
    do.call(print, c(list(summary_to_print), print_opts))
    cat("\n")
  }

}
