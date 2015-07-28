# Multidimensional Rasch model reference class ---------------------------------

#' The multidimensional Rasch model Reference Class.
#'
#' @field person_names A character vector for person names.
#' @field item_names A character vector for item names.
#' @seealso See \code{\link{common_stanfit}} for additional methods. See
#'   \code{\link{mrasch_stan}} and for estimating the Rasch model. See
#'   \code{\link{plot_icc}} for addtional options for \code{icc}.
#' @export
mrasch_stanfit <- setRefClass("mrasch_stanfit",
                             contains = "common_stanfit",
                             fields = c("person_names", "item_names"))

mrasch_stanfit$methods(
  show = function(decimals = 2,
                  probs = c(.025, .25, .5, .75, .975)) {
    "Display customized output."
    print_header_stan(fit)
    print_stan(fit,
               pars = "beta",
               title = "Difficulty parameters:",
               names = list(beta = item_names),
               decimals = decimals,
               probs = probs)
    print_stan(fit,
               pars = c("sigma", "omega"),
               title = "Ability distribution parameters:",
               decimals = decimals,
               probs = probs)
    print_vector_stan(fit,
                      pars="theta",
                      title="Ability parameter vector:")
  })

mrasch_stanfit$methods(
  icc = function(item, ...) {
    "Plot an item characteristic curve."
    inputs <- list(...)
    inputs[["fit"]] <- fit
    inputs[["alpha"]] <- 1
    inputs[["gamma"]] <- 0
    inputs <- finish_icc_method_inputs_stan(item, inputs, item_names)
    do.call(plot_icc, inputs)
  })


# Multidimensional Rasch model wrappers ----------------------------------------

#' Estimate the multidimensional Rasch model using Stan
#'
#' @param response_matrix A response matrix. Columns represent items, and rows
#'   represent persons. Each element indicates a correct response by one or
#'   zero, and NA may be supplied for missing responses.
#' @param id A vector identifying persons. Required if \code{response_matrix} is
#'   not supplied.
#' @param item A vector identifying persons. Required if \code{response_matrix}
#'   is not supplied.
#' @param response A vector coded as 1 for a correct response and 0 otherwise.
#'   Required if \code{response_matrix} is not supplied.
#' @param z A design matrix for the random effects. It must have one row per
#'   item and one column per dimension.
#' @param ... Additional options passed to \code{\link[rstan]{sampling}}. The
#'   usual choices are \code{iter} for the number of iterations and
#'   \code{chains} for the number of chains.
#' @return A \code{\link{rasch_stanfit}} Reference Class object.
#' @seealso See \code{\link[rstan]{stan}} for additional options. See
#'   \code{\link{mrasch_stanfit}} and \code{\link{common_stanfit}} for
#'   applicable Reference Class methods.
#' @examples
#' # The spelling data response matrix
#' x <- spelling[, 2:5]
#'
#' # We pretend that the first two items in the spelling data load on a different
#' # dimension than the second two
#' z <- cbind(c(1, 1, 0, 0), c(0, 0, 1, 1))
#'
#' # We estimate the model
#' myfit <- mrasch_stan(response_matrix = x, z = z, iter = 200, chains = 4)
#' @export
mrasch_stan <- function(response_matrix = NULL,
                        id = NULL,
                        item = NULL,
                        response = NULL,
                        z,
                        ... ) {

  # If response_matrix is provided, use to get vectors for id, item, response.
  # Else, check that id, item, and response are all supplied.
  if(!is.null(response_matrix)) {
    vector_list <- response_matrix_to_long_stan(response_matrix)
    id = vector_list$id
    item = vector_list$item
    response = vector_list$response
  } else {
    if(any(is.null(id), is.null(item), is.null(response))) {
      stop("All three of id, item, and response must be supplied if response_matrix is not supplied")
    }
  }

  # Check input vectors for errors.
  check_vectors_stan(permitted = c("numeric", "character"), id, item, response)

  # Additional checks on response vector. Must be numeric and 0, 1.
  try(response <- as.numeric(response))
  if(is.null(response)) stop("response must be numeric")
  if(!all(response %in% 0:1)) stop("response must contain only 0 and 1")

  match_id <- match_id_stan(id)
  match_item <- match_id_stan(item)

  try(z <- as.matrix(z))
  if(is.null(z)) stop("z must be a matrix or vector")
  # If z is a vector of integers indicating dimension for each item, switch it
  # to a design matrix. Check the values for z first.
  if(ncol(z) == 1) {
    dims <- 1:max(z)
    if(any(dims %in% z == FALSE)) stop("Invalid dimensions indicated in vector z")
    if(any(z %in% dims == FALSE)) stop("Invalid dimensions indicated in vector z")
    new_z <- matrix(0, ncol = max(dims), nrow = nrow(z))
    for(r in 1:nrow(new_z)) new_z[r, z[r]] <- 1
    z <- new_z
  }
  if(nrow(z) != max(match_item$new)) stop("Matrix z must contain have exactly one row per item")
  if(any(z %in% 0:1 == FALSE)) stop("Matrix z must contain only 0 and 1")
  if(ncol(z) >= nrow(z)) stop("Too many dimensions specified in matrix z")
  if(any(apply(z, 2, sum) == 0)) stop("A dimension in matrix z has no items")
  if(any(apply(z, 1, sum) == 0)) stop("An item in matrix z has no associated dimensions")

  stan_data <- list(
    I  = max(match_item$new),
    J  = max(match_id$new),
    D  = ncol(z),
    N  = length(response),
    ii = match_item$new,
    jj = match_id$new,
    y  = response,
    z  = z)

  model_file <- file.path(system.file("extdata", package = "edstan"),
                          "mrasch.stan")

  stan_fit <- rstan::stan(file = model_file,
                          data = stan_data,
                          ... )

  RC <- mrasch_stanfit$new(fit = stan_fit,
                           data = stan_data,
                           person_names = unique(match_id$old),
                           item_names = unique(match_item$old) )

  return(RC)

}

