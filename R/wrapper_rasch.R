# Rasch model reference class --------------------------------------------------

#' The Rasch model Reference Class
#'
#' @field person_names A character vector for person names.
#' @field item_names A character vector for item names.
#' @seealso
#' See \code{\link{common_stanfit}} for additional methods. See
#' \code{\link{rasch_long_stan}} and \code{\link{rasch_wide_stan}} for
#' estimating the Rasch model. See \code{\link{plot_icc}} for addtional options
#' for \code{icc}.
#' @export
rasch_stanfit <- setRefClass("rasch_stanfit",
                             contains = "common_stanfit",
                             fields = c("person_names", "item_names"))

rasch_stanfit$methods(
  show = function(decimals = 2) {
    "Display customized output."
    print_header_stan(fit)
    print_stan(fit,
               pars = "beta",
               title = "Difficulty parameters:",
               names = list(beta = item_names),
               decimals = decimals )
    print_stan(fit,
               pars = "sigma",
               title = "Person standard deviation parameters:",
               decimals = decimals )
    print_vector_stan(fit,
                      pars="theta",
                      title="Ability parameter vector:")
  })

rasch_stanfit$methods(
  icc = function(item, ...) {
    "Plot an item characteristic curve."
    inputs <- list(...)
    inputs[["fit"]] <- fit
    inputs[["alpha"]] = 1
    inputs[["gamma"]] = 0
    inputs <- finish_icc_method_inputs_stan(item, inputs, item_names)
    do.call(plot_icc, inputs)
  })


# Rasch model wrapper ----------------------------------------------------------

#' Estimate the Rasch model using Stan
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
#' @param ... Additional options passed to \code{\link[rstan]{sampling}}. The
#'   usual choices are \code{iter} for the number of iterations and
#'   \code{chains} for the number of chains.
#' @return A \code{\link{rasch_stanfit}} Reference Class object.
#' @seealso See \code{\link[rstan]{sampling}} for additional options. See
#'   \code{\link{rasch_stanfit}} and \code{\link{common_stanfit}} for applicable
#'   Reference Class methods.
#' @examples
#' # If the data are in a response matrix ("wide-form"):
#' myfit1 <- rasch_stan(response_matrix = spelling[, 2:5], chains = 4,
#'                      iter = 200)
#' myfit1$show()
#'
#' # Make the spelling data long-form for next example
#' require(reshape2)
#' wide <- spelling[, 2:5]
#' wide$id <- 1:nrow(wide)
#' long <- melt(wide, id.vars = "id", variable.name = "item",
#'              value.name = "response")
#'
#' # If the data are in "long-form":
#' myfit2 <- rasch_stan(id = long$id, item = long$item,
#'                      response = long$response, chains = 4, iter = 200)
#' myfit2$show()
#' @export
rasch_stan <- function(response_matrix = NULL,
                       id = NULL,
                       item = NULL,
                       response = NULL,
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
  if(!all(response %in% 0:1)) stop("response must contain only 0 and 1.")

  match_id <- match_id_stan(id)
  match_item <- match_id_stan(item)

  stan_data <- list(
    I  = max(match_item$new),
    J  = max(match_id$new),
    N  = length(response),
    ii = match_item$new,
    jj = match_id$new,
    y  = response )

  model_obj <- get_model_stan("rasch")

  stan_fit <- rstan::sampling(object = model_obj,
                              data = stan_data,
                              ... )

  RC <- rasch_stanfit$new(fit = stan_fit,
                          data = stan_data,
                          person_names = unique(match_id$old),
                          item_names = unique(match_item$old) )

  return(RC)


}
