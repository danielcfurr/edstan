################################################################################
# 2PL reference class

#' The two-parameteric logistic Reference Class.
#'
#' @field person_names A character vector for person names.
#' @field item_names A character vector for item names.
#' @seealso 
#' See \code{\link{common_stanfit}} for additional methods. See 
#' \code{\link{twopl_long_stan}} and \code{\link{twopl_wide_stan}} for 
#' estimating the 2PL. See \code{\link{plot_icc}} for addtional options for
#' \code{icc}.
#' @export
twopl_stanfit <- setRefClass("twopl_stanfit",
                             contains = "common_stanfit",
                             fields = c("person_names", "item_names"))

twopl_stanfit$methods(
  print = function(decimals = 2) {
    "Display customized output."
    print_header_stan(fit)
    print_stan(fit,
               pars = "alpha", 
               title = "Discrimination parameters:",
               names = list(alpha = item_names),
               decimals = decimals)
   print_stan(fit, 
              pars = "beta", 
              title = "Difficulty parameters:",
              names = list(beta = item_names),
              decimals = decimals )
   print_vector_stan(fit, 
                     pars="theta", 
                     title="Ability parameter vector:")
  })

twopl_stanfit$methods(
  icc = function(item, ...) {
    "Plot an item characteristic curve."
    if(is.numeric(item)) {
      item_id <- item
    } else {
      s <- paste("^", item, "$", sep = "")
      item_id <- grep(s, item_names)
    }
    thetas <- get_parameters(fit, pars = "theta")$mean
    thetas_to_plot <- seq(from = min(thetas), to = max(thetas), 
                          length.out = 100)
    plot_icc(fit = fit, item = item_id, gamma = 0, theta = thetas_to_plot, ...)
  })


################################################################################
# 2PL wrappers

#' Estimate the 2PL model using long-form data.
#' 
#' @param id A vector identifying persons.
#' @param item A vector identifying persons.
#' @param response A vector coded as 1 for a correct response and 0 otherwise.
#' @param ... Additional options passed to \code{\link[rstan]{stan}}.
#' @return A \code{\link{twopl_stanfit}} object.
#' @seealso See \code{\link{twopl_wide_stan}} for wide-form data. See \code{\link{twopl_stanfit}} and \code{\link{common_stanfit}} for applicable methods.
#' @examples
#' # Make the spelling data long-form
#' require(reshape2)
#' wide <- spelling[, 2:5]
#' wide$id <- 1:nrow(wide)
#' long <- melt(wide, id.vars = "id")
#' 
#' # Estimate the model
#' myfit <- twopl_long_stan(long$id, long$variable, long$value, chains = 4, iter = 200)
#' myfit$print()
#' @export
twopl_long_stan <- function(id,
                            item,
                            response,
                            path = "",
                            ... ) {
  
  if(check_lengths_stan(list(id, item, response)) ) {
    stop("Vectors (id, item, and response) have differing lengths.")
  }
  
  if(have_na_stan(list(id, item, response)) ) {
    stop("Vectors (id, item, and response) have NAs.")
  }
  
  match_id <- match_id_stan(id)
  match_item <- match_id_stan(item)
  
  stan_data <- list(
    I  = max(match_item$new),
    J  = max(match_id$new),
    N  = length(response),
    ii = match_item$new,
    jj = match_id$new,
    y  = response )

  code_file <- system.file("extdata", "twopl.stan", package = "edstan")
  
  stan_fit <- rstan::stan(file = code_file,
                   data = stan_data,
                   ... )
  
  RC <- twopl_stanfit$new(fit = stan_fit,
                          data = stan_data,
                          person_names = unique(match_id$old),
                          item_names = unique(match_item$old) )
  
  return(RC)
  
}


#' Estimate the 2PL model using a response matrix.
#' 
#' @param response_matrix A response matrix. Columns represent items, and rows represent persons. Each element is one or zero or may be NA if missing.
#' @param ... Additional options passed to \code{\link[rstan]{stan}}.
#' @return A \code{\link{twopl_stanfit}} object.
#' @seealso See \code{\link{twopl_long_stan}} for long-form data. See \code{\link{twopl_stanfit}} and \code{\link{common_stanfit}} for applicable methods.
#' @examples
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' myfit$print()
#' @export
twopl_wide_stan <- function(response_matrix,
                            ... ) {
  
  response <- as.vector(t(response_matrix))

  if(is.null(rownames(response_matrix))) {
    id <- rep(1:nrow(response_matrix), each = ncol(response_matrix))
  } else {
    id <- rep(rownames(response_matrix), each = ncol(response_matrix))
  }
  
  if(is.null(colnames(response_matrix))) {
    item <- rep(1:ncol(response_matrix), times = nrow(response_matrix))
  } else {
    item <- rep(colnames(response_matrix), times = nrow(response_matrix))
  }  
  
  not_missing <- !is.na(response)
  
  RC <- twopl_long_stan(id = id[not_missing],
                        item = item[not_missing],
                        response = response[not_missing],
                        ... ) 
  
  return(RC)
  
}
