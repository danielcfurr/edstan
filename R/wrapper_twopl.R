
#---------------------------------------------------------------------------
# 2PL reference class

twopl_stanfit <- setRefClass("twopl_stanfit",
                             fields = c("fit", "person_names", "item_names"))

twopl_stanfit$methods(
  results = function( ... ) {
    print_stan(fit,
               pars = "alpha", 
               title = "Discrimination parameters:",
               names = list(alpha = item_names) )
   print_stan(fit, 
              pars = "beta", 
              title = "Difficulty parameters:",
              names = list(beta = item_names) )
   print_vector_stan(fit, pars="theta", title="Ability parameter vector:")
  })


#---------------------------------------------------------------------------
# 2PL wrappers

#' Estimate the 2PL model using long-form data.
#' 
#' @param id A vector identifying persons.
#' @param item A vector identifying persons.
#' @param response A vector coded as 1 for a correct response and 0 otherwise.
#' @param ... Additional options passed to stan().
#' @return A twopl_stanfit reference class object.
#' @examples
#' require(reshape2)
#' wide <- cito[, 2:19]
#' wide$id <- 1:nrow(wide)
#' long <- melt(wide, id.vars = "id", variable_name = "item")
#' twopl_long_stan(long$id, long$item, long$value, chains = 4, iter = 200)
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

  code_file <- system.file("extdata", "stancode_twopl.stan", package = "edstan")
  
  stan_fit <- rstan::stan(file = code_file,
                   data = stan_data,
                   ... )
  
  RC <- twopl_stanfit$new(fit = stan_fit,
                          person_names = unique(match_id$old),
                          item_names = unique(match_item$old) )
  
  return(RC)
  
}


#' Estimate the 2PL model using a response matrix.
#' 
#' @param response_matrix A response matrix. Columns represent items, and rows represent persons. Each element is one or zero or may be NA if missing.
#' @param ... Additional options passed to stan().
#' @return A twopl_stanfit reference class object.
#' @examples
#' myfit <- twopl_wide_stan(cito[, 2:19], chains = 4, iter = 200)
#' myfit$results()
twopl_wide_stan <- function(response_matrix,
                            ... ) {
  
  response <- as.vector(t(response_matrix))

  if(is.null(rownames(response_matrix))) {
    id <- rep(1:nrow(response_matrix), each = ncol(response_matrix))
  } else {
    id <- rep(rownames(response_matrix), each = ncol(response_matrix))
  }
  
  if(is.null(colnames(response_matrix))) {
    item <- rep(1:ncol(response_matrix), each = nrow(response_matrix))
  } else {
    item <- rep(colnames(response_matrix), each = nrow(response_matrix))
  }  
  
  not_missing <- !is.na(response)
  
  RC <- twopl_long_stan(id = id[not_missing],
                        item = item[not_missing],
                        response = response[not_missing],
                        path = path,
                        ... ) 
  
  return(RC)
  
}
