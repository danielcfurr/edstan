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
#' @param covariates An optional data frame containing (only) person-covariates.
#'   It must contain one row per person or be of the same length as \code{y},
#'   \code{ii}, and \code{jj}. If it contains one row per person, it must be in
#'   the same order as the response matrix (or \code{unique(jj)}). If it has a
#'   number of columns equal to the length of \code{y},
#'   \code{ii}, and \code{jj}, it must be in the same order as \code{jj} (for
#'   example, it may be a subset of columns from the same data frame that contains
#'   \code{y}, \code{ii}, and \code{jj}).
#' @param formula An optional formula for the latent regression that is applied
#'   to \code{covariates}. The left side  should be blank (for example,
#'   \code{~ v1 + v2}). By default it includes only a model intercept,
#'   interpretable as the mean of the person distribution. If set to
#'   \code{NULL}, then \code{covariates} is used directly  as the design matrix
#'   for the latent regression.
#' @return A data list suitable for \code{\link{irt_stan}}.
#' @seealso See \code{\link{labelled_integer}} for a means of creating
#' appropriate inputs for \code{ii} and \code{jj}.
#' See \code{\link{irt_stan}} to fit a model to the data list.
#' @examples
#' # For a response matrix ("wide-form" data) with person covariates:
#' spelling_list <- irt_data(response_matrix = spelling[, 2:5],
#'                           covariates = spelling[, "male", drop = FALSE],
#'                           formula = ~ 1 + male)
#'
#' # Alternatively, the same may be created by:
#' W <- cbind(intercept = 1, spelling[, "male"])
#' spelling_list <- irt_data(response_matrix = spelling[, 2:5],
#'                           covariates = W,
#'                           formula = NULL)
#'
#' # For long-form data (one row per item-person pair):
#' agg_list_1 <- irt_data(y = aggression$poly,
#'                        ii = aggression$item,
#'                        jj = aggression$person)
#'
#' # Add a latent regression and use labelled_integer() with the items
#' agg_list_2 <- irt_data(y = aggression$poly,
#'                        ii = labelled_integer(aggression$description),
#'                        jj = aggression$person,
#'                        covariates = aggression[, c("male", "anger")],
#'                        formula = ~ 1 + male*anger)
#' @export
irt_data <- function(response_matrix = matrix(), y = integer(), ii = integer(),
                     jj = integer(), covariates = data.frame(), formula = ~1) {

  # If response matrix is provided...
  if(!identical(response_matrix, matrix())) {

    if(!all(y == integer(), ii == integer(), jj == integer())) {
      warning("Options 'y', 'ii', and 'jj' are ignored when response_matrix is provided.")
    }

    # Assemble y into a vector of non-missing values
    not_missing <- as.vector(t(!is.na(response_matrix)))
    y <- as.vector(t(response_matrix))[not_missing]

    # Assemble ii into a vector of non-missing values
    ii_unique <- 1:ncol(response_matrix)
    names(ii_unique) <- colnames(response_matrix)
    ii_expand <- rep(ii_unique, times = nrow(response_matrix))
    ii <- ii_expand[not_missing]

    # Assemble jj into a vector of non-missing values
    jj_unique <- 1:nrow(response_matrix)
    names(jj_unique) <- rownames(response_matrix)
    jj_expand <- rep(jj_unique, each = ncol(response_matrix))
    jj <- jj_expand[not_missing]


    # If response matrix is NOT provided...
  } else {

    # Check that all long-form options are provided
    if(any(identical(y, integer()), identical(ii, integer()),
           identical(jj, integer()))) {
      stop("Options 'y', 'ii', and 'jj' are all required when response_matrix is not provided.")
    }

    # Check that all vectors have same length
    vec_lengths <- lengths(list(y, ii, jj))
    if(min(vec_lengths) != max(vec_lengths)) {
      stop("Vectors 'y', 'ii', and 'jj' must have the same length.")
    }

    # Check that there are no NAs
    if(sum(is.na(c(y, ii, jj))) > 0) {
      stop("'y', 'ii', and 'jj' may not include NA.")
    }

    # Check that ii and jj are consecutive integers
    is_consec_integer <- function(x) {
      is_integer <- all(x == as.integer(x))
      is_start_at_one <- min(x) == 1
      is_consecutive <- all(1:max(x) %in% unique(x))
      return(all(is_integer, is_start_at_one, is_consecutive))
    }
    if(!is_consec_integer(ii)) {
      stop("'ii' must consist of consecutive integers with lowest value of one.")
    }
    if(!is_consec_integer(jj)) {
      stop("'jj' must consist of consecutive integers with lowest value of one.")
    }

  }

  # Check suitability of y
  min_y <- tapply(y, ii, min)
  max_y <- tapply(y, ii, max)
  lu_y <- tapply(y, ii, function(x) length(unique(x)))
  if(any(min_y != 0)) {
    warning("One or more items have a minimum score not equal to zero. This ",
            "is may be okay for rating scale models.")
  }
  if(any(max_y + 1 != lu_y)) {
    warning("One or more items have missing response categories. This is may ",
            "be okay for rating scale models.")
  }

  # If 'formula' set to NULL, use 'covariates' as is. If 'covariates' not
  # provided, set to be constant only. Otherwise, if nrow(covariates) equals
  # number of persons, apply formula to it. Or if nrow(covariates) equals
  # number of total responses, shorten it and then apply formula.
  if(is.null(formula)) {
    W <- covariates
  } else if(identical(covariates, data.frame())) {
    W <- matrix(1, ncol = 1, nrow = max(jj))
  } else {
    if(nrow(covariates) == max(jj)) {
      W <- stats::model.matrix(formula, covariates)
    } else if(nrow(covariates) == length(jj)) {
      W <- stats::model.matrix(formula, covariates[!duplicated(jj),])
    } else {
      stop("The 'covariates' must have a number of rows equal to the ",
           "number of persons or to the length of 'jj'. If 'covariates' has ",
           "multiple rows per person (as with long-form data), all of them ",
           "must be identical.")
    }
  }
  if(!all(W[,1] == 1)) {
    stop("The person covariate matrix must have a first column with all ",
         "elements set to one. If you supplied a formula, the intercept term ",
         "cannot be omitted. If you did not supply a formula, the first ",
         "column of 'covariates' must have all elements set to one.")
  }

  data_list <- list(N = length(y), I = max(ii), J = max(jj), ii = ii, jj = jj,
                    y = y, K = ncol(W), W = W)

  return(data_list)

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
  x_integer <- match(x, unique(x))
  names(x_integer) <- as.character(x)
  return(x_integer)
}
