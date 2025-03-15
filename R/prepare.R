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
#' @param integerize Whether to apply \code{\link{labelled_integer}} to
#'   \code{ii} and \code{jj}. Defaults to \code{TRUE} and should be used
#'   unless in the inputs are already consecutive integers.
#' @param validate_regression Whether to check the latent regression
#'   equation and covariates for compatibility with the prior distributions
#'   for the coefficients. Defaults to \code{TRUE} and throws a warning
#'   if problems are identified.
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
                     jj = integer(), covariates = data.frame(), formula = NULL,
                     integerize = TRUE, validate_regression = TRUE) {

  long_format <- identical(response_matrix, matrix())

  if(!long_format) {
    # If response matrix is provided...

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

  } else {
    # If response matrix is NOT provided...

    # Check that all long-form options are provided
    if(any(identical(y, integer()), identical(ii, integer()),
           identical(jj, integer()))) {
      stop("Options 'y', 'ii', and 'jj' are all required when response_matrix ",
           "is not provided.")
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

    # Apply labelled_integer() if specified
    if (integerize) {
      ii <- labelled_integer(ii)
      jj <- labelled_integer(jj)
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
    warning("One or more items have a minimum score not equal to zero.")
  }
  if(any(max_y + 1 != lu_y)) {
    warning("One or more items have unused response categories.")
  }

  # Construct a formula and/or covariate data frame if not provided
  if(!identical(covariates, data.frame()) & !is.null(formula)) {
    # If covariates and formula are both provided
    # Pass
  } else if (!identical(covariates, data.frame())) {
    # If only covariates
    formula <- ~ .
  } else if (!is.null(formula)) {
    # If only formula
    stop("Error: 'covariates' must be specified when formula is provided.")
  } else if (long_format) {
    # If neither provided and using long format data
    covariates <- data.frame(meh = rep(1, times = length(jj)))
    formula <- ~ 1
  } else {
    # If neither provided and using wide format data
    covariates <- data.frame(meh = rep(1, times = max(jj)))
    formula <- ~ 1
  }

  # If long form data, reduce covariates to one row per person
  if (long_format) {
    covariates <- .long_format_covariates(covariates, jj, formula)
  }

  # Check that covariates have expected number of rows
  if (nrow(covariates) != max(jj)) {
    stop("The 'covariates' must have a number of rows equal to the ",
         "number of persons for wide format data. For long format data, ",
         "the covariates should have a row count equal the length of 'y'")
  }

  # Apply the formula to the covariates, validating if requested
  if (validate_regression) {
    W <- .validate_regression_model(formula, covariates)
  } else {
    W <- model.matrix(formula, covariates)
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


#' Rescale continuous covariates as appropriate for edstan models
#'
#' @param x A numeric vector, matrix, or data frame
#' @return A numeric vector, matrix, or data frame with rescaled covariates
#'   having mean of zero and standard deviation of 0.5.
#' @examples
#' vec <- rnorm(5, 100, 20)
#' rescale_continuous(vec)
#'
#' mat <- matrix(rnorm(5*5, 100, 20), ncol = 5)
#' rescale_continuous(mat)
#' @export
rescale_continuous <- function(x) {
  f <- function(y) (y - mean(y)) / sd(y) / 2
  d <- length(dim(x))
  if (is.vector(x)) {
    return(f(x))
  } else if (length(dim(x)) == 2) {
    return(apply(x, 2, f))
  } else {
    stop("Error: Input must be a vector, matrix, or data frame.")
  }
}


#' Rescale binary covariates as appropriate for edstan models
#'
#' @param x A numeric vector, matrix, or data frame
#' @return A numeric vector, matrix, or data frame with rescaled covariates
#'   having mean of zero and range (maximum - minimum) of one.
#' @examples
#' vec <- c(1, 3, 1, 3, 1)
#' rescale_binary(vec)
#'
#' mat <- matrix(c(1, 3, 1, 3, 1), nrow = 5, ncol = 5)
#' rescale_binary(mat)
#' @export
rescale_binary <- function(x) {
  f <- function(y)  (y - mean(y)) / (max(y) - min(y))
  if (is.vector(x)) {
    return(f(x))
  } else if (length(dim(x)) == 2) {
    return(apply(x, 2, f))
  } else {
    stop("Error: Input must be a vector, matrix, or data frame.")
  }
}


#' Convert covariate data frame for long format data
#'
#' Intended for internal use only.
#'
#' @param covariates A data frame containing covariates.
#' @param jj Index for person associated with each row.
#' @param not_missing Boolean for whether each row is associated with a missing
#'   item response.
#' @return A data frame with one row per person.
#' @keywords internal
.long_format_covariates <- function(covariates, jj, formula) {

  mm <- model.matrix(formula, covariates)
  split_df <- split(as.data.frame(mm), jj)
  unique_rows_by_person <- sapply(split_df, function(x) nrow(unique(x)))

  if (any(unique_rows_by_person > 1)) {
    stop("Error: For long format data, all 'covariate' rows for each person ",
         "must be identical.")
  }

  covariates <- covariates[!duplicated(jj), , drop=FALSE]

  return(covariates)

}


#' Validate a boolean covariate
#'
#' Intended for internal use only.
#'
#' @param x A vector of covariate values.
#' @param nm Name for the covariate.
#' @return A character vector of identified issues.
#' @keywords internal
.validate_binary <- function(x, nm) {
  x_mean <- mean(x)
  x_range <- max(x) - min(x)
  issues <- c()

  if (all.equal(0, x_mean, tolerance = .01) != TRUE) {
    text <- paste(nm, ": binary covariate has mean of",
                  sprintf("%0.3f", x_mean), "rather than 0")
    issues <- append(issues, c(text))
  }

  if (all.equal(1, x_range, tolerance = .01) != TRUE) {
    text <- paste(nm, ": binary covariate has range of",
                  sprintf("%0.3f", x_range), "rather than 1")
    issues <- append(issues, c(text))
  }

  return(issues)

}


#' Validate a continuous covariate
#'
#' Intended for internal use only.
#'
#' @param x A vector of covariate values.
#' @param nm Name for the covariate.
#' @return A character vector of identified issues.
#' @keywords internal
.validate_continuous <- function(x, nm) {
  x_mean <- mean(x)
  x_sd <- sd(x)
  issues <- c()

  if (all.equal(0, x_mean, tolerance = .01) != TRUE) {
    text <- paste(nm, ": continuous covariate has mean of",
                  sprintf("%0.3f", x_mean), "rather than 0")
    issues <- append(issues, c(text))
  }

  if (all.equal(.5, x_sd, tolerance = .01) != TRUE) {
    text <- paste(nm, ": continuous covariate has SD of",
                  sprintf("%0.3f", x_sd), "rather than .5")
    issues <- append(issues, c(text))
  }

  return(issues)

}


#' Validate formula and covariate data
#'
#' Intended for internal use only.
#'
#' @param formula A formula for the latent regression.
#' @param data A data frame of covariates.
#' @return A character vector of identified issues.
#' @keywords internal
.validate_regression_model <- function(formula, data) {
  mm <- model.matrix(formula, data)
  model_terms <- terms(formula, data = data)

  # Check for missing values
  if (any(is.na(mm))) {
    stop("Error: The 'covariates' must not contain NA values.")
  }

  issues <- c()

  if (attr(model_terms, "intercept") == 0) {
    issues <- append(issues, c("formula does not include an intercept term"))
  }

  not_interactions <- which(attr(model_terms, "order")  == 1)
  columns_to_inspect <- which(attr(mm, "assign") %in% not_interactions)

  for (i in columns_to_inspect) {
    cname <- colnames(mm)[i]
    x <- mm[, i]
    u <- length(unique(x))

    if (u == 1) {
      issues <- append(issues, c(paste0(cname, "is constant")))
    } else if (u == 2) {
      issues <- append(issues, .validate_binary(x, cname))
    } else {
      issues <- append(issues, .validate_continuous(x, cname))
    }

  }

  if (length(issues)) {
    message("Potential scaling problems were observed for the covariates:")

    message(paste(
      paste("  -", issues),
      collapse = "\n"
    ))

    message(paste(
      "Recommendations for scaling appropriately with the coefficent priors:",
      "  - An intercept term should be included in the regression",
      "  - Continuous covariates should have a mean of 0 and SD of .5",
      "  - Binary covariates should have a mean of 0 and range (max minus min) of 1",
      "  - Other scalings may be sensible but cannot be validated with this function",
      sep = "\n"
    ))

    warning("Rescaling the regression covariates may be appropriate")

  }

  return(mm)

}
