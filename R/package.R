#' Stan for item response theory
#'
#' \pkg{edstan} streamlines the fitting of Bayesian item response
#' theory models using \pkg{rstan}.
#'
#' A user will generally want to use the following functions to fit a model:
#'
#' \enumerate{
#'   \item \code{\link{irt_data}} to format the data,
#'   \item \code{\link{irt_stan}} to fit a model,
#'   \item \code{\link{stan_columns_plot}} to view sampling diagnostics, and
#'   \item \code{\link{print_irt_stan}} to view parameter summaries.
#' }
#'
#' The package also includes six Stan
#' models (see \code{\link{irt_stan}} for a list) and two example datasets
#' (\code{\link{aggression}} and \code{\link{spelling}}).
#'
#' It is expected that once a user is comfortable fitting pre-defined
#' \pkg{edstan} models, they will write their own Stan models and fit them with
#' \code{\link[rstan]{stan}}, for which \code{\link{irt_stan}} is a wrapper.
#'
#' @import rstan
"_PACKAGE"
#> [1] "_PACKAGE"
