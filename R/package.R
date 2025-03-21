#' Stan for item response theory
#'
#' \pkg{edstan} streamlines the fitting of Bayesian item response
#' theory models using \pkg{rstan}.
#'
#' A typical workflow in fitting a model using \pkg{edstan} involves the
#' following sequence:
#'
#' \enumerate{
#'   \item \code{\link{irt_data}} to format the data,
#'   \item \code{\link{irt_stan}} to fit a model,
#'   \item \code{\link{stan_columns_plot}} to view sampling diagnostics, and
#'   \item \code{\link{print_irt_stan}} to view parameter summaries.
#' }
#'
#' The package includes six Stan item response models
#' (see \code{\link{irt_stan}} for a list) and two example datasets
#' (\code{\link{aggression}} and \code{\link{spelling}}). It is expected that
#' once that a user is comfortable utilizing the preceding workflow with the
#' pre-defined \pkg{edstan} models, they will go on to write their own Stan
#' models.
#'
#' @import rstan
"_PACKAGE"
#> [1] "_PACKAGE"
