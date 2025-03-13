#' Estimate an item response model with Stan
#'
#' @param data_list A Stan data list created with \code{\link{irt_data}}.
#' @param model The file name for one of the provided .stan files, or
#'   alternatively, a user-created .stan file that accepts \code{data_list} as
#'   input data.
#'   The ".stan" file extension may be omitted.
#'   Defaults to either "rasch_latent_reg.stan" or "pcm_latent_reg.stan".
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
#'    \strong{Model}             \tab \strong{File}                \cr
#'    Rasch                      \tab \emph{rasch_latent_reg.stan} \cr
#'    Partial credit             \tab \emph{pcm_latent_reg.stan}   \cr
#'    Rating Scale               \tab \emph{rsm_latent_reg.stan}   \cr
#'    Two-parameter logistic     \tab \emph{2pl_latent_reg.stan}   \cr
#'    Generalized partial credit \tab \emph{gpcm_latent_reg.stan}  \cr
#'    Generalized rating Scale   \tab \emph{grsm_latent_reg.stan}
#' }
#'
#' Three simplified models are also available: \emph{rasch_simple.stan},
#' \emph{pcm_simple.stan}, \emph{rsm_simple.stan}. These are (respectively) the
#' Rasch, partial credit, and rating scale models omitting the latent
#' regression. There is no reason to use these instead of the models listed
#' above, given that the above models allow for rather than require the
#' inclusion of covariates for a latent regression. Instead, the purpose of
#' the simplified models is to provide a straightforward starting point
#' researchers who wish to craft their own Stan models.
#'
#' @return A \code{\link[rstan]{stanfit-class}} object.
#' @seealso See \code{\link[rstan]{stan}}, for which this function is a wrapper,
#'   for additional options.
#'   See \code{\link{irt_data}} and \code{\link{labelled_integer}} for functions
#'   that facilitate creating a suitable \code{data_list}.
#'   See \code{\link{print_irt_stan}} and \code{\link[rstan]{print.stanfit}} for
#'   ways of getting tables summarizing parameter posteriors.
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
#'\dontrun{
#' # Fit the Rasch and 2PL models on wide-form data with a latent regression
#'
#' spelling_list <- irt_data(response_matrix = spelling[, 2:5],
#'                           covariates = spelling[, "male", drop = FALSE],
#'                           formula = ~ 1 + male)
#'
#' rasch_fit <- irt_stan(spelling_list, iter = 300, chains = 4)
#' print_irt_stan(rasch_fit, spelling_list)
#'
#' twopl_fit <- irt_stan(spelling_list, model = "2pl_latent_reg.stan",
#'                       iter = 300, chains = 4)
#' print_irt_stan(twopl_fit, spelling_list)
#'
#'
#' # Fit the rating scale and partial credit models without a latent regression
#'
#' agg_list_1 <- irt_data(y = aggression$poly,
#'                        ii = labelled_integer(aggression$description),
#'                        jj = aggression$person)
#'
#' fit_rsm <- irt_stan(agg_list_1, model = "rsm_latent_reg.stan",
#'                     iter = 300, chains = 4)
#' print_irt_stan(fit_rsm, agg_list_1)
#'
#' fit_pcm <- irt_stan(agg_list_1, model = "pcm_latent_reg.stan",
#'                     iter = 300, chains = 4)
#' print_irt_stan(fit_pcm, agg_list_1)
#'
#'
#' # Fit the generalized rating scale and partial credit models including
#' # a latent regression
#'
#' agg_list_2 <- irt_data(y = aggression$poly,
#'                        ii = labelled_integer(aggression$description),
#'                        jj = aggression$person,
#'                        covariates = aggression[, c("male", "anger")],
#'                        formula = ~ 1 + male*anger)
#'
#' fit_grsm <- irt_stan(agg_list_2, model = "grsm_latent_reg.stan",
#'                      iter = 300, chains = 4)
#' print_irt_stan(fit_grsm, agg_list_2)
#'
#' fit_gpcm <- irt_stan(agg_list_2, model = "gpcm_latent_reg.stan",
#'                      iter = 300, chains = 4)
#' print_irt_stan(fit_grsm, agg_list_2)
#' }
#' @export
irt_stan <- function(data_list, model = "", ... ) {

  max_y <- tapply(data_list$y, data_list$ii, max)
  is_polytomous <- any(max_y > 1)

  # Choose Stan model file if one not provided. If provided, add ".stan" to Stan
  # file if needed. Look for file first in working directory/given file path. If
  # not found, look up in package install folder.
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
  fit <- rstan::stan(stan_file, data = data_list, ...)
  return(fit)

}
