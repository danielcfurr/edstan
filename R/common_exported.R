################################################################################
# Exported functions (and the common Reference Class)
#
# The user will have direct access to these functions and classes. For this
# reason, everything in this file requires help documentation.
################################################################################

# This is a bit of roxygen documentation that indicates that rstan is to be
# loaded when edstan is loaded. Necessary for get_model_stan() to work for some
# reason I don't understand.
#' @import rstan
NULL


################################################################################
# common Reference Class

#' The common Reference Class for edstan.
#'
#' @field fit A \code{stanfit} object.
#' @field data The data list used in stan model fitting.
#' @seealso
#' The \code{common_stanfit} methods are closely related to \code{rstan} and
#' \code{edstan} functions. See \code{rstan} functions
#' \code{\link[rstan]{pairs.stanfit}}, \code{\link[rstan]{plot-method}},
#' \code{\link[rstan]{traceplot}}, and \code{\link[rstan]{get_stancode}}. See
#' \code{edstan} functions \code{\link{get_parameters}},
#' \code{\link{get_mean_logLik}}, \code{\link{get_best_logLik}},
#' \code{\link{get_dic}}, and \code{\link{plot_autocor}}.
#' @export
common_stanfit <- setRefClass("common_stanfit",
                              fields = c("fit", "data"))

# Existing Stan methods:

common_stanfit$methods(
  pairs = function(...) {
    "Plot pairs of posterior parameter draws."
    graphics::pairs(fit, ...)
  })

common_stanfit$methods(
  plot = function(...) {
    "Plot posterior credible intervals."
    rstan::plot(fit, ...)
  })

common_stanfit$methods(
  traceplot = function(pars, ...) {
    "Draw traceplots."
    rstan::traceplot(fit, pars, ...)
  })

common_stanfit$methods(
  stancode = function() {
    "Return the Stan code"
    cat(rstan::get_stancode(fit))
  })

# New Stan methods:

common_stanfit$methods(
  parameters = function(...) {
    "Retrieve a data frame of parameter estimates."
    get_parameters(fit, ...)
  })

common_stanfit$methods(
  mean_logLik = function() {
    "Get the mean posterior log-likelihood."
    get_mean_logLik(fit)
  })

common_stanfit$methods(
  best_logLik = function() {
    "Get the posterior log-likelihood evaluated at posterior parameter means."
    get_best_logLik(fit, data)
  })

common_stanfit$methods(
  dic = function() {
    "View mean deviance, deviance evaluated at posterior parameter means, and DIC."
    get_dic(fit, data)
  })

common_stanfit$methods(
  autocor = function(pars, ...) {
    "Plot auto-correlations for parameter draws."
    plot_autocor(fit, pars, ...)
  })



################################################################################
# Exported functions

#' Retrieve a data frame of parameter estimates from a \code{stanfit} object.
#'
#' @param fit A \code{stanfit} object.
#' @param ... Additional parameters passed to \code{\link[rstan]{summary}}.
#' @return A data frame.
#' @examples
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#' all_parameters <- get_parameters(stan_fit)
#' just_thetas <- get_parameters(stan_fit, pars = "theta")
#' @export
get_parameters <- function(fit, ...) {
  whole_summary <- rstan::summary(fit, ...)
  output <- as.data.frame(whole_summary[["summary"]])
  return(output)
}


#' Get the mean posterior log-likelihood from a \code{stanfit} object.
#'
#' @param fit A \code{stanfit} object.
#' @return The mean posterior log-likelihood.
#' @examples
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#' ll <- get_mean_logLik(stan_fit)
#' @export
get_mean_logLik <- function(fit) {
  sim <- slot(fit, "sim")
  log_list <- rstan::get_logposterior(fit)
  log_likelihoods <- NULL
  for(i in 1:length(log_list)) {
    full_chain <- log_list[[i]]
    post_warmup <- full_chain[-1*(1:sim$warmup2[i])]
    log_likelihoods <- c(log_likelihoods, post_warmup)
  }
  ll <- mean(log_likelihoods)
  return(ll)
}


#' Get the posterior log-likelihood evaluated at posterior parameter means.
#'
#' @param fit A \code{stanfit} object.
#' @param data The data list provided in to stan.
#' @return The posterior log-likelihood evaluated at posterior parameter means.
#' @examples
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#' ll <- get_best_logLik(stan_fit)
#' @export
get_best_logLik <- function(fit, data) {
  original_inits <- rstan::get_inits(fit)
  pars <- names(original_inits[[1]])
  posterior_means <- list()
  for(n in pars) {
    par_table <- get_parameters(fit, pars = n)
    posterior_means[[n]] <- par_table$mean
  }
  posterior_as_inits = list()
  posterior_as_inits[[1]] <- posterior_means
  trash <- capture.output(
    eval <- rstan::stan(fit = fit,
                        chains = 1,
                        iter = 1,
                        data = data,
                        init = posterior_as_inits)
  )
  ll <- rstan::get_logposterior(eval)[[1]]
  return(ll)
}

#' View mean deviance, deviance evaluated at posterior parameter means, and DIC.
#'
#' @param fit A \code{stanfit} object.
#' @param data The data list provided in to stan.
#' @return The posterior log-likelihood evaluated at posterior parameter means.
#' @examples
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#' get_dic(stan_fit)
#' @export
get_dic <- function(fit, data) {
  mean_dev <- -2 * get_mean_logLik(fit)
  best_dev <- -2 * get_best_logLik(fit, data)
  p <- mean_dev - best_dev
  DIC <- mean_dev + p
  output <- c(DIC = DIC, mean_dev = mean_dev, best_dev = best_dev)
  return(output)
}


#' Plot an item characteristic curve.
#'
#' @param fit        A \code{stanfit} object. Required if \code{alpha}, \code{beta}, or \code{gamma} is not specificied.
#' @param item       Either a single integer for the item to be looked up \code{item_names}. Required if \code{alpha}, \code{beta}, or \code{gamma} is not specificied.
#' @param new        Indicates whether a new plot should be made. Otherwise, the item characteristic curve is drawn on the active plot. Default is TRUE.
#' @param alpha      A value for the discrimination parameter. Required if \code{fit} is not supplied.
#' @param beta       A value for the difficulty parameter. Required if \code{fit} is not supplied.
#' @param gamma      A value for the pseudo-guessing parameter. Required if \code{fit} is not supplied.
#' @param theta      A vector of abilities to plot.
#' @param ...        Additional options passed to \code{\link{plot}} or \code{\link{lines}}.
#' @return
#' A single plotted item characteristic curve. If any parameter \code{alpha},
#' \code{beta}, or \code{gamma} is not supplied, the function attempts to look
#' up the values using the \code{stanfit} supplied to the \code{fit} option. This look up
#' requires an item number specified in \code{item}.
#' @examples
#' plot_icc(alpha = 1, beta = 0, gamma = 0)
#' plot_icc(alpha = 1.2, beta = -1, gamma = .2)
#'
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#' items <- myfit$item_names
#' plot_icc(fit = stan_fit, item = 1, gamma = 0)
#' @export
plot_icc <- function(fit=NULL, item=NULL, new = TRUE,
                     alpha = NULL, beta = NULL, gamma = NULL,
                     theta = seq(from=-2, to=2, length.out=100),
                     ...) {

  if(is.null(beta)) {
    beta_get <- paste("beta[", item, "]", sep = "")
    beta <- get_parameters(fit, pars = beta_get)$mean
  }

  if(is.null(alpha)) {
    alpha_get <- paste("alpha[", item, "]", sep = "")
    alpha <- get_parameters(fit, pars = alpha_get)$mean
  }

  if(is.null(gamma)) {
    gamma_get <- paste("gamma[", item, "]", sep = "")
    gamma <- get_parameters(fit, pars = gamma_get)$mean
  }

  if(new) {
    plot(x = theta,
         y = gamma + (1-gamma) / (1 + exp(-alpha*(theta - beta))),
         type = "l",
         ...)
  } else {
    lines(x = theta,
         y = gamma + (1-gamma) / (1 + exp(-alpha*(theta - beta))),
         ...)
  }

}


#' Plot (or get a table of) auto-correlations for parameter draws.
#'
#' @param fit         A \code{stanfit} object.
#' @param pars        Parameters for which to calculate auto-correlations.
#' @param back        Number of iterations back to calculate auto-correlations. Default is 10.
#' @param show_matrix Whether the correlations should be returned as a matrix. Default is FALSE.
#' @param show_plot   Whether to draw the plots. Default is TRUE.
#' @return
#' If \code{show_plot = TRUE}, a matrix of plots of the auto-correlations for
#' the chosen parameters. If \code{show_table = TRUE}, the auto-correlations in
#' the form of a matrix. Both may be requested.
#' @examples
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#' plot_autocor(stan_fit, pars = "alpha")
#' autocorrs <- plot_autocor(stan_fit, pars = c("alpha", "beta"), show_matrix = TRUE)
#' @export
plot_autocor <- function(fit, pars, back = 10, show_matrix = FALSE, show_plot = !show_matrix) {

  ext <- rstan::extract(myfit$fit,
                        pars = pars,
                        permute = FALSE,
                        inc_warmup = FALSE)
  pars <- dimnames(ext)$parameters

  correlations <- matrix(NA, nrow = back, ncol = length(pars))
  colnames(correlations) <- pars

  for(j in 1:length(pars)) {
    x <- ext[,,pars[j]]
    for(i in 1:back) {
      current <- delayed <- matrix(NA,
                                   ncol = ncol(x),
                                   nrow = nrow(x) - i)
      current <- x[1:(nrow(x) - i), ]
      current.vector <- as.vector(current)
      delayed <- x[(i + 1):nrow(x), ]
      delayed.vector <- as.vector(delayed)
      correlations[i, j] <- cor(current.vector, delayed.vector, use = "complete.obs")
    }
  }

  if(show_plot) {

    if(length(pars) > 1) {
      layout.vec <- rep(0, times = 2 * ceiling(length(pars) / 2))
      layout.vec[1:length(pars)] = 1:length(pars)
      layout.mat <- matrix(layout.vec,
                           ncol = 2,
                           nrow = ceiling(length(pars) / 2),
                           byrow = TRUE)
      nf <- layout(layout.mat)
      layout.show(nf)
    }
    for(j in 1:length(pars)) {
      plot(1:10, correlations[, j],
           main = paste("Autocorrelation for", pars[j]),
           xlab = "Distance",
           ylab = NA,
           ylim = range(correlations),
           type = "n")
      segments(x0 = 1:back,
               x1 = 1:back,
               y0 = rep(0, times = back),
               y1 = correlations[, j])
      abline(h = 0, col = "gray")
    }

  }

  if(show_matrix) return(correlations)

}


# Accepts name for .stan file. Loads the associated stan_model from Rdata file,
# or creates it if it does not exist.

#' Load or save a stan model from a .Rdata file.
#'
#' @param model A string for the name of an \code{edstan} model.
#' @return
#' Returns a Stan model object for the specified model..
#' @details
#' This function is used to create a Stan model object from a Stan program file
#' and then to save the Stan model object in an .Rdata file. The usual use for
#' this function is create the .Rdata file the first time a given
#' \code{edstan} wrapper is called. On subsequent calls to the given wrapper,
#' the Stan model object is just loaded from the .Rdata file, improving the
#' speed of the estimation. In general, users will have no need to use this
#' function directly.
#' @examples
#' rasch_model_obj <- get_model_stan("rasch")
#'
#' # If we wanted to call this for every edstan model at once while we take
#' # a lunch break:
#' folder <- system.file("extdata", package = "edstan")
#' files <- dir(folder)
#' for(f in files) {
#'   if(grepl("[.]stan$", f)) {
#'     model <- sub("[.]stan$", "", f)
#'     get_model_stan(model)
#'   }
#' }
#' @export
get_model_stan <- function(model) {

  rdata_file <- file.path(system.file("extdata", package = "edstan"),
                          paste(model, ".Rdata", sep = ""))
  stan_file <- file.path(system.file("extdata", package = "edstan"),
                         paste(model, ".stan", sep = ""))

  if(!file.exists(stan_file)) stop("Stan program file not found: ", stan_file)

  # If the Rdata file exists, load its contents. If not, try to create it.
  if(file.exists(rdata_file)) {

    load(rdata_file)

  } else {

    stanmodel_obj <- rstan::stan_model(file = stan_file,
                                       model_name = model,
                                       save_dso = TRUE)
    try(save(stanmodel_obj, file = rdata_file))

    # Let user know that file was written (or not).
    if(file.exists(rdata_file)) {
      message("Stan model file saved: ", rdata_file)
    } else {
      warn("Failed to save Stan model file: ", rdata_file)
    }

  }

  return(stanmodel_obj)

}
