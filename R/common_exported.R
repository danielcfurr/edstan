################################################################################
# Exported functions (and the common Reference Class)
#
# The user will have direct access to these functions and classes. For this
# reason, everything in this file requires help documentation.
################################################################################

# This is a bit of roxygen documentation that indicates that rstan is to be
# loaded when edstan is loaded.
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
#' \code{edstan} functions \code{\link{get_parameters}} and
#' \code{\link{plot_autocor}}.
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
  autocor = function(pars, ...) {
    "Plot auto-correlations for parameter draws."
    plot_autocor(fit, pars, ...)
  })

common_stanfit$methods(
  posterior = function(pars, ...) {
    "Plot posterior density for parameter draws."
    plot_posterior(fit, pars, ...)
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


#' Plot kernal densities for parameter posteriors.
#'
#' @param fit         A \code{stanfit} object.
#' @param pars        Parameters for which to plot densities.
#' @param overall     Whether to plot the overall density. Default is TRUE.
#' @param chains      Whether to plot densities for individual chains. Default is TRUE.
#' @param legend      Whether to include a legend. Default is FALSE.
#' @param inc_warmup  Whether to include warmup draws. Default is FALSE.
#' @return
#' One kernal density plot per parameter.
#' @examples
#' # Get an example stanfit object.
#' myfit <- twopl_wide_stan(spelling[, 2:5], chains = 4, iter = 200)
#' stan_fit <- myfit$fit
#'
#' # Plot first discrimination parameter with a legend.
#' plot_posterior(stan_fit, pars = "alpha[1]", legend = TRUE)
#'
#' # Show plots for all discrimation parameters on same display.
#' plot_posterior(stan_fit, pars = "alpha")
#' @export
plot_posterior <- function(fit, pars = NULL, overall = TRUE, chains = TRUE,
                           legend = FALSE, inc_warmup = FALSE) {

  opts <- list(object = fit, permuted = FALSE, inc_warmup = inc_warmup)
  if(!is.null(pars)) opts[["pars"]] <- pars
  draws <- do.call(rstan::extract, opts)

  n_pars <- dim(draws)[3]
  n_chains <- dim(draws)[2]
  parameter_names <- dimnames(draws)[["parameters"]]

  if(n_pars > 10) warning("You have requested ", n_pars, " plots. ",
                          "Try specifying fewer parameters to plot via 'par'.")

  # Additional stuff for making more than one plot.
  if(n_pars > 1) {
    par_bkp <- par(no.readonly = TRUE)
    on.exit(try(par(par_bkp)))
    par(mfrow = c(ceiling(n_pars / 2), 2))
    par(mar =  c(2, 4, 4, 2) + 0.1)
  }

  # Collect titles, colors, and line weights for plotting.
  plot_names <- plot_colors <- plot_lwd <- c()
  if(overall) {
    plot_names <- c(plot_names, "Overall")
    plot_colors <- c(plot_colors, "black")
    plot_lwd <- c(plot_lwd, 2)
  }
  if(chains) {
    plot_names <- c(plot_names, paste("Chain", 1:n_chains))
    plot_colors <- c(plot_colors, rainbow(n_chains))
    plot_lwd <- c(plot_lwd, rep(1, times = n_chains))
  }

  for(i in 1:n_pars) {

    # Collect "density" objects for overall and/or each chain
    plot_densities <- list()
    if(overall) {
      plot_densities[["overall"]] <- density(draws[,,i])
    }
    if(chains) {
      for(j in 1:n_chains) {
        plot_densities[[paste("Chain", j)]] <- density(draws[,j,i])
      }
    }

    # Make blank plot with appropriate range
    ylim <- range(sapply(plot_densities, function(x) x[["y"]]))
    xlim <- range(sapply(plot_densities, function(x) x[["x"]]))
    plot(xlim, ylim,
         main = parameter_names[i],
         ylab = "Density",
         xlab = NA,
         type = "n")

    # Plot densities
    for(j in 1:length(plot_densities)) {
      lines(x = plot_densities[[j]][["x"]],
            y = plot_densities[[j]][["y"]],
            col = plot_colors[j],
            lwd = plot_lwd[j])
    }

    if(legend) {
      legend("topleft",
             legend = plot_names,
             col = plot_colors,
             lty = "solid",
             lwd = plot_lwd)
    }

  }

}

