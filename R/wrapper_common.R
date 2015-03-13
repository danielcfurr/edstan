################################################################################
# common reference class

#' The common Reference Class for edstan.
#'
#' @field fit A \code{stanfit} object.
#' @field data The data list used in stan model fitting.
#' @seealso 
#' The \code{common_stanfit} methods are closely related to \code{rstan} and \code{edstan} functions. 
#' See \code{rstan} functions \code{\link[rstan]{pairs.stanfit}}, 
#' \code{\link[rstan]{plot-method}}, and \code{\link[rstan]{traceplot}}.
#' See \code{edstan} functions \code{\link{get_parameters}}, 
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
# Outward facing functions

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


################################################################################
# Inward facing functions

match_id_stan <- function(x) {
  old_id_vector <- as.character(x)
  unique_strings <- unique(old_id_vector)
  unique_new_ids <- 1:length(unique_strings)
  names(unique_new_ids) <- unique_strings
  new_id_vector <- unique_new_ids[old_id_vector]
  output <- data.frame(old = old_id_vector,
                       new = new_id_vector,
                       stringsAsFactors = FALSE )
  return(output)
}


# Provide a list of vectors. Returns TRUE if they differ in length.
check_lengths_stan <- function(x) {
  
  lengths <- NULL
  for(vec in x) {
    lengths <- c(lengths, length(vec))
  }
  
  different_lengths <- min(lengths) != max(lengths)
  
  return(different_lengths)
  
}


# Provide a list of vectors. Returns TRUE if any contain NAs.
have_na_stan <- function(x) {
  
  counts_na <- NULL
  for(vec in x) {
    counts_na <- c(counts_na, sum(is.na(vec)) )
  }
  
  found_na <- any(counts_na > 0)
  
  return(found_na)
  
}


print_header_stan <- function(fit) {
  sim <- slot(fit, "sim")
  cat("Inference for Stan model: \n")
  cat(sim$chains, " chains, each with iter=", sim$iter, 
      "; warmup=", sim$warmup, "; thin=", sim$thin, ";\n", sep = "")
  cat("post-warmup draws per chain=", sim$warmup, 
      "; total post-warmup draws=", sum(sim$warmup2), ".", sep = "")
  cat("\n")
}


# Wrapper for printing Stan results.
# Provide names for rownames in output as named lists.
print_stan <- function(fit, 
                       pars,
                       names = NULL,
                       title = "",
                       decimals = 2,
                       ... ) {
  
  summary_table <- get_parameters(fit, pars = pars)
  se_mean <- grep("se_mean", colnames(summary_table))
  n_eff <- grep("n_eff", colnames(summary_table))
  Rhat <- grep("Rhat", colnames(summary_table))
  
  bad_rhat <- summary_table[, Rhat] < 1
  summary_table[bad_rhat, Rhat] <- 1
  
  fmt <- paste("%1.", decimals, "f", sep = "")
  fmt2 <- paste("%1.", decimals + 1, "f", sep = "")
  
  summary_table[, se_mean] <- sprintf(fmt2, summary_table[, se_mean])
  summary_table[, n_eff] <- sprintf("%1.0f", summary_table[, n_eff])
  summary_table[, Rhat] <- sprintf("%1.2f", summary_table[, Rhat])
  
  sub <- -1*c(se_mean, n_eff, Rhat)
  for(n in names(summary_table)[sub]) {
    summary_table[, n] <- sprintf(fmt, summary_table[, n])
  }
  
  output <- NULL
  for(i in pars) {
    index <- grep(paste("^", i, sep=""), rownames(summary_table))
    part_table <- summary_table[index, ]
    name_vec <- names[[i]]
    if(!is.null(name_vec)) rownames(part_table) <- name_vec
    output <- rbind(output, part_table)
  }
  
  if(title != "") cat("\n", title, "\n", "\n")
  print(output)
  
}


# Function to print table of results for vectorized parameters. Used by
# print_stan.
print_vector_stan <- function(fit, 
                              pars, 
                              title = "") {
  
  summary_table <- get_parameters(fit, pars = pars)
  
  output <- as.data.frame(matrix(NA, nrow=length(pars), ncol=5))
  names(output) <- c("n", "min(n_eff)", "max(n_eff)", "min(Rhat)", "max(Rhat)")
  rownames(output) <- pars
  
  for(i in 1:length(pars)) {
    index <- grep(paste("^", pars[i], sep=""), rownames(summary_table))
    part.table <- summary_table[index, ]
    output[i, 1] <- nrow(part.table)
    output[i, 2] <- min(part.table[, "n_eff"])
    output[i, 3] <- max(part.table[, "n_eff"])
    output[i, 4] <- min(part.table[, "Rhat"])
    output[i, 5] <- max(part.table[, "Rhat"])
  }
  
  bad_rhat <- output[, 4:5] < 1
  output[, 4:5][bad_rhat] <- 1
  output[, 1:3] <- sprintf("%1.0f", output[, 1:3])
  output[, 4:5] <- sprintf("%1.2f", output[, 4:5])
  
  if(title != "") cat("\n", title, "\n", "\n")
  print(output)
  
}
