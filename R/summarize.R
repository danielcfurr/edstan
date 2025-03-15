#' View a table of selected parameter posteriors after using \code{irt_stan}
#'
#' @param fit A \code{stanfit-class} object created by \code{\link{irt_stan}}.
#' @param data_list An optional Stan data list created with
#'   \code{\link{irt_data}}. If provided, the printed posterior summaries for
#'   selected parameters are grouped by item. Otherwise, ungrouped results are
#'   provided, which may be preferred, for example, for the Rasch or rating
#'   scale models.
#' @param ... Additional options passed to \code{\link[base]{print}}.
#' @examples
#' # Make a suitable data list:
#' spelling_list <- irt_data(response_matrix = spelling[, 2:5],
#'                           covariates = spelling[, "male", drop = FALSE],
#'                           formula = ~ 1 + male)
#'
#'\dontrun{
#' # Fit a latent regression  2PL
#' twopl_fit <- irt_stan(spelling_list, model = "2pl_latent_reg.stan",
#'                       iter = 300, chains = 4)
#'
#' # Get a table of parameter posteriors
#' print_irt_stan(twopl_fit, spelling_list)
#' # Or
#' print_irt_stan(twopl_fit)
#' }
#' @export
print_irt_stan <- function(fit, data_list = NULL, ...) {

  possible_pars <- c("alpha", "beta", "kappa", "lambda", "sigma")
  available <- possible_pars %in% fit@model_pars
  names(available) <- possible_pars

  if(is.null(data_list)) {

    capture <- utils::capture.output(
      print(fit, pars = possible_pars[available], ...)
    )

  } else {

    # Get number of beta parameters per item
    y <- data_list$y
    ii <- data_list$ii
    if(available["kappa"]) {
      m <- rep(1, times = max(ii))  # For rating scale models
    } else {
      m <- tapply(y, ii, max)       # For binary/partial credit models
    }

    # Make a list of groups of item parameter by item
    out_list <- list(paste0("beta[", 1:m[1], "]"))
    for(i in 2:max(ii)) {
      out_list[[i]] <- paste0("beta[", (sum(m[1:(i-1)])+1):sum(m[1:i]), "]")
    }
    if(available["alpha"]) {
      for(i in 1:max(ii)) {
        out_list[[i]] <- c(paste0("alpha[", i, "]"), out_list[[i]])
      }
    }

    # Make labels for the items
    if(is.null(names(data_list$ii))) {
      out_labels <- paste("Item", unique(data_list$ii))
    } else {
      out_labels <- paste0("Item ", unique(data_list$ii), ": ",
                           unique(names(data_list$ii)))
    }

    # Add kappas to the list and add a label, if needed
    if(available["kappa"]) {
      out_list[[length(out_list) + 1]] <- "kappa"
      out_labels[length(out_labels) + 1] <- "Rating scale step parameters"
    }

    # Add ability distribution parameters to the list and add a label
    if(available["sigma"]) {
      out_list[[length(out_list) + 1]] <- c("lambda", "sigma")
    } else {
      out_list[[length(out_list) + 1]] <- "lambda"
    }
    out_labels[length(out_labels) + 1] <- "Ability distribution"

    # Get print() output and reformat
    capture <- utils::capture.output(print(fit, pars = unlist(out_list), ...))
    blanks <- grep("^$", capture)
    capture[blanks[1]:blanks[2]] <- paste0("  ", capture[blanks[1]:blanks[2]])
    for(i in 1:length(out_list)) {
      search_str <- gsub("\\[", "\\\\[", out_list[[i]][1])
      idx <- min(grep(paste0("^  ", search_str), capture))
      capture <- c(capture[1:(idx-1)], out_labels[i],
                   capture[idx:length(capture)])
    }

  }

  cat(capture, sep = "\n")

}


#' View a plot of summary statistics after using \code{irt_stan}
#'
#' @param fit A \code{stanfit-class} object created by \code{\link{irt_stan}}
#'   or \code{\link[rstan]{stan}}.
#' @param stat A string for the statistic from the \code{summary} method for a
#'   \code{stanfit} object to plot. The default is "Rhat" but could, for
#'   example, be "mean" or "n_eff".
#' @param ... Additional options (such as \code{pars} or \code{use_cache}),
#'   passed to the \code{summary} method for a \code{stanfit} object. Not
#'   required.
#' @return A \code{ggplot} object.
#' @seealso See \code{\link[rstan]{stan_rhat}}, which provides a histogram of
#'   Rhat statistics.
#' @examples
#' # Make a suitable data list:
#' spelling_list <- irt_data(response_matrix = spelling[, 2:5],
#'                           covariates = spelling[, "male", drop = FALSE],
#'                           formula = ~ 1 + male)
#'
#'\dontrun{
#' # Fit a latent regression  2PL
#' twopl_fit <- irt_stan(spelling_list, model = "2pl_latent_reg.stan",
#'                       iter = 300, chains = 4)
#'
#' # Get a plot showing Rhat statistics
#' rhat_columns(twopl_fit)
#'
#' # Get a plot showing number of effective draws
#' rhat_columns(twopl_fit, stat = "n_eff")
#' }
#' @export
stan_columns_plot <- function(fit, stat = "Rhat", ...) {

  fit_summary <- as.data.frame(rstan::summary(fit, ...)[["summary"]])

  # Creating vector before adding to data.frame helps pass CRAN checks.
  Parameter <- as.factor(gsub("\\[.*]", "", rownames(fit_summary)))
  value_to_plot <- fit_summary[, stat]
  fit_summary$Parameter <- Parameter
  fit_summary$value_to_plot <- value_to_plot

  ggplot2::ggplot(fit_summary) +
    ggplot2::aes(x = Parameter, y = value_to_plot, color = Parameter) +
    ggplot2::geom_jitter(height = 0, width = 0.25, show.legend = FALSE) +
    ggplot2::ylab(stat)

}


#' Read and print the code for an edstan model
#'
#' This function reads a file from the `inst/extdata/` directory of the package,
#' returning its contents invisibly while optionally printing them.
#'
#' @param filename The name of the stan file.
#' @param print Whether to print the stan file contents. Default is `TRUE`.
#' @return Invisibly returns a character vector of the stan file contents.
#' @export
edstan_model_code <- function(filename, print = TRUE) {
  file_path <- system.file("extdata", filename, package = "edstan")

  if (file_path == "") {
    stop("File not found in inst/extdata/: ", filename)
  }

  contents <- readLines(file_path, warn = FALSE)

  if (print) cat(contents, sep = "\n")

  invisible(contents)

}

