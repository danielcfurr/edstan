#' An S4 class for a fitted edstan model.
#'
#' @slot edstan_options A list of options provided when fitting the model
edstanfit <- setClass(
  "edstanfit",
  contains = "stanfit",
  slots = c(edstan_options = "list")
)


setMethod(
  "show",
  signature = "edstanfit",
  definition = function(object) print(x = object)
)


setMethod(
  "summary",
  signature = "edstanfit",
  definition = function(
    object,
    probs = c(.1, .9),
    return_data_frame = TRUE
  ) {

    summarize_edstan_fit(object, probs, return_data_frame)

})


setMethod(
  "print",
  signature = "edstanfit",
  definition = function(
    x,
    probs = c(0.1, 0.9),
    digits_summary = 2
  ) {

    # Largely copied from rstan...

    if (x@mode == 1L) {
      cat("Stan model '", x@model_name, "' is of mode 'test_grad';\n",
          "sampling is not conducted.\n", sep = '')
      return(invisible(NULL))
    } else if (x@mode == 2L) {
      cat("Stan model '", x@model_name, "' does not contain samples.\n", sep = '')
      return(invisible(NULL))
    }

    # if(!include) pars <- setdiff(x@sim$pars_oi, pars)
    # s <- summary(x, pars, probs, ...)
    # if (is.null(s)) return(invisible(NULL))
    n_kept <- x@sim$n_save - x@sim$warmup2
    cat("Inference for Stan model: ", x@model_name, '.\n', sep = '')
    cat(x@sim$chains, " chains, each with iter=", x@sim$iter,
        "; warmup=", x@sim$warmup, "; thin=", x@sim$thin, "; \n",
        "post-warmup draws per chain=", n_kept[1], ", ",
        "total post-warmup draws=", sum(n_kept), ".\n\n", sep = '')


    # Begin changes for edstan ...

    tables <- summarize_edstan_fit(x, probs = probs, return_data_frame = FALSE)

    for(n in 1:length(tables)) {

      tbl <- tables[[n]]
      tbl[,"n_eff"] <- round(tbl[,"n_eff"], digits = 0)
      tbl <- round(tbl, digits_summary)

      cat(names(tables)[n], "\n")
      print(tbl)
      cat("\n")

    }

    # ... end changes for edstan

    sampler <- attr(x@sim$samples[[1]], "args")$sampler_t

    if (!is.null(x@stan_args[[1]]$method) &&
        x@stan_args[[1]]$method == "variational") {
      if ("diagnostics" %in% names(x@sim)
          & "ir_idx" %in% names(x@sim$diagnostics)
          & !is.null(x@sim$diagnostics$ir_idx)) {
        cat("\nApproximate samples were drawn using VB(", x@stan_args[[1]]$algorithm,
            ") + PSIS at ", x@date, ".\n", sep = '')
      } else {
        cat("\nApproximate samples were drawn using VB(", x@stan_args[[1]]$algorithm,
            ") at ", x@date, ".\n", sep = '')
        message("We recommend genuine 'sampling' from the posterior distribution for final inferences!")
      }
      return(invisible(NULL))
    } else {
      cat("\nSamples were drawn using ", sampler, " at ", x@date, ".\n",
          "For each parameter, n_eff is a crude measure of effective sample size,\n",
          "and Rhat is the potential scale reduction factor on split chains (at \n",
          "convergence, Rhat=1).\n", sep = '')
      return(invisible(NULL))
    }

  })


# Helper functions -------------------------------------------------------------

summarize_edstan_fit <- function(
  fit,
  probs = c(.1, .9),
  return_data_frame = TRUE
) {

  start <- fit@edstan_options$pos_s
  end <- fit@edstan_options$pos_e
  beta_base <- extract(fit, permuted = FALSE, pars = "beta_base")
  beta_step <- extract(fit, permuted = FALSE, pars = "beta_step")
  alpha <- extract(fit, permuted = FALSE, pars = "alpha")

  beta_label <- if (fit@edstan_options$flag_thresholds) "Thresh" else "Step"

  item_tables <- lapply(1:length(start), function(i) {

    difficulty_base <- beta_base[,,i,drop = FALSE]

    labels <- paste(beta_label, 1)

    if (start[i] > 0) {
      difficulty_steps <- beta_step[,,start[i]:end[i],drop = FALSE]
      difficulties <- array(c(difficulty_base, difficulty_steps),
                            dim = dim(difficulty_steps) + c(0, 0, 1))
      labels <- c(labels, paste(beta_label, 2:dim(difficulties)[3]))
    }

    if (fit@edstan_options$flag_thresholds) {
      difficulties <- aperm(apply(difficulties, 1:2, cumsum), c(2, 3, 1))
    }

    if (fit@edstan_options$flag_varying_slope) {
      item_summary <- array(c(alpha[,,i, drop = FALSE], difficulties),
                            dim = dim(difficulties) + c(0, 0, 1))
      labels <- c("Discrim", labels)
    } else {
      item_summary <- difficulties
    }

    dimnames(item_summary) <- list(
      iter = NULL,
      chain = NULL,
      parameter = labels
    )

    x <- monitor(item_summary, probs = probs, warmup = 0, print = FALSE)
    quantile_names <- grep("%$", colnames(x), value = TRUE)
    all_names <- c("mean", "se_mean", "sd", quantile_names, "n_eff", "Rhat")
    x <- as.matrix(x)[, all_names]
    x

  })

  names(item_tables) <- paste0(
    "Item ", fit@edstan_options$lookup_item,
    ": ", names(fit@edstan_options$lookup_item)
  )

  if (fit@edstan_options$flag_varying_slope) {
    ability_pars <- c("lambda")
    alpha_label <- NULL
  } else {
    ability_pars <- c("alpha", "lambda")
    alpha_label <- "Common discrim"
  }

  if (is.null(fit@edstan_options$covariates)) {
    lambda_label <- paste("Covar", 1:fit@par_dims$lambda)
  } else {
    lambda_label <-fit@edstan_options$covariates
  }

  a <- extract(fit, permuted = FALSE, ability_pars)
  a <- monitor(a, probs = probs, warmup = 0, print = FALSE)
  quantile_names <- grep("%$", colnames(a), value = TRUE)
  all_names <- c("mean", "se_mean", "sd", quantile_names, "n_eff", "Rhat")
  ability_table <- as.matrix(a)[, all_names]
  row.names(ability_table) <- c(alpha_label, lambda_label)

  tables <- c(item_tables, "Person distribution" = list(ability_table))

  if (return_data_frame) {

    df_list <- lapply(1:length(tables), function(i) {
      data.frame(group = names(tables)[[i]],
                 parameter = rownames(tables[[i]]),
                 tables[[i]],
                 row.names = NULL)
    })

    return( do.call(rbind, df_list) )

  } else {

    return(tables)

  }

}
