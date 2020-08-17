#' Plot convergence diagnostics
#'

convergence_plot <- function(
  fit,
  include = NA,
  exclude = "y_rep",
  stats = c("n_eff", "Rhat")
  ) {

  parameters <- names(extract(fit))
  if (!is.na(include)) parameters <- parameters[parameters %in% include]
  if (!is.na(exclude)) parameters <- parameters[!parameters %in% exclude]

  s <- rstan::summary(as(fit, "stanfit"), pars = parameters, probs = NA)
  s <- s[["summary"]]

  df_list <- lapply(stats, function(stat) {
    data.frame(
      stat = stat,
      parameter = sub("^([a-zA-Z_]+).*$", "\\1", rownames(s)),
      value = s[, stat]
    )
  })

  df <- do.call(rbind, df_list)
  df$parameter <- factor(df$parameter, levels = rev(parameters))

  ggplot2::ggplot(df) +
    ggplot2::aes(x = parameter, y = value) +
    ggplot2::geom_jitter(color = "blue", height = 0, width = 0.25, alpha = .5) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~stat, scales = "free_x")

}
