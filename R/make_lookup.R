#' Create a lookup index
#'

make_lookup <- function(x) {
  labels <- as.character(unique(x))
  idx <- 1:length(labels)
  names(idx) <- labels
  return(idx)
}
