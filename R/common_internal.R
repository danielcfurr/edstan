################################################################################
# "Internal" functions
#
# Everying in this file is NOT "exported", meaning that the user will have no
# easy access to these functions. The functions are "internal". They do not
# necessarily need error-trapping and never need help file documentation.
################################################################################


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


finish_icc_method_inputs_stan <- function(item, inputs, item_names) {

  if(is.numeric(item)) {
    # Check that there is an item with this ID.
    if(item %in% 1:length(item_names) == FALSE) stop("Item ID is out of range.")
    inputs[["item"]] <- item
  } else {
    # Check that there is an item with this name. Look up its ID.
    search_string <- paste("^", item, "$", sep = "")
    item_number <- grep(search_string, item_names)
    # If item name is not found, item_number will have length = 0.
    if(length(item_number) == 0) stop("Item name not found.")
    inputs[["item"]] <- item_number
  }
  
  # Choose values of theta to plot if not specificed in inputs.
  if(is.null(inputs[["theta"]])) {
    estimated_thetas <- get_parameters(inputs[["fit"]], pars = "theta")$mean
    inputs[["theta"]] <- seq(from = min(estimated_thetas), 
                             to = max(estimated_thetas), length.out = 100)
  }

  return(inputs)

}


# Accept a response matrix, change to long-form. Use row and column names instead
# of numbers when available for id and item values.
response_matrix_to_long_stan <- function(x, permitted = c(0, 1, NA)) {
  
  x_name <- deparse(substitute(x))
  try(x <- as.matrix(x))
  
  # Check that x is a matrix.
  # If ncol(x) == 1, then it was a vector, not matrix.
  if(is.null(x) | ncol(x) == 1) {
    stop(paste(x_name, "must be a matrix or coercible to matrix"))
  }
  
  # Check that x contains only the permitted values.
  if(!all(x %in% permitted)) {
    permitted_char <- paste(permitted, collapse = ", ")
    stop(paste(x_name, "must contain only", permitted_char))
  }
  
  # Extract person id names or numbers.
  if(is.null(rownames(x))) {
    id <- rep(1:nrow(x), each = ncol(x))
  } else {
    id <- rep(rownames(x), each = ncol(x))
  }
  
  # Extract item id names or numbers.
  if(is.null(colnames(x))) {
    item <- rep(1:ncol(x), times = nrow(x))
  } else {
    item <- rep(colnames(x), times = nrow(x))
  }  
  
  # Extract responses
  response <- as.vector(t(x))
  
  not_missing <- !is.na(response)
  output <- list(id = id[not_missing],
                 item = item[not_missing],
                 response = response[not_missing])
  
  return(output)
  
}


# Provide named vectors. Checks whether inputs (1) are vectors, (2) have the
# same length and (3) have no NAs. Returns a list with 'bad' indicating TRUE if
# there is a problem and 'message' providing a description of the error. Use
# this function in conjunction with stop().
check_vectors_stan <- function(permitted = c("numeric"), ...) {
  
  inputs <- list(...)
  # Grabs the names of the objects passed to this function.
  names_inputs <- sapply(match.call()[-1], deparse)
  msg <- NULL
  
  # First check that the vectors (1) are not matrix, array, data.frame and then
  # (2) are numeric or character. (Factors are numeric).
  are_vectors <- sapply(inputs, function(x) is.null(dim(x)) & mode(x) %in% permitted )
  if(any(are_vectors == FALSE)) {
    new_msg <- paste("Must be a vector of type",
                     paste(permitted, collapse = "/"),
                     ":",
                     paste(names_inputs[!are_vectors], collapse = ", "))
    msg <- c(msg, "\n", new_msg)
  }
  
  # Only continue if all inputs are found to be vectors.
  if(all(are_vectors)) {
    
    # Check that all vectors have same length.
    lengths <- sapply(inputs, length)
    different <- min(lengths) != max(lengths)
    if(different) {
      new_msg <- paste("Vectors have differing lengths:", 
                       paste(names_inputs, collapse = ", "))
      msg <- c(msg, "\n", new_msg)
    }
    
    # Check that each vector has no NA.
    have_na <- sapply(inputs, function(x) sum(is.na(x) > 0))
    if(any(have_na)) {
      new_msg <- paste("Vector has missing values:", 
                       paste(names_inputs[have_na], collapse = ", "))
      msg <- c(msg, "\n", new_msg)
    }
    
  }
  
  # If there are errors, then stop and report
  if(length(msg) > 0) stop(msg)
  
  return(NULL)
  
}


# Print header for output. Basic information on Stan estimation.
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
  
  # If rhat < 1, adjust so that rhat = 1.
  bad_rhat <- summary_table[, Rhat] < 1
  # If rhat = NaN, then bad_rhat = NA. Set to bad_rhat = FALSE instead to leave 
  # output unchanged.
  bad_rhat[is.na(bad_rhat)] <- FALSE 
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
