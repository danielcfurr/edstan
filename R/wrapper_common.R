# Provide one ID variable. Returns a corrected ID vector and gives a warning
# if correction was needed.

match_id_stan <- function(x, warn=TRUE) {

  fixed_x <- unclass(as.factor(x))
  n_discrepancies <- sum(x != fixed_x)

  if(n_discrepancies > 0 & warn == TRUE) {
    warning(paste("ID vector", deparse(substitute(x)), 
                  "modified for stan input.",
                  "ID vector should be consecutive integers starting with 1.",
                  "Continuing with modified ID values.") )
  }

  output <- data.frame(old = as.character(x),
                       new = fixed_x,
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

# Function to print table of results for vectorized parameters. Used by
# print_stan.

print_vector_stan <- function(fit, pars, title="") {
  
  whole.table <- rstan::summary(fit, pars = pars)
  whole.table <- whole.table[["summary"]]
  
  output <- matrix(NA, nrow=length(pars), ncol=5)
  colnames(output) <- c("n", "min(n_eff)", "max(n_eff)", "min(Rhat)", "max(Rhat)")
  rownames(output) <- pars
  
  for(i in 1:length(pars)) {
    index <- grep(paste("^", pars[i], sep=""), rownames(whole.table))
    part.table <- whole.table[index, ]
    output[i,1] <- nrow(part.table)
    output[i,2] <- min(part.table[, "n_eff"])
    output[i,3] <- max(part.table[, "n_eff"])
    output[i,4] <- min(part.table[, "Rhat"])
    output[i,5] <- max(part.table[, "Rhat"])
  }
  
  #Prints in red like error message
  #message("Summary table for vector parameters.") 
  if(title != "") cat("\n", title, "\n")
  print(output, digits=c(0,0,0,3,3))
  
}


# Wrapper for printing Stan results.
# Provide names for rownames in output as named lists.

print_stan <- function(fit, 
                       pars,
                       names = NULL,
                       title = "",
                       round = 2) {

  whole.table <- rstan::summary(fit, pars = pars)
  whole.table <- whole.table[["summary"]]
  
  output <- NULL
  for(i in pars) {
    index <- grep(paste("^", i, sep=""), rownames(whole.table))
    part.table <- whole.table[index, ]
    name_vec <- names[[i]]
    if(!is.null(name_vec)) rownames(part.table) <- name_vec
    output <- rbind(output, part.table)
  }
  
  #output <- round(output, round)
  
  if(title != "") cat("\n", title, "\n")
  print(output)
  
}
