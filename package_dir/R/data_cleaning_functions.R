map <- function(x, map, one_to_one=TRUE) {
  # Check that arguments are of correct type.
  if (!is.list(map) || (is.null(names(map))) ) { 
	 stop("The name map must be a named list.") 
	}

  for(i in names(map)) {
    wx <- which(x %in% map[[i]])
		if (length(wx) >= 1) {
			x[wx[1]] <- i
		} 
		if (one_to_one && (length(wx) > 1))
			warning("Multiple matches, only first column mapped.")
	}
  return(x)
}


map_unknowns <- function(x, map) {
	x[x %in% map] <- NA
	return(x)
}

map_values <- function(x, input, output) {
	x[x %in% input] <- output[x %in% input]
	return(x)
}


