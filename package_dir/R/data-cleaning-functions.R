#' Maps names in a list (or data frame) to new names based on a mapping
#' from a list (map).
#'
#' @param x A list (or data frame) with names to be mapped.
#' @param map A named list of character vectors.  In re-mapping list names are the new
#'        target columns and source columns are named in the vectors.
#' @param one_to_one A flag which (if TRUE) triggers a warning when
#'        multiple columns might be mapped (only the first is).
#' @return The original list (or data frame) modified.
#' @examples
#' n <- 10
#' widgets <- data.frame(id=1:n, weight_dry=exp(rnorm(n)),
#'                       weight_wet=exp(rnorm(n))+10,
#'                       vol=exp(rnorm(n)))
#' widgets <- map(x=widgets, map=list(volume="vol"))

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

#' Maps special "NA" values to NA's in a vector.
#' @param x A vector with special values.
#' @param map A vector of special values to be mapped to NA.
#' @return x, with special values turned to NA.
map_unknowns <- function(x, map) {
	x[x %in% map] <- NA
	return(x)
}

#' Maps vectorvalues one-to-one.
#' @param x A vector to operate on.
#' @param input A vector of values to be modified.
#' @param output A vector of values to replace input values.
#' @param x, with modifications.
map_values <- function(x, input, output) {
	x[x %in% input] <- output[x %in% input]
	return(x)
}


