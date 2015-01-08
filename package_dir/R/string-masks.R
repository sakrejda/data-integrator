#' Takes strings, check if they match patterns, and construct expressions according to a string.
#' 
#' @param x A vector of strings which masks will be created for.
#' @param patterns A list of patterns (for grepl) which the strings (might) match.
#' @param expr A quoted list of logical expressions operating on pattern-derived 
#'        masks which will further combine the masks
#'
#' @return A list (of length \code{length(patterns)+length(expr)}) of
#'         vectors (each of length \code{length(x)}).
#' @examples
#' n <- 10
#' widgets <- data.frame(id=1:n, weight_dry=exp(rnorm(n)),
#'                       weight_wet=exp(rnorm(n))+10, volume=exp(rnorm(n)))
#' patterns <- list(weights="^weight")
#' expr <- quote(list(measurement = weights | volume))
#' masks <- string_masks(x=names(widgets), patterns=patterns, expr=expr)
string_masks <- function(x, patterns, expr=NULL, all_strings=TRUE) {
	if (all_strings) {
		more_patterns <- as.list(x)
		names(more_patterns) = more_patterns
		patterns <- c(more_patterns, patterns)
	}
  masks <- mapply(FUN=grepl, pattern=patterns, MoreArgs=list(x=x), SIMPLIFY=FALSE)
  names(masks) <- names(patterns)
  masks <- list2env(masks,parent=.GlobalEnv)
	if (!is.null(expr)) {
	  new_masks <- eval(expr,masks)
  	masks <- c(new_masks, as.list(masks))
	}
  return(masks)
}





