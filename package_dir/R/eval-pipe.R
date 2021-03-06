
#' Runs expressions from a list in an independently scoped environment.
#' @param data A list of environments (or data frames, or a data frame
#'        which is promoted to an environment.  This creates a scoped
#'        context for evaluation expressions.  This scope \emph{skips}
#'        .GlobalEnv.
#' @param pipeline A quoted list of expressions which are evaluted in
#'        scoped context provided by the data.
#' @return An internally generated list of expression results.
#' @examples
#' data <- replicate(8,new.env())
#' data[[2]]$z <- pi
#' pipe=quote(list({a <- 3+3}, {b <- a*2}, {q <-a*b*z} ))
#' o <- eval_pipe(data,pipe) 

eval_pipe <- function(
	data = NULL, # List of environments, or an environment (which gets immediately promoted.
	pipeline = list(),
  debug=FALSE,
  simplify=TRUE
) {
	if(is.null(data)) stop("Data is required for a transformation.")
	if(is.environment(data)) data <- list(environment)
	if(is.list(data)) {
		for ( i in seq_along(data)) {
			if (!is.environment(data[[i]])) 
				data[[i]] <- as.environment(data[[i]])
		}
	}
	if(is.null(pipeline)) return(data)

	n_expr <- length(pipeline)-1
	n_data <- length(data)
	pipeline <- pipeline[2:(n_expr+1)]
	output <- replicate(n_expr,new.env())
	data <- c(data,output)

	store_parents <- lapply(data,parent.env)
	skipper <- new.env()
	parent.env(skipper) <- parent.env(.GlobalEnv)


	for (i in seq_along(data)) {
		if (i == 1) 
			parent.env(data[[i]]) <- skipper
		else
			parent.env(data[[i]]) <- data[[i-1]]
	}

	for ( i in seq_along(pipeline)) {
    if (!debug) {
		  try(eval(expr=pipeline[[i]], envir=data[[n_data+i]]))
    } else {
	    eval(expr=pipeline[[i]], envir=data[[n_data+i]])
    } 
	}

	## Restore original parents...
	for (i in 1:n_data) {
		parent.env(data[[i]]) <- store_parents[[i]]
	} 
	for ( i in (n_data+1):(n_data+n_expr) ) {
		parent.env(data[[i]]) <- parent.frame()
	}
  if (simplify) {
  	return(lapply(output,as.list))
  } else {
    return(data)
  }
}







