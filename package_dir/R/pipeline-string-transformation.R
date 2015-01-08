#' @title Runs a series of string-transforming functions.
#' @description Simpler than the generic pipeline transformation.
#' @param string A string to operate on.  
#' @param pipeline A list of functions to run on the strings.
#' @param string_args A vector of names of function arguments which
#' take the string for processing.  Typically just 'x', but sometimes
#' 'text', lets you use functions directly without rewriting thin
#' wrappers.
#' @param envir An environment where extra arguments can be found.
#' @return A string, transformed.  Better, faster, stronger!
#' @aliases pipeline_string_transformation


string_pipeline <- function(
	string=NULL, pipeline=NULL, string_args = rep('x',length(pipeline)), envir=parent.frame()
) {
	if(is.null(string)) stop("A string is required for a transformation.")
	if(is.null(pipeline)) return(string)
	pipeline_args <- lapply(X=pipeline, FUN=formalArgs)

	for ( i in seq_along(pipeline) ) {
		f <- pipeline[[i]]
		args <- list()
		for (arg in pipeline_args[[i]]) {
			if (arg == string_args[i]) {
				args[[arg]] <- string	
			} else {
				args[[arg]] <- get(x=arg, envir=envir)
			}
		}
		string <- do.call(what=f, args=args)
	}
	return(string)
}

pipeline_string_transformation <- string_pipeline

