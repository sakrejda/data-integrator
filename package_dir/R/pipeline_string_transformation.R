pipeline_string_transformation <- function(
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



