pipeline_data_transformation <- function(
	data=NULL, pipeline=NULL, envir=parent.frame(), final_names=NULL
) {
	if(is.null(data)) stop("Data is required for a transformation.")
	if(is.null(pipeline)) return(data)
	pipeline_args <- lapply(X=pipeline, FUN=formalArgs)
	source_names <- sapply(pipeline_args, `[[`, 1)
	target_names <- names(pipeline)
	if(is.null(final_names)) final_names <- target_names
	if(length(final_names) == 1 && final_names == 'all') 
		final_names <- unique(c(source_names, target_names, names(data)))

	source_env <- new.env(parent=envir)
	for (nom in names(data)) {
		assign(x=nom, value=data[[nom]], envir=source_env)
	}

	for ( i in seq_along(pipeline) ) {
		f <- pipeline[[ target_names[i]	]]
		args <- list()
		for (arg in pipeline_args[[i]]) {
			assign(x='arg', value=arg, envir=source_env)
			args[[arg]] <- with(data=source_env, expr=get(x=arg))
		}
		assign(x=target_names[i], value=do.call(what=f, args=args), envir=source_env)
	}

	for (nom in final_names) {
		data[[nom]] <- get(x=nom, envir=source_env)
	}
	return(data[final_names])
}



