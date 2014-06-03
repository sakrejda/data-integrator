pipeline_data_transformation <- function(
	data=NULL, pipeline=NULL, envir=parent.frame(), final_names=NULL,
	multipath=FALSE   ## Messy option, but allows you to skip
										## transformations which lack their main
										## argument in the data.
) {
	if(is.null(data)) stop("Data is required for a transformation.")
	if(is.null(pipeline)) return(data)
	pipeline_args <- lapply(X=pipeline, FUN=formalArgs)
	source_names <- sapply(pipeline_args, `[[`, 1)
	target_names <- names(pipeline)
	if(is.null(final_names)) final_names <- target_names
	if(length(final_names) == 1 && final_names == 'all')
		keep_all_names <- TRUE
	else 
		keep_all_names <- FALSE
	

	source_env <- new.env(parent=envir)
	for (nom in names(data)) {
		assign(x=nom, value=data[[nom]], envir=source_env)
	}

	for ( i in seq_along(pipeline) ) {
		f <- pipeline[[ i	]]
		args <- list()
		for (arg in pipeline_args[[i]]) {
			if (multipath && (arg == source_names[i]) && !exists(x=arg, envir=source_env))
				next;
#			assign(x='arg', value=arg, envir=source_env)
#			args[[arg]] <- with(data=source_env, expr=get(x=arg))
			args[[arg]] <- get(x=arg, envir=source_env, inherits=TRUE)
		}
		if (multipath && !(source_names[i] %in% names(args)) ) 
			next;
		assign(x=target_names[i], value=do.call(what=f, args=args), envir=source_env)
	}

	if (keep_all_names) {
		final_names <- unique(c(source_names, target_names, names(data)))
	}

	final_names <- final_names[ sapply(X=final_names,FUN=exists,envir=source_env) ]
	for (nom in final_names) {
		data[[nom]] <- get(x=nom, envir=source_env)
	}

	return(data[final_names])
}



