pipeline_data_transformation <- function(
	data=NULL, pipeline=NULL, envir=parent.frame(), final_names=NULL,
	multipath=FALSE   ## Messy option, but allows you to skip
										## transformations which lack their main
										## argument in the data.
) {
	if(is.null(data)) stop("Data is required for a transformation.")
	if(is.null(pipeline)) return(data)
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

	source_names <- vector(mode='character', length=0)
	for ( i in seq_along(pipeline) ) {
		o_name <- target_names[i] 
		if (is.function(pipeline[[i]])) {
			f <- pipeline[[ i	]]
			f_args <- formalArgs(f)
			i_name <- f_args[1]
		} else {
			for ( j in seq_along(pipeline[[i]])) {
				f <- pipeline[[i]][[j]]
				f_args <- formalArgs(f)
				i_name <- f_args[1]
				if (exists(x=i_name, where=source_env)) break
			}
		}
		if (multipath && !exists(x=i_name, where=source_env)) next	

		if (!multipath && !exists(x=i_name, where=source_env)) {
			msg <- paste0("Name '", i_name, "' is not a column in data.\n")
			stop(msg)
		}
		if (!is.function(f)) stop("'f' is not a functio.\n")
		if (is.null(formalArgs(f))) {
			msg <- paste0("'f' must have at least one formal argument.\n")
			stop(msg)
		}

		source_names <- c(source_names,i_name)

		args <- list()
		for (arg in f_args) {
			args[[arg]] <- get(x=arg, envir=source_env, inherits=TRUE)
		}
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



