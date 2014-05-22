safe_read_csv <- function( file, instructions=NULL) {
	e <- new.env()
	if (!is.null(instructions)) {
		source(instructions, local=e, echo=FALSE)
	}
	data <- read.csv(file=file, check.names=FALSE, 
									 colClasses='character', stringsAsFactors=FALSE)
	
	if(exists(x="column_map", where=e, inherits=FALSE)) {
		names(data) <- map(x=names(data), map=as.list(e[['column_map']]))
	}

	for (nom in names(data)) {
		if (exists(x='unknowns', where=e, inherits=FALSE)) {
			data[[nom]] <- map_unknowns(data[[nom]], e[['unknowns']])
		}

		if(exists(x='value_map', where=e, inherits=FALSE)) {
			data[[nom]] <- map_values(data[[nom]],e[['value_map']][['input']],
																e[['value_map']][['output']])
		}
	}
	return(data)
}

