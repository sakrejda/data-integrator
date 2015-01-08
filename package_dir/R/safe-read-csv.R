#' @title Read a .csv file with minimal modifications as allowed by R,
#' then modify according to locally sourced files.
#' @param file A .csv type file.
#' @param instructions A .R file (source-able) which defines column name
#' mapping ("column_map"), NA mapping ("unknowns"), as well as value
#' mapping ("value_map", with "input", and "output" vectors.
#' @param drop_uppercase Turns column names to all lower-case.  
#' @return A data.frame with the .csv file loaded and modified as
#' specified.

safe_read_csv <- function( file, instructions=NULL, drop_uppercase=FALSE) {
	e <- new.env()
	if (!is.null(instructions)) {
		source(instructions, local=e, echo=FALSE)
	}
	data <- read.csv(file=file, check.names=FALSE, 
									 colClasses='character', stringsAsFactors=FALSE)

	if(drop_uppercase) colnames(data) <- tolower(colnames(data))

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

