#' A reference class which always yields a valid connection with x$conn.
#'
#' @field driver A driver from RPostgreSQL.
#' @field credentials A filename for connection info. to the database
#'        (including host and port).
#' @field conn The always-valid connection.

db_connector <- setRefClass(Class="db_connector",
	fields = list(
		driver = "PostgreSQLDriver",
		credentials = "character",
		conn = function(conn=NULL) {
			if (!is.null(conn)) return("no.")
			return(get_connection())
		}
	),
	methods = list(
		initialize = function(credentials) {
			"Checks for RPostgreSQL, uses credentials to generate a connection and test it."
			if (!require(RPostgreSQL)) stop("RPostgreSQL required.")
			driver <<- dbDriver("PostgreSQL")
			credentials <<- credentials
			add_connection()
			check_crd()
			connection_in_use(get_connection())
			clean_connections()
			clear_connections()
		},
		check_crd = function(crd=NULL) {
			"Checks whether credentials are available and complete."
			if (is.null(crd)) {
				crd <- readRDS(.self$credentials)
			} else {
				crd <- readRDS(crd)
			}
			required_elements <- c('port','host','user','password','dbname')
			missing <- sapply(required_elements, function(x,crd) !(x %in% crd), crd=names(crd) )
			if (any(missing)) {
				paste0('Credentials list must include elements for:\n ',
					paste('\t',required_elements[missing],sep='',collapse=',\n'))
			}
			rm(crd)
			return("Credentials ok.\n")
		},
		add_connection = function() {
			"Adds a connection to the pool if necessary."
			crd <- readRDS(file=credentials)
			status <- tryCatch(expr = {
				dbConnect(drv=driver, port=crd[['port']], host=crd[['host']], 
					user = crd[['user']], password=crd[['password']], dbname=crd[['dbname']])
			}, error=function(e) return("FAIL"))
			rm(crd)
			if (identical(status, "FAIL")) {
				dbListTables(dbListConnections(driver)[[length(dbListConnections(driver))]])
				warning(paste0(
					"Was not able to allocate a new connection, returned\n",
				 	"connection has lost its previously pending result."))
			}
			connections <- dbListConnections(driver)
			return(connections[[length(connections)]])
		},
		get_connection=function() {
			"Yields an unused connection from the pool."
			if (identical(list(),dbListConnections(driver)))
				add_connection()
			for ( i in 1:length(dbListConnections(driver)) ) {
				connection <- dbListConnections(driver)[[i]]
				if (!connection_in_use(connection)) {
					return(connection)
				} 
			}
			add_connection()
			return(dbListConnections(driver)[[length(dbListConnections(driver))]])
		},
		connection_in_use = function(connection=NULL) {
			"Tests whether a connection is in use."
			if (is.null(connection)) return(NA)
			temp_result <- tryCatch(
				expr=dbSendQuery(connection,"SELECT * FROM generate_series(1,2);"),
				error=function(e) return("FAIL")
			)
			test <- (identical(temp_result,"FAIL")) 
			if (!test) dbClearResult(temp_result)
			return(test)
		},
		clean_connections = function() {
			"Cleans out available connections."
			for (i in 1:length(dbListConnections(driver))) {
				if (!isPostgresqlIdCurrent(dbListConnections(driver)[[i]])) {
					dbDisconnect(dbListConnections(driver)[[i]])
				}
			}
		},
		clear_connections = function() {
			"Runs a simple command to clear pending results on available connections."
			lapply(dbListConnections(driver),dbListTables)
			return(TRUE)
		}
	)
)
		

dbRobustWriteTable <- function(credentials) {
    numFullChunks <- nrow(value)%/%100
    lengthLastChunk <- nrow(value)%%100
		cnct <- db_connector(credentials)
    if (numFullChunks >= 1) {
        writeSeqFullChunks <- data.frame(Start = seq(0,numFullChunks-1,1)*100+1, Stop = seq(1,numFullChunks,1)*100)
    }
    writeSeqLastChunk <- data.frame(Start = numFullChunks*100+1, Stop = numFullChunks*100+lengthLastChunk)
    if (numFullChunks >= 1) {
        writeSeqAllChunks <- rbind(writeSeqFullChunks,writeSeqLastChunk)
    } else { writeSeqAllChunks <- writeSeqLastChunk }

    for(i in 1:nrow(writeSeqAllChunks)) {
            try <- 0
            rowSeq <- seq(writeSeqAllChunks$Start[i],writeSeqAllChunks$Stop[i],1)
            while (!dbWriteTable(conn = cnct$conn, name = name, value = value[rowSeq,], overwrite = FALSE, append = TRUE) & try < tries) {
                try <- try + 1
                if (try == tries) { stop("EPIC FAIL") }
                print(paste("Fail number",try,"epical fail at",tries,"tries.",sep = " "))
            }
    }
}
