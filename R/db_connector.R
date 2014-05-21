db_linker <- setRefClass(Class="db_connector",
	fields = list(
		init = "logical",
		connection = "PostgreSQLConnection",
		driver = "PostgreSQLDriver",
		credentials = "character",
		conn = function(conn=NULL) {
			if (is.null(conn)) {
				if (!isPostgresqlIdCurrent(connection)) {
					crd <- readRDS(file=credentials)
					connection <<- dbConnect(drv=driver, port=crd[['port']], host=crd[['localhost']], 
						user = crd[['user']], password=crd[['password']], dbname=crd[['dbname']])
					rm(crd)
				}
				return(connection)
			} else if (!is.null(conn)) {
				connection <<- conn
				return(connection)
			} 
		}
	),
	methods = list(
		initialize = function(credentials) {
			driver <<- dbDriver("PostgreSQL")
			credentials <<- credentials
			crd <- readRDS(file=credentials)
	    connection <<- dbConnect(drv=driver, port=crd[['port']], host=crd[['host']], 
				user = crd[['user']], password=crd[['password']], dbname=crd[['dbname']])
	    rm(crd)
			init <<- TRUE
		}
	)
)
		

dbRobustWriteTable <- function(conn, user, password, host, dbname, name, value, tries) {
    numFullChunks <- nrow(value)%/%100
    lengthLastChunk <- nrow(value)%%100
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
            while (!dbWriteTable(conn = conn, name = name, value = value[rowSeq,], overwrite = FALSE, append = TRUE) & try < tries) {
                conn  <- dbConnect(MySQL(),user=user,password=password,host=host,dbname=dbname)
                try <- try + 1
                if (try == tries) { stop("EPIC FAIL") }
                print(paste("Fail number",try,"epical fail at",tries,"tries.",sep = " "))
            }
    }
}
