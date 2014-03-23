# ################################################################################
# Data cleaning functions
# ################################################################################


# This function maps the Westbrook Access database column names to the R-compatible names.
mapNames <- function(data, nameMap) {
   # Check that arguments are of correct type.
   if (!is.list(nameMap) | (is.null(names(nameMap))) ) { stop("The name map must be a named list.") }
   if (!is.data.frame(data)) stop("The argument 'data' must be a data frame.")

   # Get column names of the data frame
   cNames <- colnames(data)

   out <- for(i in names(nameMap)) {
         for(j in seq(along=nameMap[[i]])) {
            whichCol <- which(nameMap[[i]][j] == cNames)

            # Warn about duplicated column names if any.
            if (length(whichCol) > 1) warning("The data frame has duplicated column names.  These will all be replaced as per the name map, but you should check the output.  Problem for: ", nameMap[[i]][j])

            # Replace if any columns match:
            if (sum(whichCol) != 0) {
               colnames(data)[whichCol] <- i
            }
         }
      }
   return(data)
}

# End map names to standard names

toPOSIX <- function( df, cols ) {
    for ( col in cols ) {
        df[[col]] <- as.POSIXct(df[[col]])
    }
    return(df)
}
# Convert specified columnst to POSIXct

mapTypes <- function(data, typeMap) {
   for (col in names(typeMap)) {
      if (col %in% names(data)) {
         if (typeMap[[col]] == "factor") {
            data[[col]] <- as.factor(data[[col]])
         } else if   (typeMap[[col]] == "orderedFactor") {
            data[[col]] <- factor(x = data[[col]], levels = unique.default(x), ordered = TRUE)
         } else if   (typeMap[[col]] == "numeric") {
            data[[col]] <- as.numeric(data[[col]])
         } else if   (typeMap[[col]] == "character") {
            data[[col]] <- as.character(data[[col]])
         } else if   (typeMap[[col]] == "logical") {
            data[[col]] <- as.logical(data[[col]])
         } else if   (typeMap[[col]] == "POSIXct") {
            data[[col]] <- as.POSIXct(data[[col]])
         } else {
            stop("FAIL: Data type cannot be mapped by mapTypes function.  Fix typeMap argument.")
         }
      }
   }
   return(data)
}

# End map types to standard types.


# Map codes to standard codes:
mapCodes <- function(data, codeMap = codeMap) {
         for ( i in names(codeMap) ) {
            for ( j in names(codeMap[[i]]) ) {
               for ( k in codeMap[[i]][[j]] ) {
                  if ( !is.null(data[[i]][ data[[i]] == k ]) ) {
                    data[[i]][ data[[i]] == k ] <- j
                  }
               }
            }
         }
   return(data)
}
# End map codes to standard codes.



# Fix database 'NA' values for use in R.
fixNA <- function(data, naVals = NULL) {
   if (!is.list(naVals)) { stop("A list of values to relabel as 'NA' must be supplied.") }
   if (is.data.frame(data)) {
      for (i in 1:ncol(data)) {
         data[[i]][is.element(data[[i]], naVals)] <- NA
      }
      return(data)
   } else { stop("The supplied argument is not a data frame according to 'is.data.frame'.  Check with str(arg).") }
}




# This function can be used to write large data frames over an unreliable internet
# connection, currently it writes 100 rows at a time which may be sub-optimal on
# more reliable connections and unreliable on truly bad connections.  The number of
# rows to write per batch should become a parameter.


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

dbReconnect <- function(
  conn = NULL,
  idFile = NULL,
  drv = MySQL()
) {
    if (  is.null(idFile) ) {
        stop('Can\'t reconnect, no id.')
    }
    if ( !is.null(conn) ) {
      tryCatch(
        expr = dbDisconnect( conn = conn ),
        error = function(cond) {
          print(cond)
        }
      )
    }
    ev = new.env()
    load( file = idFile, envir = ev )
    ev$drv <- drv
    conn <- with(
        data = ev,
        expr = dbConnect(
            drv = drv,
            user = user,
            password = pass,
            dbname = dbname,
            host = host
        )
    ); rm(ev)
    return(conn)

}

