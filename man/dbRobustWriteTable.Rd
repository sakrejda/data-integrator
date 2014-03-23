\name{dbRobustWriteTable}
\alias{dbRobustWriteTable}
\title{
A function to write a table to a database over an unreliable connection.
}
\description{
\code{dbRobustWriteTable} writes the data value to the database in 100
row chunks, reconnecting whenever a chunk fails.}
\usage{
dbRobustWriteTable(conn, user, password, host, dbname, name, value, tries)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
		Connection object for the database.
}
  \item{user}{
		Username for the databse.
}
  \item{password}{
		Password for the database.
}
  \item{host}{
		Hostname for the database.
}
  \item{dbname}{
		Database name, for reconnecting.
}
  \item{name}{
	Table name for reconnecting.
}
  \item{value}{
	Value to write to the database.
}
  \item{tries}{
	Number of times to try writing each 100 row chunk before giving up.
}
}
\details{
	This function is useful for writing large tables over unreliable
	connections (e.g.- a poor internet connection, or even a very large
	table over a reasonable connection).  It writes in 100 row chunks
	which may not be optimal but is not yet adjustable.
}
\value{
	\code{TRUE} if successful and \code{FALSE} otherwise.
}
\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>
}
