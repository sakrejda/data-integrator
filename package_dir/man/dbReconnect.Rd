\name{dbReconnect}
\alias{dbReconnect}
\title{
A function to reuse a connection object
}
\description{
dbReconnect allows you to reuse a connection object transparently.  If
the connection has expired, a new one is returned, otherwise it is
reused.
}
\usage{
dbReconnect(conn = NULL, idFile = NULL, drv = MySQL())
}
\arguments{
  \item{conn}{
		A connection returned from a previous call to
		\code{dbConnect}/\code{dbReconnect}
}
  \item{idFile}{
		A file which, when loaded, provides the variables 'user', 'pass',
		'dbname', and 'host'.  This is important for automated scripts to
		avoid placing these things in the script.
}
  \item{drv}{A function which returns a driver object for use in
		\code{dbConnect}.
}
}
\details{
	This function is useful when multiple scripts need to access the same
	database.  Rather than assuming each script cleans up succesfully, you 
	can always use this function to connect and avoid running out of
	connections.
}
\value{
	A connection object useful for interacting with a database.
}

\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>
}

