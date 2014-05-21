\name{mapCodes}
\alias{mapCodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	A function which maps values within the data set to new ones. }
\description{
The function goes through the codeMap and replaces codes in a
column-specific fashion  based on codeMap.}
\usage{
mapCodes(data, codeMap = codeMap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
		A data frame, this will be modified and returned.
}
  \item{codeMap}{
		A two-level list.  The first level names should be the column
		names of data to modify.  The second level names should be the
		standardized codes to modify to.  The values (leaf nodes) should
		be the values which are bein replaced by standardized codes.
}
}
\details{
}
\value{
	 A data frame, same as the \code{data} argument, but with modified
	 codes.}
\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>
}
