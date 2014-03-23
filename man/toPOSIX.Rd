\name{toPOSIX}
\alias{toPOSIX}
\title{
	A function to modify the named columns to the POSIXct data type.}
\description{
	The function attempts to modify each named column to POSIXct.}
\usage{
toPOSIX(df, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
		A data frame.
}
  \item{cols}{
		A vector which names the columns to modify.}
}
\details{
}
\value{
	A data frame, with named columns modified to the POSIXct data type.
}
\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>}
