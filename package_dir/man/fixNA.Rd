\name{fixNA}
\alias{fixNA}
\title{
	A function to turn NA codes (e.g.- -999 in a numeric data set) to R's
	\code{NA} values.
}
\description{
	The function searches each column of data and replaces all occurrences
	of each NA code to the relevant R \code{NA} value.
}
\usage{
fixNA(data, naVals = NULL)
}
\arguments{
  \item{data}{
		A data frame}
  \item{naVals}{
		A list of codes which should be replaced by \code{NA}.}
}
\details{
}
\value{
	Data in the same format as it was input, but with NA codes replaced by
R \code{NA} values.}
\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>}
