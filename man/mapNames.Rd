\name{mapNames}
\alias{mapNames}
\title{
	A function to map column names to new (standardized names).}
\description{
	The function iterates through the \code{nameMap} list and changes
	any column names in data which match the values of a \code{nameMap}
	element to the name of that \code{nameMap} element.}
\usage{
mapNames(data, nameMap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
		A data frame.}
  \item{nameMap}{
		A list.  Top-level names are the final (standardized) names, and
		their values are the names being mapped from.}
}
\details{
}
\value{
	A data frame, same as the \code{data} argument.}
\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>
}
