\name{mapTypes}
\alias{mapTypes}
\title{
	A function to change the data types of a data frame.}
\description{
	The function takes the element names form \code{typeMap} as the
	columns to modify.  The values of those elements are the type to
	modify to.}
\usage{
mapTypes(data, typeMap)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
		A data frame.}
  \item{typeMap}{
		A list (or named vector) with the names being the columns to modify
	and the values are the data types to modify to.  Limited data types
	are supported.}
}
\details{
}
\value{
	A data frame, with modified data types for specified columns.}
\author{
	Krzysztof Sakrejda <krzysztof.sakrejda@gmail.com>}

