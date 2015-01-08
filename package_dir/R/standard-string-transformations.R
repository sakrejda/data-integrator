
#' Standard strings transformations.
standard_string_transformations <- list(
	to_lower_case = tolower,
	drop_leading_whitespace = function(x) gsub(x=x, pattern='\\A[ ]*', replacement='', perl=TRUE),
	drop_trailing_whitespace = function(x) gsub(x=x, pattern='[ ]*\\z', replacement='', perl=TRUE),
	whitespace_to_underscore = function(x) gsub(x=x, pattern='\\s+', replacement='_', perl=TRUE)
)





