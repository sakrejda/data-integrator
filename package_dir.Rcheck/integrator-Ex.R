pkgname <- "integrator"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('integrator')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("dependency_resolver")
### * dependency_resolver

flush(stderr()); flush(stdout())

### Name: dependency_resolver
### Title: Dependency resolver for function lists.
### Aliases: dependency_resolver

### ** Examples

test_list <- list(
	g = function(f, q) { f+q },
	f = function(x) { x^2 },
	h = function() { 10 }
)

sorted_test_list <- dependency_resolver(test_list)



cleanEx()
nameEx("eval_pipe")
### * eval_pipe

flush(stderr()); flush(stdout())

### Name: eval_pipe
### Title: Runs expressions from a list in an independently scoped
###   environment.
### Aliases: eval_pipe

### ** Examples

data <- replicate(8,new.env())
data[[2]]$z <- pi
pipe=quote(list({a <- 3+3}, {b <- a*2}, {q <-a*b*z} ))
o <- eval_pipe(data,pipe)



cleanEx()
nameEx("map")
### * map

flush(stderr()); flush(stdout())

### Name: map
### Title: Maps names in a list (or data frame) to new names based on a
###   mapping from a list (map).
### Aliases: map

### ** Examples

n <- 10
widgets <- data.frame(id=1:n, weight_dry=exp(rnorm(n)),
                      weight_wet=exp(rnorm(n))+10,
                      vol=exp(rnorm(n)))
widgets <- map(x=widgets, map=list(volume="vol"))



cleanEx()
nameEx("quicksort")
### * quicksort

flush(stderr()); flush(stdout())

### Name: quicksort
### Title: Sorting with an arbitrarily defined comparator ('<=' by
###   default).
### Aliases: quicksort quicksort.list

### ** Examples

o <- quicksort(rbinom(n=30, size=15, prob=0.8))



cleanEx()
nameEx("quicksort.list")
### * quicksort.list

flush(stderr()); flush(stdout())

### Name: quicksort.list
### Title: Sorting with an arbitrarily defined comparator ('<=' by
###   default).
### Aliases: quicksort quicksort.list

### ** Examples

o <- quicksort.list(as.list(rbinom(n=30, size=15, prob=0.8)))



cleanEx()
nameEx("string_masks")
### * string_masks

flush(stderr()); flush(stdout())

### Name: string_masks
### Title: Takes strings, check if they match patterns, and construct
###   expressions according to a string.
### Aliases: string_masks

### ** Examples

n <- 10
widgets <- data.frame(id=1:n, weight_dry=exp(rnorm(n)),
                      weight_wet=exp(rnorm(n))+10, volume=exp(rnorm(n)))
patterns <- list(weights="^weight")
expr <- quote(list(measurement = weights | volume))
masks <- string_masks(x=names(widgets), patterns=patterns, expr=expr)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
