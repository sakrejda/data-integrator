## Naive, after reading a few web pages about how to do it... 
## I just need to sort a short list with a given comparator...
## Based on: 
##
##http://algs4.cs.princeton.edu/23quicksort/
##http://en.wikipedia.org/wiki/Quicksort
##http://rosettacode.org/wiki/Sorting_algorithms/Quicksort
## Thanks internet, that Pascal intro course was a long time ago...

quicksort <- function(x, cmp=`<=`) {
	A <- new.env()
	A[['x']] <- x

	swap <- function(i,j) {
		tmp <- A$x[i]
		A$x[i] <- A$x[j]
		A$x[j] <- tmp
	}

	qs <- function(i, k) {
		if (i < k) {
			p <- partition(i, k)
			if (p != 1) {
				qs(i, p-1)
			}
			if (p != length(A$x)) {
				qs(p+1, k)
			}
		}
	}

	partition <- function(l, r) {
		pivot_index <- choose_pivot(l, r)
		pivot_value <- A$x[pivot_index]
		swap(r,pivot_index)
		store_index <- l
		for ( i in l:(r-1)) {
			if (cmp(A$x[i],pivot_value)) {
				swap(i,store_index)
				store_index <- store_index + 1
			}
		}
		swap(r,store_index)
		return(store_index)
	}

	choose_pivot <- function(l, r) {
		return(l)
	}

	qs(1,length(A$x))
	return(A$x)
}

#o <- quicksort(rbinom(n=30, size=15, prob=0.8))

quicksort.list <- function(x, cmp=`<=`) {
	A <- new.env()
	A[['x']] <- x
	## Save names as attributes for x
	for ( i in 1:length(A[['x']])) {
		attr(x=A[['x']][[i]], which='name') <- names(x)[i]
	}

	swap <- function(i,j) {
		tmp <- A$x[[i]]
		tmp_attr <- attributes(A$x[[i]])
		A$x[[i]] <- A$x[[j]]
		attributes(A$x[[i]]) <- attributes(A$x[[j]])
		A$x[[j]] <- tmp
		attributes(A$x[[j]]) <- tmp_attr
	}

	qs <- function(i, k) {
		if (i < k) {
			p <- partition(i, k)
			if (p != 1) {
				qs(i, p-1)
			}
			if (p != length(A$x)) {
				qs(p+1, k)
			}
		}
	}

	partition <- function(l, r) {
		pivot_index <- choose_pivot(l, r)
		pivot_value <- A$x[[pivot_index]]
		swap(r,pivot_index)
		store_index <- l
		for ( i in l:(r-1)) {
			if (cmp(A$x[[i]],pivot_value)) {
				swap(i,store_index)
				store_index <- store_index + 1
			}
		}
		swap(r,store_index)
		return(store_index)
	}

	choose_pivot <- function(l, r) {
		return(l)
	}

	qs(1,length(A$x))
	## Recover names from attributes for x
	for ( i in 1:length(A[['x']])) {
		names(A$x)[i] <- attr(x=A[['x']][[i]], which='name')
	}
	return(A$x)
}

#o <- quicksort.list(as.list(rbinom(n=30, size=15, prob=0.8)))

cmp_function_dependence <- function(f, g) {
	nom_f <- attr(f,'name')
	args_f <- formalArgs(f)
	if (!is.null(args_f)) args_f <- sort(args_f)
	nom_g <- attr(g,'name')
	args_g <- formalArgs(g)
	if (!is.null(args_g)) args_g <- sort(args_g)
	if (is.null(args_f) && !is.null(args_g)) return(TRUE)
	if (is.null(args_g) && !is.null(args_f)) return(FALSE)
	if (is.null(args_f) &&  is.null(args_g)) return(TRUE)

	if (isTRUE(all.equal(args_f, args_g))) return(TRUE)
	if (nom_g %in% args_f) {
		return(FALSE)
	} else {
		return(TRUE)
	}
	stop("BAD THINGS.")
}

dependency_resolver <- function(f_list) {
	f_list <- quicksort.list(f_list, cmp=cmp_function_dependence)
	return(f_list)
}

#test_list <- list(
#	g = function(f, q) { f+q },
#	f = function(x) { x^2 },
#	h = function() { 10 }
#)
#
#sorted_test_list <- dependency_resolver(test_list)






