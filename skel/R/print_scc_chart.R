##' Print a SCC chart.
##' 
##' @param x [\code{\link{scc_chart}}]\cr
##'    A \code{\link{scc_chart}} object.
##' @param ... \cr
##'    Not used.
##'
##' @return Nothing. As a side-effect a textual information is printed to the console
##'
##' @S3method print scc_chart
##' @method print scc_chart
print.scc_chart = function(x, ...) {
	cat("Sheward Control Chart of type", toupper(x$type), "\n")
	cat("Number of blocks     :", number_of_blocks(x))
	l1 = length(x$blocks[[1]])
	if (all(lapply(x$blocks, length) == l1)) {
		cat(" (all blocks have equal size)")
	} else {
		cat(" (blocks differ in size)")
	}
	cat("\n")
	#FIXME: add output of bounds and 'table' of violated bounds
}