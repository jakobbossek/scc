#' Print a SCC chart.
#'
#' @param x [\code{scc_chart}]\cr
#'   A chart.
#' @param ... [any]\cr
#'   Not used.
#' @return
#'   Nothing. As a side-effect a textual information is printed to the console.
#' @export
print.scc_chart = function(x, ...) {
	catf("Sheward Control Chart of type", toupper(x$type), "\n")
	catf("Number of blocks: ", getNumberOfBlocks(x))
	l1 = length(x$blocks[[1]])
	if (all(lapply(x$blocks, length) == l1)) {
		cat(" (all blocks have equal size)")
	} else {
		cat(" (blocks differ in size)")
	}
	cat("\n")
}
