#' Returns number of blocks of a chart.
#'
#' @param x [\code{scc_chart}]\cr
#'   An object of type \code{scc_chart}.
#'
#' @return [\code{integer(1)}]
#'    Number of blocks.
#' @export
getNumberOfBlocks = function(x) {
	UseMethod("getNumberOfBlocks")
}

#' @export
getNumberOfBlocks.scc_chart = function(x, ...) {
	length(x$blocks)
}
