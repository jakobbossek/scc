#' Returns number of blocks of a chart.
#'
#' @param x [\code{scc_chart}]\cr
#'   An object of type \code{scc_chart}.
#'
#' @return [\code{integer(1)}]
#'    Number of blocks.
#' @export
number_of_blocks = function(x) {
	UseMethod("number_of_blocks")
}

#' @export
number_of_blocks.scc_chart = function(x, ...) {
	length(x$blocks)
}
