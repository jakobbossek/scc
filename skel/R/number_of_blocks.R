##' Returns number of blocks of a chart.
##' 
##' @param x [\code{\link{scc_chart}}]\cr
##'    An object of type \code{\link{scc_chart}}.
##'
##' @return [\code{integer(1)}]\cr
##'    Number of blocks.
##' @export
number_of_blocks = function(x) {
	UseMethod("number_of_blocks")
}

##' @S3method number_of_blocks scc_chart
number_of_blocks.scc_chart = function(x, ...) {
	length(x$blocks)
}