#' Determines the number of outliers, i.e., values out of town.
#'
#' @param x [\code{scc_chart}]\cr
#'   An object of type \code{scc_chart}.
#' @param ... [\code{list}]\cr
#'   Not used.
#'
#' @return
#'   Data frame containing the number of y-values below LCB and UCB.
#'
#' @export
number_of_outliers = function(x, ...) {
	UseMethod("number_of_outliers")
}

#' @S3method number_of_outliers scc_chart
number_of_outliers.scc_chart = function(x, ...) {
	n_blocks = number_of_blocks(x)
	bounds = x$bounds

	# FIXME: check <= or <
	below_LCB = (x$y_value <= bounds$LCB)
	above_UCB = (x$y_value >= bounds$UCB)

	below_LWB = (x$y_value <= bounds$LWB)
	above_UWB = (x$y_value >= bounds$UWB)
	# FIXME: what about observations which lie between warning and control bounds
	df = data.frame(
		"belowLCB" = length(which(below_LCB)),
		"belowLWB" = length(which(below_LWB)),
		"aboveUWB" = length(which(above_UWB)),
		"aboveUCB" = length(which(above_UCB)))
	df
}
