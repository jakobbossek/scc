#' Determines the number of outliers, i.e., values out of town.
#'
#' @param x [\code{scc_chart}]\cr
#'   Chart.
#' @param ... [any]\cr
#'   Not used.
#' @return
#'   Data frame containing the number of y-values below LCB and UCB.
#' @export
getNumberOfOutliers = function(x, ...) {
	UseMethod("getNumberOfOutliers")
}

#' @export
getNumberOfOutliers.scc_chart = function(x, ...) {
	n_blocks = getNumberOfBlocks(x)
	bounds = x$bounds

	#FIXME: check <= or <
	below_LCB = (x$y_value <= bounds$LCB)
	above_UCB = (x$y_value >= bounds$UCB)

	below_LWB = (x$y_value <= bounds$LWB)
	above_UWB = (x$y_value >= bounds$UWB)

	df = data.frame(
		"belowLCB" = sum(below_LCB),
		"belowLWB" = sum(below_LWB),
		"aboveUWB" = sum(above_UWB),
		"aboveUCB" = sum(above_UCB)
  )
	return(df)
}
