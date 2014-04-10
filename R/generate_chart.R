#' Generates control chart object.
#'
#' @param blocks [\code{list}]\cr
#'   List of blocks from which quantity is computed.
#' @param type [\code{character(1)}]\cr
#'   Type of the control chart as a string. Currently only x-chart
#'   is supported.
#' @param parameters [\code{list}]\cr
#'   List of parameters needed to compute control respectively warning
#'   bounds.
#' @param bounds [\code{list}]\cr
#'   List which contains the fields upper control bound (UCB), lower control bound
#'   (LCB), upper warning bound (UWB) and lower warning bound (LWB). At least the
#'   two first of those must be provided.
#' @param description [\code{character(1)}]\cr
#'   Description of the measured quantity. Used as subtitle in graphical representation.
#' @param y_value [\code{numeric}]\cr
#'   Values of the measured characteristic. Typically a single numeric value or a n sequence,
#'   where n is the number of blocks.
#' @param y_value_name [\code{character(1)}]\cr
#'   Name for the measured characteristic. Used for example for the y axis label in the plot
#'   by default.
#' @param desired_value [\code{numeric}]\cr
#'   Reference value of the measured quantity, i.e., the average value. May be a single
#'   value or comply with the number of blocks (see blocks parameter).
#' @param desired_value_name [\code{character(1)}]\cr
#'   Label of the desired value. Used in the graphical representation.
#'
#' @return
#'   Object of type \code{scc_chart}.
#'
#' @export
generate_chart = function(blocks, type,
	parameters, bounds,
	description,
	y_value, y_value_name="measured quantity",
	desired_value, desired_value_name) {
	#FIXME: check arguments for validity
	blocks = if(is.data.frame(blocks)) dataframe_to_list(blocks, byrow=FALSE) else blocks
	structure(list(
		blocks = blocks,
		type = type,
		parameters = parameters,
		bounds = bounds,
		y_value = y_value,
		y_value_name = y_value_name,
		description = description,
		desired_value = desired_value,
		desired_value_name = desired_value_name),
		class = "scc_chart"
	)
}

is_scc_chart = function(x) {
	inherits(x, "scc_chart")
}
