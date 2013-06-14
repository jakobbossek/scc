##' Generates an exponentially weighted moving average control chart.
##'
##' @param blocks [\code{list}]\cr
##'    List of blocks from which quantity is computed.
##' @param center [\code{numeric(1)}]\cr
##'    Long term process mean.
##' @param sd [\code{numeric(1)}]\cr
##'    Standard deviation of the process.
##' @param parameters [\code{list}]\cr
##'    List of parameters needed to compute control respectively warning
##'    bounds.
##' @param description [\code{character(1)}]\cr
##'    Description of the measured quantity. Used as subtitle in graphical representation.
##' @param y_value [\code{numeric}]\cr
##'    Values of the measured characteristic. Typically a single numeric value or a n sequence,
##'    where n is the number of blocks.
##' @param y_value_name [\code{character(1)}]\cr
##'    Name for the measured characteristic. Used for example for the y axis label in the plot
##'    by default.
##' @param desired_value [\code{numeric}]\cr
##'    Reference value of the measured quantity, i.e., the average value. May be a single
##'    value or comply with the number of blocks (see blocks parameter).
##' @param desired_value_name [\code{character(1)}]\cr
##'    Label of the desired value. Used in the graphical representation.
##'
##' @return Object of class \code{\link{scc_chart}} and \code{\link{ewma_chart}}.
##'
##' @export
generate_ewma_chart = function(blocks,
	center,
	sd,
	parameters=list(lambda=0.5),
	description,
	y_value_name="measured quantity",
	desired_value, desired_value_name) {

	# build up parameter set 
	not_privided_params = list(lambda=0.5, omega=1.96, k=3)
	not_provided_param_names = setdiff(names(not_privided_params), names(parameters))
	parameters = c(parameters,not_privided_params[not_provided_param_names])
	print(parameters)

	stopifnot(parameters$lambda > 0 & parameters$lambda < 1)
	# compute bounds
	n_blocks = length(blocks)
	bounds = generate_ewma_bounds(n_blocks, center, sd, parameters)

	block_means = as.numeric(lapply(blocks, mean))
	y_value = numeric(n_blocks)
	lambda = parameters$lambda
	y_value[1] = center
	for (i in 2:n_blocks) {
		y_value[i] = lambda * block_means[i] + (1-lambda) * y_value[i-1]
	}

	# generate scc chart object
	generate_chart(
		blocks=blocks,
		type="ewma",
		parameters=parameters,
		bounds=bounds,
		description=description,
		y_value=y_value,
		y_value_name=y_value_name,
		desired_value=center,
		desired_value_name=desired_value_name
		)

}

generate_ewma_bounds = function(n_blocks, center, sd, parameters) {
	k = parameters$k
	omega = parameters$omega
	lambda = parameters$lambda
	LCB = numeric(n_blocks)
	UCB = numeric(n_blocks)
	LWB = numeric(n_blocks)
	UWB = numeric(n_blocks)
	for (i in 1:n_blocks) {
		sqrt_term = sqrt((lambda/(n_blocks*(2-lambda)))*(1-(1-lambda)^(2*i)))
		LCB[i] = center - k * sd * sqrt_term
		UCB[i] = center + k * sd * sqrt_term
		LWB[i] = center - omega * sd * sqrt_term
		UWB[i] = center + omega * sd * sqrt_term
			
	}
	return(list(LCB=LCB, UCB=UCB,LWB=LWB,UWB=UWB))
}

#attr("generate_ewma_chart", "class") = "ewma_chart"