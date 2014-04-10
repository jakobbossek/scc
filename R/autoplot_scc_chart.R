#' Plot a SCC chart with ggplot2.
#'
#' @param object [\code{scc_chart}]\cr
#'   An object of type \code{scc_chart}.
#' @param xlab [\code{character(1)}]\cr
#'   A title for the x axis.
#' @param ylab [\code{character(1)}]\cr
#'   A title for the y axis.
#' @param title [\code{character(1)}]\cr
#'   A title for the plot. See main parameter in plot function.
#' @param show_histogram [\code{character(1)}]\cr
#'   Should a histogram of the y-value be depicted beside the chart?
#'   Default ist \code{FALSE}.
#' @param ... [\code{list}]\cr
#'   Not used.
#'
#' @return
#'   A \code{\link[ggplot2]{ggplot2}} object.
#'
#' @export autoplot.scc_chart
#' @method autoplot scc_chart
autoplot.scc_chart = function(object,
	xlab="block", ylab=object$y_value_name, title=object$description,
	show_histogram=FALSE, ...) {
    #FIXME: add flag to let the user set if the longest outlier sequence shall be highlighted

  	# extract relavant data
  	desired_value = object$desired_value
  	n_blocks = number_of_blocks(object)

  	# base chart plot
  	df = data.frame(block=1:n_blocks, y=object$y_value)
  	pl = ggplot(df, aes_string(x = "block", y = "y"))
  	pl = pl + geom_line(colour = "gray")
  	pl = pl + geom_point(shape = 1, size = 3)

  	# check whether warning and control bounds are constant
  	if (length(desired_value) == 1) {
    	pl = pl + geom_hline(linetype = "solid", colour = "grey", yintercept = as.numeric(desired_value))
  	} else {
   		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, y = desired_value), aes_string(x = "block", y = "y"))
  	}

  	if (length(object$bounds$UCB) == 1) {
    	pl = pl + geom_hline(linetype = "dashed", colour = "black", yintercept = as.numeric(c(object$bounds$LCB, object$bounds$UCB)))
  	} else {
  		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, lower = object$bounds$LCB), aes_string(x = "block", y = "lower"), linetype = "dashed", colour = "black")
    	pl = pl + geom_line(data = data.frame(block = 1:n_blocks, upper = object$bounds$UCB), aes_string(x = "block", y = "upper"), linetype = "dashed", colour = "black")
  	}

  	if ("LWB" %in% names(object$bounds) && "UWB" %in% names(object$bounds)) {
		if (length(object$bounds$UWB) == 1) {
			pl = pl + geom_hline(linetype = "dashed", colour = "black", yintercept = as.numeric(c(object$bounds$LWB, object$bounds$UWB)))
	  	} else {
	  		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, lower = object$bounds$LWB), aes_string(x = "block", y = "lower"), linetype = "dashed", colour = "darkgrey")
	    	pl = pl + geom_line(data = data.frame(block = 1:n_blocks, upper = object$bounds$UWB), aes_string(x = "block", y = "upper"), linetype = "dashed", colour = "darkgrey")
	  	}
  	}

  	# add axis labels
  	pl = pl + scale_x_continuous(xlab)
  	pl = pl + scale_y_continuous(ylab)
  	pl = pl + ggtitle(title)

  	# additional absolute frequency histogram with kernel density estimation
  	# FIXME: think about this. Really neccessary? grid.arrange does not return ggplot object!!!
    if (show_histogram) {
  		pl_hist = ggplot(df, aes_string(x = "y")) +
  		geom_histogram(aes_string(y = "..density.."), colour = "darkgreen", alpha = 0.3, fill = "green", bin.width = 1) +
  		geom_density(alpha = 0.4, fill = "white") +
  		scale_y_continuous("dens") +
  		theme(axis.ticks = element_blank(),
         	axis.title.y = element_blank(),
         	axis.text.y =  element_blank()) +
  		coord_flip()

	  	pl = grid.arrange(pl, pl_hist, widths=c(5/6,1/6), ncol=2)
  	}
  	return(pl)
}
