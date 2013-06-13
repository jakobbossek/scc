##' Plot a SCC chart with ggplot2.
##'
##' @param x [\code{\link{scc_chart}}]\cr
##'    An object of type \code{link{scc_chart}}.
##' @param ... \cr
##'    Not used.
##' @param xlab [\code{character(1)}]\cr
##'    A title for the x axis.
##' @param ylab [\code{character(1)}]\cr
##'    A title for the y axis.
##' @param title [\code{character(1)}]\cr
##'    A title for the plot. See main parameter in plot function.
##' @param show_histogram [\code{character(1)}]\cr
##'    Should a histogram of the y-value be depicted beside the chart?
##'    Default ist \code{FALSE}.
##'
##' @return A \code{\link[ggplot2]{ggplot2}} object.
##' 
##' @S3method autoplot scc_chart
##' @method autoplot scc_chart
autoplot.scc_chart = function(x, ..., 
	xlab="block", ylab=x$y_value_name, title=x$description,
	show_histogram=FALSE) {
  	# extract relavant data
  	desired_value = x$desired_value
  	n_blocks = number_of_blocks(x)

  	# base chart plot
  	df = data.frame(block=1:n_blocks, y=x$y_value)
  	pl = ggplot(df, aes(x = block, y = y)) 
  	pl = pl + geom_line(colour = "gray")
  	pl = pl + geom_point(shape = 1, size = 3)
  
  	# check whether warning and control bounds are constant 
  	if (length(desired_value) == 1) {
    	pl = pl + geom_hline(linetype = "solid", colour = "grey", yintercept = as.numeric(desired_value))
  	} else {
   		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, y = desired_value), aes(x = block, y = y))
  	}
  
  	if (length(x$bounds$UCB) == 1) {
    	pl = pl + geom_hline(linetype = "dashed", colour = "black", yintercept = as.numeric(c(x$bounds$LCB, x$bounds$UCB)))
  	} else {
  		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, lower = x$bounds$LCB), aes(x = block, y = lower), linetype = "dashed", colour = "black")
    	pl = pl + geom_line(data = data.frame(block = 1:n_blocks, upper = x$bounds$UCB), aes(x = block, y = upper), linetype = "dashed", colour = "black")
  	}

  	if ("LWB" %in% names(x$bounds) && "UWB" %in% names(x$bounds)) {
		if (length(x$bounds$UWB) == 1) {
			pl = pl + geom_hline(linetype = "dashed", colour = "black", yintercept = as.numeric(c(x$bounds$LWB, x$bounds$UWB)))
	  	} else {
	  		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, lower = x$bounds$LWB), aes(x = block, y = lower), linetype = "dashed", colour = "darkgrey")
	    	pl = pl + geom_line(data = data.frame(block = 1:n_blocks, upper = x$bounds$UWB), aes(x = block, y = upper), linetype = "dashed", colour = "darkgrey") 
	  	}
  	}

  	# add axis labels
  	pl = pl + scale_x_continuous(xlab)
  	pl = pl + scale_y_continuous(ylab)
  	pl = pl + ggtitle(title)
  
  	# additional absolute frequency histogram with kernel density estimation
  	# FIXME: think about this. Really necsary? grid.arrange does not return ggplot object!!!
    if (show_histogram) {
  		pl_hist = ggplot(df, aes(x = y)) + 
  		geom_histogram(aes(y = ..density..), colour = "darkgreen", alpha = 0.3, fill = "green", bin.width = 1) +
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