##' Plot a SCC chart.
##'
##' @param x [\code{\link{scc_chart}}]\cr
##'    An object of type \code{\link{scc_chart}}.
##' @param ... \cr
##'    Further params passed to plot function.
##'
##' @return Nothing. As a side-effect the chart is plotted.
##'
##' @S3method plot scc_chart
##' @method print scc_chart
plot.scc_chart = function(x, ...) {
	# FIXME: add labels for bounds to the right margin
	n_blocks = number_of_blocks(x)
	bounds = x$bounds

	# add enough whitespace between plot border and control lines
	ylim_space = 0.1*(bounds$UCB-bounds$LCB)

	# draw polygon
	plot(1:n_blocks, x$y_value, type="l", 
		lwd=2, 
		main=x$description,
		xlab="block",
		ylab=x$y_value_name,
		ylim=c(bounds$LCB - ylim_space, bounds$UCB + ylim_space),
		...)

	grid()
	# draw points of polygon
	points(1:n_blocks, x$y_value, pch=19)
	# determine number of desired_value to place the label correctly
	y_value_pos = if (length(x$desired_value) == 1) x$desired_value else x$desired_value[n_blocks]
	if (length(x$desired_value) == 1) {
		abline(h=x$desired_value, col="darkgray", lty=5)
	} else {
		lines(1:n_blocks, x$desired_value, lty=5)
	}
	# save old pars (mtext does not support string rotation, therefore text is used
	# and xpd is set to TRUE to be able to draw things outside the plot region)
	
	# old_pars = par()
	# par(xpd=TRUE)
	mtext(x$desired_value_name, side=4, at=c(n_blocks, y_value_pos), cex=0.7, outer=FALSE, line=1)
	# text(x=n_blocks+0.4, y=y_value_pos, cex=0.7, labels=x$desired_value_name, pos=4, offset=0.5)
	# par(old_pars)

	# draw control bounds
	# FIXME: handle non-constant control bounds
	if (length(bounds$UCB) == 1) {
		print("hallo")
		abline(h=bounds$UCB, lty=5, lwd=2, col="tomato")
		abline(h=bounds$LCB, lty=5, lwd=2, col="tomato")
	}

	#draw control bounds
	# FIXME: handle non-constant warning bounds
	if ("LWB" %in% names(bounds) && "UWB" %in% names(bounds)) {
		if (length(bounds$UWB) == 1) {
			abline(h=bounds$UWB, lty=5, lwd=2, col="tomato2")
			abline(h=bounds$LWB, lty=5, lwd=2, col="tomato2")
		}
	}
}