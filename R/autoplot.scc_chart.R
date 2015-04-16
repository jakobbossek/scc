#' @title Plot a SCC chart with \pkg{ggplot2}.
#'
#' @description Takes a \code{scc_chart} object and generates a graphical representation
#' of the chart in form of a \code{\link[ggplot2]{ggplot}} object.
#'
#' @param object [\code{scc_chart}]\cr
#'   Chart for plotitng.
#' @param xlab [\code{character(1)}]\cr
#'   A title for the x axis.
#' @param ylab [\code{character(1)}]\cr
#'   A title for the y axis.
#' @param title [\code{character(1)}]\cr
#'   A title for the plot. See main parameter in plot function.
#' @param append.hist [\code{logical(1)}]\cr
#'   Should a histogram of the y-value be depicted beside the chart?
#'   Default ist \code{FALSE}.
#' @param ... [any]\cr
#'   Not used.
#' @return A \code{\link[ggplot2]{ggplot2}} object.
#' @export
autoplot.scc_chart = function(
  object,
	xlab = "block", ylab = object$y_value_name, title = object$description,
	append.hist = FALSE, ...) {
    assertClass(object, "scc_chart")
    assertCharacter(xlab, len = 1L, any.missing = FALSE)
    assertCharacter(ylab, len = 1L, any.missing = FALSE)
    assertCharacter(title, len = 1L, any.missing = FALSE)
    assertFlag(append.hist)
    #FIXME: add flag to let the user set if the longest outlier sequence shall be highlighted
    #FIXME: this function is far too long. Split it up into some more functions.

  	# extract relavant data
  	desired_value = object$desired_value
  	n_blocks = getNumberOfBlocks(object)

  	# base chart plot
  	df = data.frame(block = seq(n_blocks), y = object$y_value)
  	pl = ggplot(df, aes_string(x = "block", y = "y"))
  	pl = pl + geom_line(colour = "gray")
  	pl = pl + geom_point(shape = 1, size = 3)

  	# check whether warning and control bounds are constant
    #FIXME: the following about 30 lines seem to have much copy&paste code. Do this better!
  	if (length(desired_value) == 1) {
    	pl = pl + geom_hline(linetype = "solid", colour = "grey", yintercept = as.numeric(desired_value))
  	} else {
   		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, y = desired_value),
        aes_string(x = "block", y = "y"))
  	}

  	if (length(object$bounds$UCB) == 1) {
    	pl = pl + geom_hline(linetype = "dashed", colour = "black", yintercept = as.numeric(c(object$bounds$LCB, object$bounds$UCB)))
  	} else {
  		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, lower = object$bounds$LCB),
        aes_string(x = "block", y = "lower"), linetype = "dashed", colour = "black")
    	pl = pl + geom_line(data = data.frame(block = 1:n_blocks, upper = object$bounds$UCB),
        aes_string(x = "block", y = "upper"), linetype = "dashed", colour = "black")
  	}

  	if ("LWB" %in% names(object$bounds) && "UWB" %in% names(object$bounds)) {
		if (length(object$bounds$UWB) == 1) {
			pl = pl + geom_hline(linetype = "dashed", colour = "black", yintercept =
        as.numeric(c(object$bounds$LWB, object$bounds$UWB)))
	  	} else {
	  		pl = pl + geom_line(data = data.frame(block = 1:n_blocks, lower = object$bounds$LWB),
          aes_string(x = "block", y = "lower"), linetype = "dashed", colour = "darkgrey")
	    	pl = pl + geom_line(data = data.frame(block = 1:n_blocks, upper = object$bounds$UWB),
          aes_string(x = "block", y = "upper"), linetype = "dashed", colour = "darkgrey")
	  	}
  	}

  	# add axis labels
  	pl = pl + scale_x_continuous(xlab)
  	pl = pl + scale_y_continuous(ylab)
  	pl = pl + ggtitle(title)

  	# additional absolute frequency histogram with kernel density estimation
  	#FIXME: think about this. Really neccessary? grid.arrange does not return ggplot object!
    #       Maybe return a list of ggplot2 objects instead of using gridExtra? Attaching a
    #       histogram might be a good long example for the usage of scc.
    #FIXME: maybe better use arrange-grob or something like the following?
    # pl = autoplot(chart)
    # pl = appendHistogramm(pl)
    if (append.hist) {
      requirePackages("gridExtra", why = "scc::autoplot")
  		pl_hist = ggplot(df, aes_string(x = "y")) +
  		geom_histogram(aes_string(y = "..density.."), colour = "darkgreen", alpha = 0.3, fill = "green", bin.width = 1) +
  		geom_density(alpha = 0.4, fill = "white") +
  		scale_y_continuous("dens") +
  		theme(axis.ticks = element_blank(),
         	axis.title.y = element_blank(),
         	axis.text.y =  element_blank()) +
  		coord_flip()

	  	pl = grid.arrange(pl, pl_hist, widths = c(5/6, 1/6), ncol = 2)
  	}
  	return(pl)
}
