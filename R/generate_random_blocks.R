#' Function for generating random blocks
#'
#' @param n [integer(1)]\cr
#'   Number of blocks.
#' @param block_size [integer]\cr
#'   Size of each block.
#'
#' @export
generate_random_blocks <- function(n=10, block_size=5) {
	l = lapply(1:n, function(i) rnorm(block_size, mean=0, sd=1))
	names(l) = paste("block", 1:n, sep="")
	return(l)
}

#' Function which takes a dataframe and converts it to a list.
#'
#' Each list element complies to one row respectively column of
#' the original dataframe.
#'
#' @param x [\code{data.frame}]\cr
#'   The data frame.
#' @param byrow [\code{logical(1)}]\cr
#'   Indicates whether data frame should by converted rowwise or colwise.
#' @param na.rm [\code{logical(1)}]\cr
#'   Indicates whether NAs should be removed.
#'
#' @return
#'   List.
#'
#' @export
dataframe_to_list <- function(x, byrow=FALSE, na.rm=FALSE) {
	res = list()
	n = if (byrow) nrow(x) else ncol(x)
	for (i in 1:n) {
		res[[i]] = if(byrow) x[i,] else x[,i]
		if (na.rm) res[[i]] = res[[i]][!is.na(res[[i]])]
	}
	return(res)
}
