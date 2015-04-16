#' Function for generating random blocks
#'
#' @param n [integer(1)]\cr
#'   Number of blocks.
#' @param block.size [integer]\cr
#'   Size of each block.
#' @return
#'   List of blocks.
#' @export
generateRandomBlocks = function(n = 10, block.size = 5) {
	l = lapply(1:n, function(i) rnorm(block.size, mean = 0, sd = 1))
	names(l) = paste0("block", seq(n))
	return(l)
}
