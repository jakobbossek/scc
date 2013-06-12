##' Function for generating random blocks
##'
##' @param n [integer(1)]\cr
##'    Number of blocks.
##' @param block_size [integer]\cr
##'    Size of each block.
##'
##' @export
generate_random_blocks <- function(n=10, block_size=5) {
	l = lapply(1:n, function(i) rnorm(block_size, mean=0, sd=1))
	names(l) = paste("block", 1:n, sep="")
	return(l)
}