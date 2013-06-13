##' Determines the length of the longest consecutive outlier sequence.
##'
##' @param x [\code{\link{scc_chart}}]\cr
##'    An object of type \code{\link{scc_chart}}.
##' @param ... \cr 
##'    Not used.
##'
##' @return List containing the length and the starting index.
##'
##' @export 
longest_outlier_sequence = function(x, ...) {
	UseMethod("longest_outlier_sequence")
}

##' @S3method longest_outlier_sequence scc_chart
longest_outlier_sequence.scc_chart = function(x, ...) {
	# extract information
	n_blocks = number_of_blocks(x)
	below_LCB = (x$y_value <= bounds$LCB)
	above_UCB = (x$y_value >= bounds$UCB)

	# for the logenst sequence it does not matter
	# whether succeeding points lie below or above
	# the control bounds.
	outside_CB = below_LCB | above_UCB

	# search for longest sequence
	# FIXME: to this better (maybe with dynamic programming approach)
    i = 1
    max_sequence_length = 0
    max_sequence_start_index = NULL
    while (i <= n_blocks) {
    	if (outside_CB[i]) {
    		j = i
    		sequence_length = 0
    		# while there is succeedingly an outlier
    		while (outside_CB[j] & j <= n_blocks) {
    			sequence_length = sequence_length + 1
    			j = j + 1
    		}
    		# save starting index and maximal sequence length
    		if (sequence_length > max_sequence_length) {
    			max_sequence_length = sequence_length
    			max_seq_start_index = j - sequence_length
    		}
    	}
    	i = i + 1
    }
    return(list(length = max_sequence_length, start_index = max_seq_start_index))
}