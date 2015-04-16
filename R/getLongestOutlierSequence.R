#' Determines the length of the longest consecutive outlier sequence.
#'
#' @param x [\code{scc_chart}]\cr
#'   Chart.
#' @param ... [\code{list}]\cr
#'   Not used.
#' @return
#'  List containing the length and the starting index.
#'  Length is -1, if the are no observations violating the
#'  control bounds.
#' @export
getLongestOutlierSequence = function(x, ...) {
	UseMethod("getLongestOutlierSequence")
}

#' @export
getLongestOutlierSequence.scc_chart = function(x, ...) {
	# extract information
	n_blocks = getNumberOfBlocks(x)
	bounds = x$bounds

	# check which y values are beyonds the bounds
	below_LCB = (x$y_value <= bounds$LCB)
	above_UCB = (x$y_value >= bounds$UCB)

	# for the logenst sequence it does not matter
	# whether succeeding points lie below or above
	# the control bounds.
	outside_CB = below_LCB | above_UCB

	# search for longest sequence
  i = 1
  max_sequence_length = -1
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
        max_sequence_start_index = j - sequence_length
      }
    }
    i = i + 1
  }
  return(list(
    length = max_sequence_length,
    start_index = max_sequence_start_index)
  )
}
