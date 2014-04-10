\name{longest_outlier_sequence}
\alias{longest_outlier_sequence}
\title{Determines the length of the longest consecutive outlier sequence.}
\usage{
longest_outlier_sequence(x, ...)
}
\arguments{
  \item{x}{[\code{\link{scc_chart}}]\cr An object of type
  \code{\link{scc_chart}}.}

  \item{...\cr}{Not used.}
}
\value{
List containing the length and the starting index.  Length
is -1, if the are no observations violating the control
bounds.
}
\description{
Determines the length of the longest consecutive outlier
sequence.
}

