\name{dataframe_to_list}
\alias{dataframe_to_list}
\title{Function which takes a dataframe and converts it to a list.}
\usage{
dataframe_to_list(x, byrow = FALSE, na.rm = FALSE)
}
\arguments{
  \item{x}{[\code{data.frame}]\cr The data frame.}

  \item{byrow}{[\code{logical(1)}]\cr Indicates whether
  data frame should by converted rowwise or colwise.}

  \item{na.rm}{[\code{logical(1)}]\cr Indicates whether NAs
  should be removed.}
}
\value{
List.
}
\description{
Each list element complies to one row respectively column
of the original dataframe.
}

