\name{autoplot.scc_chart}
\alias{autoplot.scc_chart}
\title{Plot a SCC chart with ggplot2.}
\usage{
\method{autoplot}{scc_chart}(x, ..., xlab = "block", ylab = x$y_value_name,
  title = x$description, show_histogram = FALSE)
}
\arguments{
  \item{x}{[\code{\link{scc_chart}}]\cr An object of type
  \code{link{scc_chart}}.}

  \item{...}{\cr Not used.}

  \item{xlab}{[\code{character(1)}]\cr A title for the x
  axis.}

  \item{ylab}{[\code{character(1)}]\cr A title for the y
  axis.}

  \item{title}{[\code{character(1)}]\cr A title for the
  plot. See main parameter in plot function.}

  \item{show_histogram}{[\code{character(1)}]\cr Should a
  histogram of the y-value be depicted beside the chart?
  Default ist \code{FALSE}.}
}
\value{
A \code{\link[ggplot2]{ggplot2}} object.
}
\description{
Plot a SCC chart with ggplot2.
}

