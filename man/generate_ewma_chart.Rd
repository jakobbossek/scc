\name{generate_ewma_chart}
\alias{generate_ewma_chart}
\title{Generates an exponentially weighted moving average control chart.}
\usage{
generate_ewma_chart(blocks, center, sd, parameters = list(lambda = 0.5),
  description, y_value_name = "measured quantity", desired_value,
  desired_value_name)
}
\arguments{
  \item{blocks}{[\code{list}]\cr List of blocks from which
  quantity is computed.}

  \item{center}{[\code{numeric(1)}]\cr Long term process
  mean.}

  \item{sd}{[\code{numeric(1)}]\cr Standard deviation of
  the process.}

  \item{parameters}{[\code{list}]\cr List of parameters
  needed to compute control respectively warning bounds.}

  \item{description}{[\code{character(1)}]\cr Description
  of the measured quantity. Used as subtitle in graphical
  representation.}

  \item{y_value_name}{[\code{character(1)}]\cr Name for the
  measured characteristic. Used for example for the y axis
  label in the plot by default.}

  \item{desired_value}{[\code{numeric}]\cr Reference value
  of the measured quantity, i.e., the average value. May be
  a single value or comply with the number of blocks (see
  blocks parameter).}

  \item{desired_value_name}{[\code{character(1)}]\cr Label
  of the desired value. Used in the graphical
  representation.}
}
\value{
Object of class \code{scc_chart} and \code{ewma_chart}.
}
\description{
Generates an exponentially weighted moving average control
chart.
}

