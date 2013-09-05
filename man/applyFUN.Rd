\name{applyFUN}
\alias{applyFUN}
\title{Apply a risk or return function to a set of weights}
\usage{
  applyFUN(R, weights, FUN = "mean", ...)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{weights}{a matrix of weights generated from
  random_portfolios or \code{optimize.portfolio}}

  \item{FUN}{name of a function}

  \item{...}{any passthrough arguments to FUN}
}
\description{
  This function is used to calculate risk or return metrics
  given a matrix of weights and is primarily used as a
  convenience function used in chart.Scatter functions
}
\author{
  Ross Bennett
}

