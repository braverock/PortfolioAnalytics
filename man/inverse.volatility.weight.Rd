\name{inverse.volatility.weight}
\alias{inverse.volatility.weight}
\title{Create an inverse volatility weighted portfolio}
\usage{
  inverse.volatility.weight(R, portfolio, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{portfolio}{an object of type "portfolio" specifying
  the constraints and objectives for the optimization}

  \item{\dots}{any other passthru parameters to
  \code{constrained_objective}}
}
\value{
  a list containing the returns, weights, objective
  measures, call, and portfolio object
}
\description{
  This function calculates objective measures for an equal
  weight portfolio.
}
\details{
  This function is simply a wrapper around
  \code{\link{constrained_objective}} to calculate the
  objective measures in the given \code{portfolio} object
  of an inverse volatility weight portfolio. The portfolio
  object should include all objectives to be calculated.
}
\author{
  Peter Carl
}

