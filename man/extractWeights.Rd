\name{extractWeights}
\alias{extractWeights}
\title{Extract weights from a portfolio run via \code{optimize.portfolio} or \code{optimize.portfolio.rebalancing}}
\usage{
  extractWeights(object, ...)
}
\arguments{
  \item{object}{list returned by optimize.portfolio}

  \item{\dots}{any other passthru parameters}
}
\description{
  This function will dispatch to the appropriate class
  handler based on the input class of the
  optimize.portfolio or optimize.portfolio.rebalancing
  output object
}
\seealso{
  \code{\link{optimize.portfolio}},
  \code{\link{optimize.portfolio.rebalancing}}
}

