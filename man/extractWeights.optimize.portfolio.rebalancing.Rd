\name{extractWeights.optimize.portfolio.rebalancing}
\alias{extractWeights.optimize.portfolio.rebalancing}
\title{extract time series of weights from output of optimize.portfolio.rebalancing}
\usage{
  extractWeights.optimize.portfolio.rebalancing(RebalResults,
    ...)
}
\arguments{
  \item{RebalResults}{object of type
  optimize.portfolio.rebalancing to extract weights from}

  \item{...}{any other passthru parameters}
}
\description{
  \code{\link{optimize.portfolio.rebalancing}} outputs a
  list of \code{\link{optimize.portfolio}} objects, one for
  each rebalancing period
}
\details{
  The output list is indexed by the dates of the
  rebalancing periods, as determined by \code{endpoints}
}
\seealso{
  \code{\link{optimize.portfolio.rebalancing}}
}

