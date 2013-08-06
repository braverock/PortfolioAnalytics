\name{extractStats.optimize.portfolio.GenSA}
\alias{extractStats.optimize.portfolio.GenSA}
\title{extract some stats from a portfolio list run with GenSA via
\code{\link{optimize.portfolio}}}
\usage{
  extractStats.optimize.portfolio.GenSA(object,
    prefix = NULL, ...)
}
\arguments{
  \item{object}{list returned by optimize.portfolio}

  \item{prefix}{prefix to add to output row names}

  \item{...}{any other passthru parameters}
}
\description{
  This function will extract the optimal portfolio weights
  and objective measures The GenSA output does not store
  weights evaluated at each iteration The GenSA output for
  trace.mat contains nb.steps, temperature, function.value,
  and current.minimum
}

