\name{extractStats.optimize.portfolio.pso}
\alias{extractStats.optimize.portfolio.pso}
\title{extract some stats from a portfolio list run with pso via
\code{\link{optimize.portfolio}}}
\usage{
  extractStats.optimize.portfolio.pso(object,
    prefix = NULL, ...)
}
\arguments{
  \item{object}{list returned by optimize.portfolio}

  \item{prefix}{prefix to add to output row names}

  \item{...}{any other passthru parameters}
}
\description{
  This function will extract the weights (swarm positions)
  from the PSO output and the out value (swarm fitness
  values) for each iteration of the optimization. This
  function can be slow because we need to run
  \code{constrained_objective} to calculate the objective
  measures on the weights.
}
\author{
  Ross Bennett
}

