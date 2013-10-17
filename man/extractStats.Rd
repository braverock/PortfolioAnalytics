\name{extractStats}
\alias{extractStats}
\alias{extractStats.optimize.portfolio.DEoptim}
\alias{extractStats.optimize.portfolio.GenSA}
\alias{extractStats.optimize.portfolio.parallel}
\alias{extractStats.optimize.portfolio.pso}
\alias{extractStats.optimize.portfolio.random}
\alias{extractStats.optimize.portfolio.ROI}
\title{extract some stats and weights from a portfolio run via \code{optimize.portfolio}}
\usage{
  extractStats(object, prefix = NULL, ...)
}
\arguments{
  \item{object}{list returned by optimize.portfolio}

  \item{prefix}{prefix to add to output row names}

  \item{...}{any other passthru parameters}
}
\description{
  This function will dispatch to the appropriate class
  handler based on the input class of the
  optimize.portfolio output object.
}
\details{
  For \code{optimize.portfolio} objects:

  In general, \code{extractStats} will extract the values
  objective measures and weights at each iteration of a set
  of weights. This is the case for the DEoptim, random
  portfolios, and pso solvers that return trace
  information. Note that \code{trace=TRUE} must be
  specified in \code{optimize.portfolio} to return the
  trace information.

  For \code{optimize.portfolio.pso} objects, this function
  will extract the weights (swarm positions) from the PSO
  output and the out values (swarm fitness values) for each
  iteration of the optimization. This function can be slow
  because we need to run \code{constrained_objective} to
  calculate the objective measures on the transformed
  weights.

  For \code{optimize.portfolio.rebalancing} objects:

  The \code{extractStats} function will return a list of
  the objective measures and weights at each rebalance date
  for \code{optimize.portfolio.rebalancing} objects. The
  objective measures and weights of each iteration or
  permutation will be returned if the optimization was done
  with DEoptim, random portfolios, or pso. This could
  potentially result in a very large list object where each
  list element has thousands of rows of at each rebalance
  period.

  The output from the GenSA solver does not store weights
  evaluated at each iteration The GenSA output for
  trace.mat contains nb.steps, temperature, function.value,
  and current.minimum
}
\seealso{
  \code{\link{optimize.portfolio}}
}

