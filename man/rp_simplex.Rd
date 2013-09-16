\name{rp_simplex}
\alias{rp_simplex}
\title{Generate random portfolios using the simplex method}
\usage{
  rp_simplex(portfolio, permutations, fev = 0:5, ...)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio'
  specifying the constraints for the optimization, see
  \code{\link{portfolio.spec}}}

  \item{permutations}{integer: number of unique constrained
  random portfolios to generate}

  \item{fev}{scalar or vector for FEV biasing}

  \item{\dots}{any other passthru parameters}
}
\value{
  a matrix of random portfolio weights
}
\description{
  This function generates random portfolios based on the
  method outlined in the Shaw paper. Need to add reference.
}
\details{
  The simplex method is useful to generate random
  portfolios with the full investment constraint where the
  sum of the weights is equal to 1 and min box constraints
  with no upper bound on max constraints. Values for
  min_sum and max_sum will be ignored, the sum of weights
  will equal 1. All other constraints such as group and
  position limit constraints will be handled by
  elimination. If the constraints are very restrictive,
  this may result in very few feasible portfolios
  remaining.

  The random portfolios are created by first generating a
  set of uniform random numbers. \deqn{U \sim [0, 1]} The
  portfolio weights are then transformed to satisfy the min
  of the box constraints. \deqn{w_{i} = min_{i} + (1 -
  \sum_{j=1}^{N} min_{j})
  \frac{log(U_{i}^{q}}{\sum_{k=1}^{N}log(U_{k}^{q}}}

  \code{fev} controls the Face-Edge-Vertex (FEV) biasing
  where \deqn{q=2^{fev}} As \code{q} approaches infinity,
  the set of weights will be concentrated in a single
  asset. To sample the interior and exterior, \code{fev}
  can be passed in as a vector. The number of portfolios,
  \code{permutations}, and the length of \code{fev} affect
  how the random portfolios are generated. For example, if
  \code{permutations=10000} and \code{fev=0:4}, 2000
  portfolios will be generated for each value of
  \code{fev}.
}

