\name{constrained_group_tmp}
\alias{constrained_group_tmp}
\title{Generic function to impose group constraints on a vector of weights}
\usage{
  constrained_group_tmp(groups, cLO, cUP, weights, min_sum,
    max_sum, normalize = TRUE)
}
\arguments{
  \item{groups}{vector to group assets}

  \item{cLO}{vector of group weight minimums}

  \item{cUP}{vector of group weight maximums}

  \item{weights}{vector of weights}

  \item{min_sum}{minimum sum of weights}

  \item{max_sum}{maximum sum of weights}

  \item{normalize}{TRUE/FALSE to normalize the weights
  vector to satisfy the min_sum and max_sum constraints}
}
\description{
  This function gets group subsets of the weights vector
  and checks if the sum of the weights in that group
  violates the minimum or maximum value. If the sum of
  weights in a given group violates its maximum or minimum
  value, the group of weights is normalized to be equal to
  the minimum or maximum value. This group normalization
  causes the sum of weights to change. The weights vector
  is then normalized so that the min_sum and max_sum
  constraints are satisfied. This "re-normalization" of the
  weights vector may then cause the group constraints to
  not be satisfied.
}
\details{
  Group constraints are implemented in ROI solvers, but
  this function could be used in constrained_objective for
  random portfolios, DEoptim, pso, or gensa solvers.
}
\author{
  Ross Bennett
}

