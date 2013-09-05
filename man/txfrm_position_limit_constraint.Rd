\name{txfrm_position_limit_constraint}
\alias{txfrm_position_limit_constraint}
\title{Transform weights for position_limit constraints}
\usage{
  txfrm_position_limit_constraint(weights, max_pos,
    nassets, tolerance = .Machine$double.eps^0.5)
}
\arguments{
  \item{weights}{vector of weights}

  \item{max_pos}{maximum position of assets with non_zero
  weights}

  \item{nassets}{number of assets}

  \item{tolerance}{tolerance for non-zero weights}
}
\description{
  This is a helper function called inside constraint_fnMap
  to transform the weights vector to satisfy position_limit
  constraints. This function sets the minimum
  nassets-max_pos assets equal to 0 such that the max_pos
  number of assets will have non-zero weights.
}
\author{
  Ross Bennett
}

