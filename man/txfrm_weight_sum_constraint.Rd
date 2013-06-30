\name{txfrm_weight_sum_constraint}
\alias{txfrm_weight_sum_constraint}
\title{Transform weights that violate weight_sum constraints}
\usage{
  txfrm_weight_sum_constraint(weights, min_sum, max_sum)
}
\arguments{
  \item{weights}{vector of weights}

  \item{min_sum}{minimum sum of asset weights}

  \item{max_sum}{maximum sum of asset weights}
}
\description{
  This is a helper function called inside constraint_fnMap
  to transform the weights vector to satisfy weight_sum
  constraints.
}
\author{
  Ross Bennett
}

