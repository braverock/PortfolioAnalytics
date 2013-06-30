\name{txfrm_box_constraint}
\alias{txfrm_box_constraint}
\title{Transform weights that violate min or max box constraints}
\usage{
  txfrm_box_constraint(weights, min, max)
}
\arguments{
  \item{weights}{vector of weights}

  \item{min}{vector of minimum asset weights from box
  constraints}

  \item{max}{vector of maximum asset weights from box
  constraints}
}
\description{
  This is a helper function called inside constraint_fnMap
  to transform the weights vector to satisfy box
  constraints.
}
\author{
  Ross Bennett
}

