\name{txfrm_group_constraint}
\alias{txfrm_group_constraint}
\title{Transform weights that violate group constraints}
\usage{
  txfrm_group_constraint(weights, groups, cLO, cUP)
}
\arguments{
  \item{weights}{vector of weights}

  \item{groups}{vector of groups}

  \item{cLO}{vector of minimum group weights from group
  constraints}

  \item{cUP}{vector of maximum group weights from group
  constraints}
}
\description{
  This is a helper function called inside constraint_fnMap
  to transform the weights vector to satisfy group
  constraints.
}
\author{
  Ross Bennett
}

