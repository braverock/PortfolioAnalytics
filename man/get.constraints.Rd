\name{get.constraints}
\alias{get.constraints}
\title{Helper function to get the enabled constraints out of the portfolio object, see \code{\link{portfolio.spec}}
 Returns an object of class constraint which is a flat list of weight_sum, box, and group constraints.
 Uses the same naming as the v1_constraint object which may be useful when passed to other functions.}
\usage{
  get.constraints(portfolio)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio'}
}
\description{
  Helper function to get the enabled constraints out of the
  portfolio object, see \code{\link{portfolio.spec}}
  Returns an object of class constraint which is a flat
  list of weight_sum, box, and group constraints.  Uses the
  same naming as the v1_constraint object which may be
  useful when passed to other functions.
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{portfolio.spec}}, \code{\link{constraint_v2}}
}

