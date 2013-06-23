\name{get.constraints}
\alias{get.constraints}
\title{Helper function to get the enabled constraints out of the portfolio object, see \code{\link{portfolio.spec}}}
\usage{
  get.constraints(portfolio)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio'}
}
\description{
  When the v1_constraint object is instantiated via
  constraint, the arguments min_sum, max_sum, min, and max
  are either specified by the user or default values are
  assigned. These are required by other functions such as
  optimize.portfolio. This function will check that these
  variables are in the portfolio object in the constraints
  list. This function could be used at the beginning of
  optimize.portfolio to check the constraints in the
  portfolio object.
}
\details{
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

