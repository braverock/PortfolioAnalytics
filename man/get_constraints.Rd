\name{get_constraints}
\alias{get_constraints}
\title{Helper function to get the enabled constraints out of the portfolio object}
\usage{
  get_constraints(portfolio)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio'}
}
\value{
  an object of class 'constraint' which is a flattened list
  of enabled constraints
}
\description{
  When the v1_constraint object is instantiated via
  constraint, the arguments min_sum, max_sum, min, and max
  are either specified by the user or default values are
  assigned. These are required by other functions such as
  optimize.portfolio and constrained . This function will
  check that these variables are in the portfolio object in
  the constraints list. We will default to min_sum=1 and
  max_sum=1 if leverage constraints are not specified.  We
  will default to min=-Inf and max=Inf if box constraints
  are not specified.  This function is used at the
  beginning of optimize.portfolio and other functions to
  extract the constraints from the portfolio object. Uses
  the same naming as the v1_constraint object which may be
  useful when passed to other functions.
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{portfolio.spec}}
}

