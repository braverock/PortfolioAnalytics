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
  the constraints list. This function could be used at the
  beginning of optimize.portfolio or other functions to
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

