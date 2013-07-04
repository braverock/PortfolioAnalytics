\name{fn_map}
\alias{fn_map}
\title{mapping function to transform or penalize weights that violate constraints}
\usage{
  fn_map(weights, portfolio, ...)
}
\arguments{
  \item{weights}{vector of weights}

  \item{portfolio}{object of class portfolio}
}
\value{
  \itemize{ \item{weights: }{vector of transformed weights
  meeting constraints} \item{out: }{penalty term} }
}
\description{
  The purpose of the mapping function is to transform a
  weights vector that does not meet all the constraints
  into a weights vector that does meet the constraints, if
  one exists, hopefully with a minimum of transformation.
}
\details{
  I think our first step should be to test each constraint
  type, in some sort of hierarchy, starting with box
  constraints (almost all solvers support box constraints,
  of course), since some of the other transformations will
  violate the box constraints, and we'll need to transform
  back again.

  This function will replace constraint_fn_map

  leverage, box, group, and position limit constraints are
  transformed diversification and turnover constraints are
  penalized
}
\author{
  Ross Bennett
}

