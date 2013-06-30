\name{constraint_fn_map}
\alias{constraint_fn_map}
\title{Constraint mapping function}
\usage{
  constraint_fn_map(weights, portfolio)
}
\arguments{
  \item{weights}{vector of weights}

  \item{portfolio}{object of class portfolio}
}
\description{
  The purpose of the mapping function is to transform a
  weights vector that does not meet all the constraints
  into a weights vector that does meet the constraints, if
  one exists, hopefully with a minimum of transformation. I
  think our first step should be to test each constraint
  type, in some sort of hierarchy, starting with box
  constraints (almost all solvers support box constraints,
  of course), since some of the other transformations will
  violate the box constraints, and we'll need to transform
  back again.
}
\author{
  Ross Bennett
}

