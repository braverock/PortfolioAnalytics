\name{fn_map}
\alias{fn_map}
\title{mapping function to transform or penalize weights that violate constraints}
\usage{
  fn_map(weights, portfolio, relax = FALSE, ...)
}
\arguments{
  \item{weights}{vector of weights}

  \item{portfolio}{object of class portfolio}

  \item{relax}{TRUE/FALSE, default FALSE. Enable
  constraints to be relaxed.}
}
\value{
  \itemize{ \item{weights: }{vector of transformed weights
  meeting constraints} \item{min: }{vector of min box
  constraints that may have been modified if relax=TRUE}
  \item{max: }{vector of max box constraints that may have
  been modified if relax=TRUE} \item{cLO: }{vector of lower
  bound group constraints that may have been modified if
  relax=TRUE} \item{cUP: }{vector of upper bound group
  constraints that may have been modified if relax=TRUE} }
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

  If relax=TRUE, we will attempt to relax the constraints
  if a feasible portfolio could not be formed with an
  initial call to \code{rp_transform}. We will attempt to
  relax the constraints up to 5 times. If we do not have a
  feasible portfolio after attempting to relax the
  constraints, then we will default to returning the
  weights vector that violates the constraints.

  Leverage, box, group, and position limit constraints are
  transformed. Diversification and turnover constraints are
  penalized
}
\author{
  Ross Bennett
}

