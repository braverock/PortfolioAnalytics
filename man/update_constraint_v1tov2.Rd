\name{update_constraint_v1tov2}
\alias{update_constraint_v1tov2}
\title{Helper function to update v1_constraint objects to v2 specification in the portfolio object}
\usage{
  update_constraint_v1tov2(portfolio, v1_constraint)
}
\arguments{
  \item{portfolio}{portfolio object passed into
  optimize.portfolio}

  \item{v1_constraint}{object of type v1_constraint passed
  into optimize.portfolio}
}
\value{
  portfolio object containing constraints and objectives
  from v1_constraint
}
\description{
  The function takes the constraints and objectives
  specified in the v1_constraint object and updates the
  portfolio object with those constraints and objectives.
  This function is used inside optimize.portfolio to
  maintain backwards compatibility if the user passes in a
  v1_constraint object for the constraint arg in
  optimize.portfolio.
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{portfolio.spec}},
  \code{\link{add.constraint}}
}

