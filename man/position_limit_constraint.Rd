\name{position_limit_constraint}
\alias{position_limit_constraint}
\title{constructor for position_limit_constraint}
\usage{
  position_limit_constraint(type, max_pos, enabled = FALSE,
    ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{max_pos}{maximum number of positions}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="position_limit" is specified,
  \code{\link{add.constraint}} Allows the user to specify
  the maximum number of positions (i.e. number of assets
  with non-zero weights)
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)
}
\author{
  Ross Bennett #'
}

