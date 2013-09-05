\name{position_limit_constraint}
\alias{position_limit_constraint}
\title{constructor for position_limit_constraint}
\usage{
  position_limit_constraint(type = "position_limit",
    assets, max_pos = NULL, max_pos_long = NULL,
    max_pos_short = NULL, enabled = TRUE, message = FALSE,
    ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{assets}{named vector of assets specifying initial
  weights}

  \item{max_pos}{maximum number of assets with non-zero
  weights}

  \item{max_pos_long}{maximum number of assets with long
  (i.e. buy) positions}

  \item{max_pos_short}{maximum number of assets with short
  (i.e. sell) positions}

  \item{enabled}{TRUE/FALSE}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters to specify
  position limit constraints}
}
\description{
  This function is called by add.constraint when
  type="position_limit" is specified,
  \code{\link{add.constraint}} Allows the user to specify
  the maximum number of positions (i.e. number of assets
  with non-zero weights) as well as the maximum number of
  long and short positions.
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos_long=3, max_pos_short=1)
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

