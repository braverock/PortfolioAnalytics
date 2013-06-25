\name{volatility_constraint}
\alias{volatility_constraint}
\title{constructor for volatility_constraint}
\usage{
  volatility_constraint(type, min.vol, max.vol,
    enabled = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{min.vol}{minimum volatility constraint}

  \item{max.vol}{maximum volatilty constraint}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="volatility" is specified,
  \code{\link{add.constraint}} If portfolio standard
  deviation is less than min.vol, add penalty to maximize
  If portfolio standard deviation is greater than max.vol,
  add penalty to minimize
}
\author{
  Ross Bennett
}

