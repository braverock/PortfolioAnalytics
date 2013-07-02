\name{volatility_constraint}
\alias{volatility_constraint}
\title{constructor for volatility_constraint}
\usage{
  volatility_constraint(type, vol_target, enabled = FALSE,
    ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{vol_target}{target volatilty constraint}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="volatility" is specified,
  \code{\link{add.constraint}} Penalize if portfolio
  standard deviation deviates from volatility target
}
\author{
  Ross Bennett
}

