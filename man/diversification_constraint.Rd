\name{diversification_constraint}
\alias{diversification_constraint}
\title{constructor for diversification_constraint}
\usage{
  diversification_constraint(type, div.target,
    enabled = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{div.target}{diversification target value}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="diversification" is specified,
  \code{\link{add.constraint}}
}
\author{
  Ross Bennett
}

