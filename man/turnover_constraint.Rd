\name{turnover_constraint}
\alias{turnover_constraint}
\title{constructor for turnover_constraint}
\usage{
  turnover_constraint(type, max.turnover, enabled = FALSE,
    ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{max.turnover}{maximum turnover value}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="turnover" is specified. see
  \code{\link{add.constraint}} This function allows the
  user to specify a maximum turnover constraint
}
\details{
  Note that turnover constraint is currently only supported
  for global minimum variance problem with solve.QP plugin
}
\author{
  Ross Bennett
}

