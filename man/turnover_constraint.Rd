\name{turnover_constraint}
\alias{turnover_constraint}
\title{constructor for turnover_constraint}
\usage{
  turnover_constraint(type = "turnover", turnover_target,
    enabled = TRUE, message = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{turnover_target}{target turnover value}

  \item{enabled}{TRUE/FALSE}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="turnover" is specified. see
  \code{\link{add.constraint}} This function allows the
  user to specify a target turnover value
}
\details{
  Note that turnover constraint is currently only supported
  for global minimum variance problem with ROI quadprog
  plugin
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.6)
}
\author{
  Ross Bennett
}

