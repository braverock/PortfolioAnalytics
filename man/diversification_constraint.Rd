\name{diversification_constraint}
\alias{diversification_constraint}
\title{constructor for diversification_constraint}
\usage{
  diversification_constraint(type = "diversification",
    div_target = NULL, enabled = TRUE, message = FALSE,
    ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{div_target}{diversification target value}

  \item{enabled}{TRUE/FALSE}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters to specify
  diversification constraint an object of class
  'diversification_constraint'}
}
\description{
  The diversification constraint specifies a target
  diversification value. This function is called by
  add.constraint when type="diversification" is specified,
  see \code{\link{add.constraint}}.
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

