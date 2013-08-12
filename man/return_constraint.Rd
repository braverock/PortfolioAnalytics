\name{return_constraint}
\alias{return_constraint}
\title{constructor for return_constraint}
\usage{
  return_constraint(type = "return", return_target,
    enabled = TRUE, message = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{return_target}{return target value}

  \item{enabled}{TRUE/FALSE}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters}
}
\description{
  This function is called by add.constraint when
  type="return" is specified, \code{\link{add.constraint}}
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec, type="return", div_target=mean(colMeans(ret)))
}
\author{
  Ross Bennett
}

