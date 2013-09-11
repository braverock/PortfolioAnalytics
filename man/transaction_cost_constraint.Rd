\name{transaction_cost_constraint}
\alias{transaction_cost_constraint}
\title{constructor for transaction_cost_constraint}
\usage{
  transaction_cost_constraint(type = "transaction_cost",
    assets, ptc, enabled = TRUE, message = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{assets}{number of assets, or optionally a named
  vector of assets specifying initial weights}

  \item{ptc}{proportional transaction cost value}

  \item{enabled}{TRUE/FALSE}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  The transaction cost constraint specifies a proportional
  cost value. This function is called by add.constraint
  when type="transaction_cost" is specified, see
  \code{\link{add.constraint}}.
}
\details{
  Note that with the ROI solvers, proportional transaction
  cost constraint is currently only supported for the
  global minimum variance and quadratic utility problems
  with ROI quadprog plugin.
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec, type="transaction_cost", ptc=0.01)
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

