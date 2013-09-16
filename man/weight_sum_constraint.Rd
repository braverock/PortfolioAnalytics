\name{weight_sum_constraint}
\alias{weight_sum_constraint}
\title{constructor for weight_sum_constraint}
\usage{
  weight_sum_constraint(type = "weight_sum",
    min_sum = 0.99, max_sum = 1.01, enabled = TRUE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{min_sum}{minimum sum of all asset weights, default
  0.99}

  \item{max_sum}{maximum sum of all asset weights, default
  1.01}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify
  weight_sum constraints}
}
\value{
  an object of class 'weight_sum_constraint'
}
\description{
  The constraint specifies the upper and lower bound on the
  sum of the weights. This function is called by
  add.constraint when "weight_sum", "leverage",
  "full_investment", "dollar_neutral", or "active" is
  specified as the type. see \code{\link{add.constraint}}
}
\details{
  Special cases for the weight_sum constraint are
  "full_investment" and "dollar_nuetral" or "active"

  If \code{type="full_investment"}, \code{min_sum=1} and
  \code{max_sum=1}

  If \code{type="dollar_neutral"} or \code{type="active"},
  \code{min_sum=0}, and \code{max_sum=0}
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

# min_sum and max_sum can be specified with type="weight_sum" or type="leverage"
pspec <- add.constraint(pspec, type="weight_sum", min_sum=1, max_sum=1)

# Specify type="full_investment" to set min_sum=1 and max_sum=1
pspec <- add.constraint(pspec, type="full_investment")

# Specify type="dollar_neutral" or type="active" to set min_sum=0 and max_sum=0
pspec <- add.constraint(pspec, type="dollar_neutral")
pspec <- add.constraint(pspec, type="active")
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

