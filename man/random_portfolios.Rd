\name{random_portfolios_v2}
\alias{random_portfolios}
\alias{random_portfolios_v2}
\title{version 2 generate an arbitary number of constrained random portfolios}
\usage{
  random_portfolios_v2(portfolio, permutations = 100, ...)
}
\arguments{
  \item{portfolio}{an object of type "portfolio" specifying
  the constraints for the optimization, see
  \code{\link{constraint}}}

  \item{permutations}{integer: number of unique constrained
  random portfolios to generate}

  \item{\dots}{any other passthru parameters}
}
\value{
  matrix of random portfolio weights
}
\description{
  repeatedly calls \code{\link{randomize_portfolio}} to
  generate an arbitrary number of constrained random
  portfolios.
}
\author{
  Peter Carl, Brian G. Peterson, (based on an idea by Pat
  Burns)
}
\seealso{
  \code{\link{portfolio.spec}}, \code{\link{objective}},
  \code{\link{randomize_portfolio_v2}}
}

