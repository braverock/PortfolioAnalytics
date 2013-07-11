\name{randomize_portfolio_v2}
\alias{randomize_portfolio_v2}
\title{version 2 generate random permutations of a portfolio seed meeting your constraints on the weights of each asset}
\usage{
  randomize_portfolio_v2(portfolio, max_permutations = 200)
}
\arguments{
  \item{portfolio}{an object of type "portfolio" specifying
  the constraints for the optimization, see
  \code{\link{portfolio.spec}}}

  \item{max_permutations}{integer: maximum number of
  iterations to try for a valid portfolio, default 200}

  \item{rounding}{integer how many decimals should we round
  to}
}
\value{
  named weighting vector
}
\description{
  version 2 generate random permutations of a portfolio
  seed meeting your constraints on the weights of each
  asset
}
\author{
  Peter Carl, Brian G. Peterson, (based on an idea by Pat
  Burns)
}

