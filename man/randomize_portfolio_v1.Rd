\name{randomize_portfolio_v1}
\alias{randomize_portfolio_v1}
\title{generate random permutations of a portfolio seed meeting your constraints on the weights of each asset}
\usage{
  randomize_portfolio_v1(rpconstraints,
    max_permutations = 200, rounding = 3)
}
\arguments{
  \item{rpconstraints}{an object of type "constraints"
  specifying the constraints for the optimization, see
  \code{\link{constraint}}}

  \item{max_permutations}{integer: maximum number of
  iterations to try for a valid portfolio, default 200}

  \item{rounding}{integer how many decimals should we round
  to}
}
\value{
  named weighting vector
}
\description{
  generate random permutations of a portfolio seed meeting
  your constraints on the weights of each asset
}
\author{
  Peter Carl, Brian G. Peterson, (based on an idea by Pat
  Burns)
}

