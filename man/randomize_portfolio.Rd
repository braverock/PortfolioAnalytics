\name{randomize_portfolio}
\alias{randomize_portfolio}
\title{generate random permutations of a portfolio seed meeting your constraints on the weights of each asset...}
\usage{randomize_portfolio(rpconstraints, max_permutations=200, rounding=3)}
\description{generate random permutations of a portfolio seed meeting your constraints on the weights of each asset}
\value{named weighting vector}
\author{Peter Carl, Brian G. Peterson, (based on an idea by Pat Burns)}
\arguments{\item{rpconstraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{max_permutations}{integer: maximum number of iterations to try for a valid portfolio, default 200}
\item{rounding}{integrer how many decimals should we round to}}
