\name{random_portfolios}
\alias{random_portfolios}
\title{generate an arbitary number of constrained random portfolios...}
\usage{random_portfolios(rpconstraints, permutations=100, ...)}
\description{generate an arbitary number of constrained random portfolios}
\details{repeatedly calls \code{\link{randomize_portfolio}} to generate an 
arbitrary number of constrained random portfolios.}
\value{matrix of random portfolio weights}
\seealso{\code{\link{constraint}}, \code{\link{objective}}, \code{\link{randomize_portfolio}}}
\author{Peter Carl, Brian G. Peterson, (based on an idea by Pat Burns)}
\arguments{\item{rpconstraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{permutations}{integer: number of unique constrained random portfolios to generate}
\item{\dots}{any other passthru parameters}}
\examples{rpconstraint<-constraint(assets=10, min_mult=-Inf, max_mult=Inf, min_sum=.99, max_sum=1.01, min=.01, max=.4, weight_seq=generatesequence())
rp<- random_portfolios(rpconstraints=rpconstraint,permutations=1000)
head(rp)}
