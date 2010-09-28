\name{optimize.portfolio.parallel}
\alias{optimize.portfolio.parallel}
\title{multiple optimize...}
\usage{optimize.portfolio.parallel(R, constraints,
    optimize_method=c("DEoptim", "random"), search_size=20000,
    trace=FALSE, ..., nodes=4)}
\description{multiple optimize.portfolio calls, presumably in parallel}
\details{TODO write function to check sensitivity of optimal results by using optimize.portfolio.parallel results}
\value{a list containing the optimal weights, some summary statistics, the function call, and optionally trace information}
\author{Kris Boudt, Peter Carl, Brian G. Peterson}
\arguments{\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}
\item{constraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{optimize_method}{one of "DEoptim" or "random"}
\item{search_size}{integer, how many portfolios to test, default 20,000}
\item{trace}{TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched}
\item{\dots}{any other passthru parameters}
\item{nodes}{how many processes to run in the foreach loop, default 4}}
