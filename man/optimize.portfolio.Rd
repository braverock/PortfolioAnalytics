\name{optimize.portfolio}
\alias{optimize.portfolio}
\title{wrapper for constrained optimization of portfolios...}
\usage{optimize.portfolio(R, constraints, optimize_method=c("DEoptim", "random"), search_size=20000, trace=FALSE, ..., rp)}
\description{wrapper for constrained optimization of portfolios}
\details{This function aims to provide a wrapper for constrained optimization of 
portfolios that allows the user to specify box constraints and business 
objectives.

This function currently supports DEoptim and random portfolios as back ends.
Additional back end contributions for Rmetrics, ghyp, etc. would be welcome.}
\value{a list containing the optimal weights, some summary statistics, the function call, and optionally trace information}
\author{Kris Boudt, Peter Carl, Brian G. Peterson}
\arguments{\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}
\item{constraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{optimize_method}{one of "DEoptim" or "random"}
\item{search_size}{integer, how many portfolios to test, default 20,000}
\item{trace}{TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched}
\item{\dots}{any other passthru parameters}
\item{rp}{matrix of random portfolio weights, default NULL, mostly for automated use by rebalancing optimization or repeated tests on same portfolios}}
