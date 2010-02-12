\name{optimize.portfolio.rebalancing}
\alias{optimize.portfolio.rebalancing}
\title{portfolio optimization with support for rebalancing or rolling periods...}
\usage{optimize.portfolio.rebalancing(R, constraints, optimize_method=c("DEoptim", "random"), search_size=20000, trace=FALSE, ..., rebalance_on,
    training_period, trailing_periods)}
\description{portfolio optimization with support for rebalancing or rolling periods}
\details{This function may eventually be wrapped into optimize.portfolio

For now, we'll set the rebalancing periods here, though I think they should eventually be part of the constraints object

This function is massively parallel, and will require 'foreach' and we suggest that you register a parallel backend.}
\value{a list containing the optimal weights, some summary statistics, the function call, and optionally trace information}
\author{Kris Boudt, Peter Carl, Brian G. Peterson}
\arguments{\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}
\item{constraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{optimize_method}{one of "DEoptim" or "random"}
\item{search_size}{integer, how many portfolios to test, default 20,000}
\item{trace}{TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched}
\item{\dots}{any other passthru parameters}
\item{rebalance_on}{a periodicity as returned by xts function periodicity and usable by endpoints}
\item{training_period}{period to use as training in the front of the data}
\item{trailing_periods}{if set, an integer with the number of periods to roll over, default NULL will run from inception}}
