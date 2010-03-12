\name{extractWeights.rebal}
\alias{extractWeights.rebal}
\title{extract time series of weights from output of optimize...}
\usage{extractWeights.rebal(RebalResults, ...)}
\description{extract time series of weights from output of optimize.portfolio}
\details{\code{\link{optimize.portfolio.rebalancing}} outputs a list of
\code{\link{optimize.portfolio}} objects, one for each rebalancing period

The output list is indexed by the dates of the rebalancing periods, as determined by \code{endpoints}}
\seealso{\code{\link{optimize.portfolio.rebalancing}}}
\arguments{\item{RebalResults}{object of type optimize.portfolio.rebalancing to extract weights from}
\item{...}{any other passthru parameters}}
