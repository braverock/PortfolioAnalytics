\name{extractStats}
\alias{extractStats}
\title{extract some stats and weights from a portfolio run via optimize...}
\usage{extractStats(object, prefix, ...)}
\description{extract some stats and weights from a portfolio run via \code{optimize.portfolio}}
\details{This function will dispatch to the appropriate class handler based on the
input class of the optimize.portfolio output object}
\seealso{\code{\link{optimize.portfolio}}}
\arguments{\item{object}{list returned by optimize.portfolio}
\item{prefix}{prefix to add to output row names}
\item{...}{any other passthru parameters}}
