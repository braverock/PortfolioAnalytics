\name{set.portfolio.moments}
\alias{set.portfolio.moments}
\title{set portfolio moments for use by lower level optimization functions...}
\usage{set.portfolio.moments(R, constraints, momentargs, ...)}
\description{set portfolio moments for use by lower level optimization functions}
\arguments{\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}
\item{constraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{momentargs}{list containing arguments to be passed down to lower level functions, default NULL}
\item{\dots}{any other passthru parameters}}
