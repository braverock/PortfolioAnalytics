\name{chart.Scatter.DE}
\alias{chart.Scatter.DE}
\title{classic risk return scatter of DEoptim results...}
\usage{chart.Scatter.DE(DE, R, constraints, neighbors, return.col="mean",
    risk.col="ES", ..., element.color="darkgray", cex.axis=0.8)}
\description{classic risk return scatter of DEoptim results}
\seealso{\code{\link{optimize.portfolio}}}
\arguments{\item{DE}{set of portfolios created by \code{\link{optimize.portfolio}}}
\item{R}{an optional an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns, used to recalulate the objective function where required}
\item{constraints}{an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}}
\item{neighbors}{set of 'neighbor' portfolios to overplot, see Details in \code{\link{charts.DE}}}
\item{return.col}{string matching the objective of a 'return' objective, on vertical axis}
\item{risk.col}{string matching the objective of a 'risk' objective, on horizontal axis}
\item{...}{any other passthru parameters}
\item{cex.axis}{The magnification to be used for axis annotation relative to the current setting of \code{cex}}
\item{element.color}{color for the default plot scatter points}}
