\name{chart.Scatter.RP}
\alias{chart.Scatter.RP}
\title{classic risk return scatter of random portfolios...}
\usage{chart.Scatter.RP(RP, neighbors=NA, return.col="mean", risk.col="ES", element.color="darkgray", cex.axis=0.8, ...)}
\description{classic risk return scatter of random portfolios}
\seealso{\code{\link{optimize.portfolio}}}
\arguments{\item{RP}{set of random portfolios created by \code{\link{optimize.portfolio}}}
\item{neighbors}{set of 'neighbor portfolios to overplot}
\item{return.col}{string matching the objective of a 'return' objective, on vertical axis}
\item{risk.col}{string matching the objective of a 'risk' objective, on horizontal axis}
\item{...}{any other passthru parameters}}
