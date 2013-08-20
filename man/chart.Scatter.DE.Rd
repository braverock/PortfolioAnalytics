\name{chart.Scatter.DE}
\alias{chart.Scatter.DE}
\title{classic risk return scatter of DEoptim results}
\usage{
  chart.Scatter.DE(DE, neighbors = NULL, ...,
    return.col = "mean", risk.col = "ES",
    element.color = "darkgray", cex.axis = 0.8)
}
\arguments{
  \item{DE}{set of portfolios created by
  \code{\link{optimize.portfolio}}}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot, see Details in \code{\link{charts.DE}}}

  \item{return.col}{string matching the objective of a
  'return' objective, on vertical axis}

  \item{risk.col}{string matching the objective of a 'risk'
  objective, on horizontal axis}

  \item{...}{any other passthru parameters}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of \code{cex}}

  \item{element.color}{color for the default plot scatter
  points}
}
\description{
  classic risk return scatter of DEoptim results
}
\seealso{
  \code{\link{optimize.portfolio}}
}

