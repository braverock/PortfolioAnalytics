\name{chart.Scatter.RP}
\alias{chart.Scatter.RP}
\title{classic risk return scatter of random portfolios}
\usage{
  chart.Scatter.RP(RP, R = NULL, neighbors = NULL,
    return.col = "mean", risk.col = "ES", ...,
    element.color = "darkgray", cex.axis = 0.8)
}
\arguments{
  \item{RP}{set of portfolios created by
  \code{\link{optimize.portfolio}}}

  \item{R}{an optional an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns, used to
  recalulate the objective function when return.col or
  risk.col is not part of the extractStats output.}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot, see Details}

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
  classic risk return scatter of random portfolios
}
\seealso{
  \code{\link{optimize.portfolio}}
}

