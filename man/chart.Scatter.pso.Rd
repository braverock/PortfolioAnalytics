\name{chart.Scatter.pso}
\alias{chart.Scatter.pso}
\title{classic risk return scatter of random portfolios}
\usage{
  chart.Scatter.pso(pso, return.col = "mean",
    risk.col = "StdDev", ..., element.color = "darkgray",
    cex.axis = 0.8, main = "")
}
\arguments{
  \item{pso}{object created by
  \code{\link{optimize.portfolio}}}

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
  \code{return.col} must be the name of a function used to
  compute the return metric on the portfolio weights
  \code{risk.col} must be the name of a function used to
  compute the risk metric on the portfolio weights
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{optimize.portfolio}}
}

