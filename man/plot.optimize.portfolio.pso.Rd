\name{plot.optimize.portfolio.pso}
\alias{plot.optimize.portfolio.pso}
\title{scatter and weights chart for portfolios}
\usage{
  plot.optimize.portfolio.pso(x, ..., return.col = "mean",
    risk.col = "ES", chart.assets = FALSE, cex.axis = 0.8,
    element.color = "darkgray", neighbors = NULL,
    main = "PSO.Portfolios", xlim = NULL, ylim = NULL)
}
\arguments{
  \item{pso}{object created by
  \code{\link{optimize.portfolio}}}

  \item{...}{any other passthru parameters}

  \item{return.col}{string matching the objective of a
  'return' objective, on vertical axis}

  \item{risk.col}{string matching the objective of a 'risk'
  objective, on horizontal axis}

  \item{chart.assets}{TRUE/FALSE to include risk-return
  scatter of assets}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of \code{cex}}

  \item{element.color}{color for the default plot scatter
  points}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}

  \item{xlim}{set the limit on coordinates for the x-axis}

  \item{ylim}{set the limit on coordinates for the y-axis}
}
\description{
  \code{return.col} must be the name of a function used to
  compute the return metric on the random portfolio weights
  \code{risk.col} must be the name of a function used to
  compute the risk metric on the random portfolio weights
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{optimize.portfolio}}
}

