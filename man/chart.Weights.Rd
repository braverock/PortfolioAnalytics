\name{chart.Weights}
\alias{chart.Weights}
\alias{chart.Weights.opt.list}
\alias{chart.Weights.optimize.portfolio.DEoptim}
\alias{chart.Weights.optimize.portfolio.GenSA}
\alias{chart.Weights.optimize.portfolio.pso}
\alias{chart.Weights.optimize.portfolio.ROI}
\alias{chart.Weights.optimize.portfolio.RP}
\title{boxplot of the weights of the optimal portfolios}
\usage{
  chart.Weights(object, neighbors = NULL, ...,
    main = "Weights", las = 3, xlab = NULL, cex.lab = 1,
    element.color = "darkgray", cex.axis = 0.8)
}
\arguments{
  \item{object}{optimal portfolio object created by
  \code{\link{optimize.portfolio}}}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot}

  \item{\dots}{any other passthru parameters}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}

  \item{las}{numeric in \{0,1,2,3\}; the style of axis
  labels \describe{ \item{0:}{always parallel to the axis
  [\emph{default}],} \item{1:}{always horizontal,}
  \item{2:}{always perpendicular to the axis,}
  \item{3:}{always vertical.} }}

  \item{xlab}{a title for the x axis: see
  \code{\link{title}}}

  \item{cex.lab}{The magnification to be used for x and y
  labels relative to the current setting of \code{cex}}

  \item{element.color}{color for the default plot lines}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of \code{cex}}

  \item{colorset}{color palette or vector of colors to use}

  \item{legend.loc}{location of the legend. If NULL, the
  legend will not be plotted}

  \item{plot.type}{"line" or "barplot"}
}
\description{
  Chart the optimal weights and upper and lower bounds on
  weights of a portfolio run via
  \code{\link{optimize.portfolio}}
}
\seealso{
  \code{\link{optimize.portfolio}}
}

