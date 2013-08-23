\name{chart.Weights.EF}
\alias{chart.Weights.EF}
\title{chart the weights along the efficient frontier}
\usage{
  chart.Weights.EF(object, colorset = NULL, ...,
    match.col = "ES", main = "EF Weights", las = 1,
    cex.lab = 0.8, cex.axis = 0.8, cex.legend = 0.8,
    legend.loc = "under", legend.labels = NULL,
    element.color = "darkgray")
}
\arguments{
  \item{object}{object of class 'efficient.frontier'
  created by \code{\link{create.EfficientFrontier}}.}

  \item{colorset}{color palette to use.}

  \item{...}{passthrough parameters to
  \code{chart.StackedBar}.}

  \item{match.col}{match.col string name of column to use
  for risk (horizontal axis). Must match the name of an
  objective.}

  \item{main}{main title used in the plot.}

  \item{las}{sets the orientation of the axis labels, as
  described in \code{\link{par}}.}

  \item{cex.lab}{The magnification to be used for x- and
  y-axis labels relative to the current setting of 'cex'.}

  \item{cex.axis}{The magnification to be used for sizing
  the axis text relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{cex.legend}{The magnification to be used for sizing
  the legend relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{legend.loc}{places a legend into a location on the
  chart similar to \code{\link{chart.TimeSeries}}. The
  default, "under," is the only location currently
  implemented for this chart. Use 'NULL' to remove the
  legend.}

  \item{legend.labels}{character vector to use for the
  legend labels}

  \item{element.color}{provides the color for drawing
  less-important chart elements, such as the box lines,
  axis lines, etc.}
}
\description{
  This creates a stacked column chart of the weights of
  portfolios along the efficient frontier.
}
\author{
  Ross Bennett
}

