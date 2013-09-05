\name{chart.Weights.EF}
\alias{chart.Weights.EF}
\alias{chart.Weights.EF.efficient.frontier}
\alias{chart.Weights.EF.optimize.portfolio}
\title{chart weights along an efficient frontier}
\usage{
  chart.Weights.EF(object, ...)
}
\arguments{
  \item{object}{object to chart.}

  \item{\dots}{passthru parameters to \code{barplot}.}

  \item{colorset}{color palette to use.}

  \item{n.portfolios}{number of portfolios to extract along
  the efficient frontier. This is only used for objects of
  class \code{optimize.portfolio}}

  \item{by.groups}{TRUE/FALSE. If TRUE, the weights by
  group are charted.}

  \item{match.col}{match.col string name of column to use
  for risk (horizontal axis). Must match the name of an
  objective.}

  \item{main}{main title used in the plot.}

  \item{cex.lab}{The magnification to be used for x-axis
  and y-axis labels relative to the current setting of
  'cex'.}

  \item{cex.axis}{The magnification to be used for sizing
  the axis text relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{cex.legend}{The magnification to be used for sizing
  the legend relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{legend.labels}{character vector to use for the
  legend labels}

  \item{element.color}{provides the color for drawing
  less-important chart elements, such as the box lines,
  axis lines, etc.}

  \item{legend.loc}{NULL, "topright", "right", or
  "bottomright". If legend.loc is NULL, the legend will not
  be plotted.}
}
\description{
  This creates a stacked column chart of the weights of
  portfolios along an efficient frontier.
}
\author{
  Ross Bennett
}

