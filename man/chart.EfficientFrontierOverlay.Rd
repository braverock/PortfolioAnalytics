\name{chart.EfficientFrontierOverlay}
\alias{chart.EfficientFrontierOverlay}
\title{Plot multiple efficient frontiers}
\usage{
  chart.EfficientFrontierOverlay(R, portfolio_list, type,
    n.portfolios = 25, match.col = "ES",
    search_size = 2000, main = "Efficient Frontiers",
    cex.axis = 0.8, element.color = "darkgray",
    legend.loc = NULL, legend.labels = NULL,
    cex.legend = 0.8, xlim = NULL, ylim = NULL, ...,
    chart.assets = TRUE, labels.assets = TRUE,
    pch.assets = 21, cex.assets = 0.8, col = NULL,
    lty = NULL, lwd = NULL)
}
\arguments{
  \item{R}{an xts object of asset returns}

  \item{portfolio_list}{list of portfolio objects created
  by \code{\link{portfolio.spec}}}

  \item{type}{type of efficient frontier, see
  \code{\link{create.EfficientFrontier}}}

  \item{n.portfolios}{number of portfolios to extract along
  the efficient frontier. This is only used for objects of
  class \code{optimize.portfolio}}

  \item{match.col}{string name of column to use for risk
  (horizontal axis). Must match the name of an objective.}

  \item{search_size}{passed to optimize.portfolio for
  type="DEoptim" or type="random"}

  \item{main}{title used in the plot.}

  \item{cex.axis}{The magnification to be used for sizing
  the axis text relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{element.color}{provides the color for drawing
  less-important chart elements, such as the box lines,
  axis lines, etc.}

  \item{legend.loc}{location of the legend; NULL,
  "bottomright", "bottom", "bottomleft", "left", "topleft",
  "top", "topright", "right" and "center"}

  \item{legend.labels}{character vector to use for the
  legend labels}

  \item{cex.legend}{The magnification to be used for sizing
  the legend relative to the current setting of 'cex',
  similar to \code{\link{plot}}.}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{...}{passthrough parameters to \code{\link{plot}}}

  \item{chart.assets}{TRUE/FALSE to include the assets}

  \item{labels.assets}{TRUE/FALSE to include the asset
  names in the plot}

  \item{pch.assets}{plotting character of the assets, same
  as in \code{\link{plot}}}

  \item{cex.assets}{A numerical value giving the amount by
  which the asset points and labels should be magnified
  relative to the default.}

  \item{col}{vector of colors with length equal to the
  number of portfolios in \code{portfolio_list}}

  \item{lty}{vector of line types with length equal to the
  number of portfolios in \code{portfolio_list}}

  \item{lwd}{vector of line widths with length equal to the
  number of portfolios in \code{portfolio_list}}
}
\description{
  Overlay the efficient frontiers of multiple portfolio
  objects on a single plot
}
\author{
  Ross Bennett
}

