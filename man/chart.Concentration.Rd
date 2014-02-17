\name{chart.Concentration}
\alias{chart.Concentration}
\title{Classic risk reward scatter and concentration}
\usage{
  chart.Concentration(object, ..., return.col = "mean",
    risk.col = "ES", chart.assets = FALSE,
    conc.type = c("weights", "pct_contrib"),
    col = heat.colors(20), element.color = "darkgray",
    cex.axis = 0.8, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{object}{optimal portfolio created by
  \code{\link{optimize.portfolio}}.}

  \item{\dots}{any other passthru parameters.}

  \item{return.col}{string matching the objective of a
  'return' objective, on vertical axis.}

  \item{risk.col}{string matching the objective of a 'risk'
  objective, on horizontal axis.}

  \item{chart.assets}{TRUE/FALSE. Includes a risk reward
  scatter of the assets in the chart.}

  \item{conc.type}{concentration type can be based on the
  concentration of weights or concentration of percentage
  component contribution to risk (only works with risk
  budget objective for the optimization).}

  \item{col}{color palette or vector of colors to use.}

  \item{element.color}{color for the border and axes.}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of
  \code{cex}.}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}.}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}.}
}
\description{
  This function charts the \code{optimize.portfolio} object
  in risk-return space and the degree of concentration
  based on the weights or percentage component contribution
  to risk.
}
\author{
  Peter Carl and Ross Bennett
}
\seealso{
  \code{\link{optimize.portfolio}}
}

