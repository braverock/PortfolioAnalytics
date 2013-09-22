\name{chart.RiskReward}
\alias{chart.RiskReward}
\alias{chart.RiskReward.opt.list}
\alias{chart.RiskReward.optimize.portfolio.DEoptim}
\alias{chart.RiskReward.optimize.portfolio.GenSA}
\alias{chart.RiskReward.optimize.portfolio.pso}
\alias{chart.RiskReward.optimize.portfolio.random}
\alias{chart.RiskReward.optimize.portfolio.ROI}
\title{classic risk reward scatter}
\usage{
  \method{chart.RiskReward}{optimize.portfolio.DEoptim} (object, ..., neighbors = NULL, return.col = "mean",
    risk.col = "ES", chart.assets = FALSE,
    element.color = "darkgray", cex.axis = 0.8,
    xlim = NULL, ylim = NULL)

  \method{chart.RiskReward}{optimize.portfolio.random} (object, ..., neighbors = NULL, return.col = "mean",
    risk.col = "ES", chart.assets = FALSE,
    element.color = "darkgray", cex.axis = 0.8,
    xlim = NULL, ylim = NULL)

  \method{chart.RiskReward}{optimize.portfolio.ROI} (object, ..., neighbors = NULL, return.col = "mean",
    risk.col = "ES", chart.assets = FALSE,
    element.color = "darkgray", cex.axis = 0.8,
    xlim = NULL, ylim = NULL, rp = FALSE)

  \method{chart.RiskReward}{optimize.portfolio.pso} (object, ..., neighbors = NULL, return.col = "mean",
    risk.col = "ES", chart.assets = FALSE,
    element.color = "darkgray", cex.axis = 0.8,
    xlim = NULL, ylim = NULL)

  \method{chart.RiskReward}{optimize.portfolio.GenSA} (object, ..., neighbors = NULL, return.col = "mean",
    risk.col = "ES", chart.assets = FALSE,
    element.color = "darkgray", cex.axis = 0.8,
    ylim = NULL, xlim = NULL, rp = FALSE)

  chart.RiskReward(object, ...)

  \method{chart.RiskReward}{opt.list} (object, ...,
    risk.col = "ES", return.col = "mean", main = "",
    ylim = NULL, xlim = NULL, labels.assets = TRUE,
    pch.assets = 1, cex.assets = 0.8, cex.axis = 0.8,
    cex.lab = 0.8, colorset = NULL,
    element.color = "darkgray")
}
\arguments{
  \item{object}{optimal portfolio created by
  \code{\link{optimize.portfolio}}.}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot, see Details.}

  \item{\dots}{any other passthru parameters.}

  \item{return.col}{string matching the objective of a
  'return' objective, on vertical axis.}

  \item{risk.col}{string matching the objective of a 'risk'
  objective, on horizontal axis.}

  \item{chart.assets}{TRUE/FALSE. Includes a risk reward
  scatter of the assets in the chart.}

  \item{element.color}{color for the default plot scatter
  points.}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of
  \code{cex}.}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}.}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}.}

  \item{rp}{TRUE/FALSE to generate random portfolios to
  plot the feasible space}

  \item{main}{a main title for the plot.}

  \item{labels.assets}{TRUE/FALSE to include the names in
  the plot.}

  \item{pch.assets}{plotting character of the assets, same
  as in \code{\link{plot}}}

  \item{cex.assets}{numerical value giving the amount by
  which the asset points should be magnified relative to
  the default.}

  \item{cex.lab}{numerical value giving the amount by which
  the labels should be magnified relative to the default.}

  \item{colorset}{color palette or vector of colors to
  use.}
}
\description{
  This function charts the \code{optimize.portfolio} object
  in risk-return space.
}
\details{
  \code{neighbors} may be specified in three ways. The
  first is as a single number of neighbors.  This will
  extract the \code{neighbors} closest portfolios in terms
  of the \code{out} numerical statistic. The second method
  consists of a numeric vector for \code{neighbors}. This
  will extract the \code{neighbors} with portfolio index
  numbers that correspond to the vector contents. The third
  method for specifying \code{neighbors} is to pass in a
  matrix. This matrix should look like the output of
  \code{\link{extractStats}}, and should contain
  \code{risk.col},\code{return.col}, and weights columns
  all properly named.
}
\seealso{
  \code{\link{optimize.portfolio}}
}

