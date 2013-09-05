\name{chart.RiskReward}
\alias{chart.RiskReward}
\alias{chart.RiskReward.optimize.portfolio.DEoptim}
\alias{chart.RiskReward.optimize.portfolio.GenSA}
\alias{chart.RiskReward.optimize.portfolio.pso}
\alias{chart.RiskReward.optimize.portfolio.ROI}
\alias{chart.RiskReward.optimize.portfolio.RP}
\title{classic risk reward scatter}
\usage{
  chart.RiskReward(object, neighbors, ..., return.col,
    risk.col, chart.assets, element.color, cex.axis, xlim,
    ylim)
}
\arguments{
  \item{object}{optimal portfolio created by
  \code{\link{optimize.portfolio}}}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot, see Details}

  \item{\dots}{any other passthru parameters}

  \item{return.col}{string matching the objective of a
  'return' objective, on vertical axis}

  \item{risk.col}{string matching the objective of a 'risk'
  objective, on horizontal axis}

  \item{chart.assets}{TRUE/FALSE. Includes a risk reward
  scatter of the assets in the chart}

  \item{element.color}{color for the default plot scatter
  points}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of \code{cex}}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}
}
\description{
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

