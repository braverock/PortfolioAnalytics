\name{plot.optimize.portfolio}
\alias{plot.optimize.portfolio}
\title{plot method for optimize.portfolio output}
\usage{
  \method{plot}{optimize.portfolio} (x, ...,
    return.col = "mean", risk.col = "ES",
    chart.assets = FALSE, neighbors = NULL, xlim = NULL,
    ylim = NULL, main = "optimized portfolio plot")
}
\arguments{
  \item{x}{set of portfolios created by
  \code{\link{optimize.portfolio}}}

  \item{...}{any other passthru parameters}

  \item{return.col}{string name of column to use for
  returns (vertical axis)}

  \item{risk.col}{string name of column to use for risk
  (horizontal axis)}

  \item{chart.assets}{TRUE/FALSE to include risk-return
  scatter of assets}

  \item{neighbors}{set of 'neighbor portfolios to overplot}

  \item{xlim}{set the limit on coordinates for the x-axis}

  \item{ylim}{set the limit on coordinates for the y-axis}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}
}
\description{
  Scatter and weights chart for portfolio optimization
}
\details{
  This is a fallback that will be called for classes of
  portfolio that do not have specific pre-existing plot
  methods.

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

