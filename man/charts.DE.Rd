\name{charts.DE}
\alias{charts.DE}
\title{scatter and weights chart  for random portfolios}
\usage{
  charts.DE(DE, risk.col, return.col, chart.assets,
    neighbors = NULL, main = "DEoptim.Portfolios",
    xlim = NULL, ylim = NULL, ...)
}
\arguments{
  \item{DE}{set of random portfolios created by
  \code{\link{optimize.portfolio}}}

  \item{...}{any other passthru parameters}

  \item{risk.col}{string name of column to use for risk
  (horizontal axis)}

  \item{return.col}{string name of column to use for
  returns (vertical axis)}

  \item{neighbors}{set of 'neighbor portfolios to overplot}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}
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
  \code{\link{extractStats}}
}

