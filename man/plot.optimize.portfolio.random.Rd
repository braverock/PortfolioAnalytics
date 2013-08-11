\name{plot.optimize.portfolio.random}
\alias{plot.optimize.portfolio.random}
\title{plot method for optimize.portfolio.random output}
\usage{
  plot.optimize.portfolio.random(x, ..., R = NULL,
    return.col = "mean", risk.col = "ES", neighbors = NULL,
    main = "optimized portfolio plot")
}
\arguments{
  \item{x}{set of portfolios created by
  \code{\link{optimize.portfolio}}}

  \item{...}{any other passthru parameters}

  \item{R}{an optional an xts, vector, matrix, data frame,
  timeSeries or zoo}

  \item{risk.col}{string name of column to use for risk
  (horizontal axis)}

  \item{return.col}{string name of column to use for
  returns (vertical axis)}

  \item{neighbors}{set of 'neighbor portfolios to overplot}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}
}
\description{
  scatter and weights chart for random portfolios
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

