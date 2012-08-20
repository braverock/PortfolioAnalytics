\name{extractStats.optimize.portfolio.ROI}
\alias{extractStats.optimize.portfolio.ROI}
\title{extract some stats from a portfolio list run with ROI via
\code{\link{optimize.portfolio}}}
\usage{
  extractStats.optimize.portfolio.ROI(object,
    prefix = NULL, ...)
}
\arguments{
  \item{object}{list returned by optimize.portfolio}

  \item{prefix}{prefix to add to output row names}

  \item{...}{any other passthru parameters}
}
\description{
  This function will take everything in the
  objective_measures slot and \code{unlist} it. This may
  produce a very large number of columns or strange column
  names.
}

