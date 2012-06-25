\name{extractStats.optimize.portfolio.parallel}
\alias{extractStats.optimize.portfolio.parallel}
\title{extract some stats from a portfolio list run via foreach in optimize.portfolio.parallel}
\usage{
  extractStats.optimize.portfolio.parallel(object,
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
\seealso{
  \code{\link{optimize.portfolio}}
  \code{\link{optimize.portfolio.parallel}}
  \code{\link{extractStats}}
}

