\name{extract.efficient.frontier}
\alias{extract.efficient.frontier}
\title{Extract the efficient frontier of portfolios that meet your objectives over a range of risks}
\usage{
  extract.efficient.frontier(object = NULL,
    match.col = "ES", from = 0, to = 1, by = 0.005, ...,
    R = NULL, portfolio = NULL, optimize_method = "random")
}
\arguments{
  \item{object}{optimial portfolio object as created by
  \code{\link{optimize.portfolio}}}

  \item{from}{minimum value of the sequence}

  \item{to}{maximum value of the sequence}

  \item{by}{number to increment the sequence by}

  \item{match.col}{string name of column to use for risk
  (horizontal axis)}

  \item{\dots}{any other passthru parameters to
  \code{optimize.portfolio}}

  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{portfolio}{an object of type "portfolio" specifying
  the constraints and objectives for the optimization, see
  \code{\link{portfolio.spec}}}

  \item{optimize_method}{one of "DEoptim", "random", "ROI",
  "pso", or "GenSA"}
}
\description{
  The efficient frontier is extracted from the set of
  portfolios created by \code{optimize.portfolio} with
  \code{trace=TRUE}.
}
\details{
  If you do not have an optimal portfolio object created by
  \code{\link{optimize.portfolio}}, you can pass in a
  portfolio object and an optimization will be run via
  \code{\link{optimize.portfolio}}
}
\note{
  Note that this function will be extremely sensitive to
  the objectives in your \code{\link{portfolio}} object.
  It will be especially obvious if you are looking at a
  risk budget objective and your return preference is not
  set high enough.
}

