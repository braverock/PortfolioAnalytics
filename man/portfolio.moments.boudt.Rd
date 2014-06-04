\name{portfolio.moments.boudt}
\alias{portfolio.moments.boudt}
\title{Portfolio Moments}
\usage{
  portfolio.moments.boudt(R, portfolio, momentargs = NULL,
    k = 1, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{portfolio}{an object of type \code{portfolio}
  specifying the constraints and objectives for the
  optimization, see \code{\link{portfolio.spec}}}

  \item{momentargs}{list containing arguments to be passed
  down to lower level functions, default NULL}

  \item{k}{number of factors used for fitting statistical
  factor model}

  \item{\dots}{any other passthru parameters}
}
\description{
  Set portfolio moments for use by lower level optimization
  functions using a statistical factor model based on the
  work of Kris Boudt.
}

