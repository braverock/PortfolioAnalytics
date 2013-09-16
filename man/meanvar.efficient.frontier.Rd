\name{meanvar.efficient.frontier}
\alias{meanvar.efficient.frontier}
\title{Generate the efficient frontier for a mean-variance portfolio}
\usage{
  meanvar.efficient.frontier(portfolio, R,
    n.portfolios = 25, risk_aversion = NULL)
}
\arguments{
  \item{portfolio}{a portfolio object with constraints and
  objectives created via \code{\link{portfolio.spec}}}

  \item{R}{an xts or matrix of asset returns}

  \item{n.portfolios}{number of portfolios to plot along
  the efficient frontier}

  \item{risk_aversion}{vector of risk_aversion values to
  construct the efficient frontier. \code{n.portfolios} is
  ignored if \code{risk_aversion} is specified and the
  number of points along the efficient frontier is equal to
  the length of \code{risk_aversion}.}
}
\value{
  a matrix of objective measure values and weights along
  the efficient frontier
}
\description{
  This function generates the mean-variance efficient
  frontier of a portfolio specifying constraints and
  objectives. To generate the mean-var efficient frontier,
  the portfolio must have two objectives 1) "mean" and 2)
  "var".
}
\author{
  Ross Bennett
}

