\name{meanetl.efficient.frontier}
\alias{meanetl.efficient.frontier}
\title{Generate the efficient frontier for a mean-etl portfolio}
\usage{
  meanetl.efficient.frontier(portfolio, R,
    n.portfolios = 25, ...)
}
\arguments{
  \item{portfolio}{a portfolio object with constraints and
  objectives created via \code{\link{portfolio.spec}}}

  \item{R}{an xts or matrix of asset returns}

  \item{n.portfolios}{number of portfolios to generate the
  efficient frontier}

  \item{\dots}{passthru parameters to
  \code{\link{optimize.portfolio}}}
}
\value{
  a matrix of objective measure values and weights along
  the efficient frontier
}
\description{
  This function generates the mean-ETL efficient frontier
  of a portfolio specifying constraints and objectives. To
  generate the mean-ETL efficient frontier, the portfolio
  must have two objectives 1) "mean" and 2) "ETL/ES/CVaR".
  If the only objective in the \code{portfolio} object is
  ETL/ES/CVaR, the we will add a mean objective.
}
\author{
  Ross Bennett
}

