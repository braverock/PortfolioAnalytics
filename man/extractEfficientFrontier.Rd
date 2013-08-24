\name{extractEfficientFrontier}
\alias{extractEfficientFrontier}
\title{Extract the efficient frontier data points}
\usage{
  extractEfficientFrontier(object, match.col = "ES",
    n.portfolios = 25)
}
\arguments{
  \item{object}{an optimal portfolio object created by
  \code{optimize.portfolio}}

  \item{match.col}{string name of column to use for risk
  (horizontal axis). \code{match.col} must match the name
  of an objective in the \code{portfolio} object.}

  \item{n.portfolios}{number of portfolios to use to plot
  the efficient frontier}
}
\value{
  an \code{efficient.frontier} object with weights and
  other metrics along the efficient frontier
}
\description{
  This function extracts the efficient frontier from an
  object created by \code{\link{optimize.portfolio}}.
}
\details{
  If the object is an \code{optimize.portfolio.ROI} object
  and \code{match.col} is "ES", "ETL", or "CVaR", then the
  mean-ETL efficient frontier will be created via
  \code{meanetl.efficient.frontier}.

  If the object is an \code{optimize.portfolio.ROI} object
  and \code{match.col} is "var", then the mean-var
  efficient frontier will be created via
  \code{meanvar.efficient.frontier}.

  For objects created by \code{optimize.portfolo} with the
  DEoptim, random, or pso solvers, the efficient frontier
  will be extracted from the object via
  \code{extract.efficient.frontier}. This means that
  \code{optimize.portfolio} must be run with
  \code{trace=TRUE}
}
\author{
  Ross Bennett
}

