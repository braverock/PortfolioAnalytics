\name{chart.Efficient.Frontier.optimize.portfolio}
\alias{chart.Efficient.Frontier.optimize.portfolio}
\title{chart the efficient frontier and risk-reward scatter plot of the assets}
\usage{
  chart.Efficient.Frontier.optimize.portfolio(object,
    match.col = "ES", n.portfolios = 25, xlim = NULL,
    ylim = NULL, cex.axis = 0.8,
    element.color = "darkgray",
    main = "Efficient Frontier", ...)
}
\arguments{
  \item{object}{optimal portfolio created by
  \code{\link{optimize.portfolio}}}

  \item{match.col}{string name of column to use for risk
  (horizontal axis). \code{match.col} must match the name
  of an objective in the \code{portfolio} object.}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{main}{a main title for the plot}

  \item{...}{passthrough parameters to \code{\link{plot}}}
}
\description{
  This function charts the efficient frontier and
  risk-reward scatter plot of the assets. The efficient
  frontier plotted is based on the the trace information
  (sets of portfolios tested by the solver at each
  iteration) in objects created by
  \code{optimize.portfolio}. When running
  \code{optimize.portfolio}, \code{trace=TRUE} must be
  specified.
}

