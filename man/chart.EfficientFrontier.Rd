\name{chart.EfficientFrontier}
\alias{chart.EfficientFrontier}
\alias{chart.EfficientFrontier.efficient.frontier}
\alias{chart.EfficientFrontier.optimize.portfolio}
\alias{chart.EfficientFrontier.optimize.portfolio.ROI}
\title{chart the efficient frontier and risk-return scatter plot of the assets}
\usage{
  chart.EfficientFrontier(object, match.col = "ES",
    n.portfolios = 25, xlim = NULL, ylim = NULL,
    cex.axis = 0.8, element.color = "darkgray",
    main = "Efficient Frontier", ...)

  chart.EfficientFrontier.optimize.portfolio.ROI(object,
    match.col = "ES", n.portfolios = 25, xlim = NULL,
    ylim = NULL, cex.axis = 0.8,
    element.color = "darkgray",
    main = "Efficient Frontier", ...)

  chart.EfficientFrontier.optimize.portfolio(object,
    match.col = "ES", n.portfolios = 25, xlim = NULL,
    ylim = NULL, cex.axis = 0.8,
    element.color = "darkgray",
    main = "Efficient Frontier", ...)

  chart.EfficientFrontier.efficient.frontier(object,
    chart.assets = TRUE, match.col = "ES",
    n.portfolios = NULL, xlim = NULL, ylim = NULL,
    cex.axis = 0.8, element.color = "darkgray",
    main = "Efficient Frontier", ...)
}
\arguments{
  \item{object}{optimal portfolio created by
  \code{\link{optimize.portfolio}}}

  \item{string}{name of column to use for risk (horizontal
  axis). \code{match.col} must match the name of an
  objective measure in the \code{objective_measures} or
  \code{opt_values} slot in the object created by
  \code{\link{optimize.portfolio}}.}

  \item{n.portfolios}{number of portfolios to use to plot
  the efficient frontier}

  \item{xlim}{set the x-axis limit, same as in
  \code{\link{plot}}}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{cex.axis}{}

  \item{element.color}{}

  \item{main}{a main title for the plot}

  \item{...}{passthrough parameters to \code{\link{plot}}}
}
\description{
  This function charts the efficient frontier and
  risk-return scatter plot of the assets given an object
  created by \code{optimize.portfolio}.
}
\details{
  For objects created by optimize.portfolio with 'DEoptim',
  'random', or 'pso' specified as the optimize_method:
  \itemize{ \item The efficient frontier plotted is based
  on the the trace information (sets of portfolios tested
  by the solver at each iteration) in objects created by
  \code{optimize.portfolio}. }

  For objects created by optimize.portfolio with 'ROI'
  specified as the optimize_method: \itemize{ \item The
  mean-StdDev or mean-etl efficient frontier can be plotted
  for optimal portfolio objects created by
  \code{optimize.portfolio}.

  \item If \code{match.col="StdDev"}, the mean-StdDev
  efficient frontier is plotted.

  \item If \code{match.col="ETL"} (also "ES" or "CVaR"),
  the mean-etl efficient frontier is plotted. }

  Note that \code{trace=TRUE} must be specified in
  \code{\link{optimize.portfolio}}

  GenSA does not return any useable trace information for
  portfolios tested at each iteration, therfore we cannot
  extract and chart an efficient frontier.
}
\author{
  Ross Bennett
}

