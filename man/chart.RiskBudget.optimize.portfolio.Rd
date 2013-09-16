\name{chart.RiskBudget.optimize.portfolio}
\alias{chart.RiskBudget.optimize.portfolio}
\title{Chart risk contribution of an \code{optimize.portfolio} object}
\usage{
  \method{chart.RiskBudget}{optimize.portfolio} (object,
    ..., neighbors = NULL, risk.type = "absolute",
    main = "Risk Contribution", ylab = "", xlab = NULL,
    cex.axis = 0.8, cex.lab = 0.8,
    element.color = "darkgray", las = 3, ylim = NULL)
}
\arguments{
  \item{object}{optimal portfolio object created by
  \code{\link{optimize.portfolio}}}

  \item{neighbors}{risk contribution or pct_contrib of
  neighbor portfolios to be plotted, see details.}

  \item{\dots}{passthrough parameters to
  \code{\link{plot}}.}

  \item{risk.type}{"absolute" or "percentage" to plot risk
  contribution in absolute terms or percentage
  contribution.}

  \item{main}{main title for the chart.}

  \item{ylab}{label for the y-axis.}

  \item{xlab}{label for the x-axis}

  \item{cex.lab}{the magnification to be used for x and y
  labels relative to the current setting of \code{cex}.}

  \item{cex.axis}{the magnification to be used for axis
  annotation relative to the current setting of
  \code{cex}.}

  \item{element.color}{provides the color for drawing
  less-important chart elements, such as the box lines,
  axis lines, etc.}

  \item{las}{numeric in \{0,1,2,3\}; the style of axis
  labels \describe{ \item{0:}{always parallel to the axis,}
  \item{1:}{always horizontal,} \item{2:}{always
  perpendicular to the axis,} \item{3:}{always vertical
  [\emph{default}].} }}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}
}
\description{
  This function charts the contribution or percent
  contribution of the resulting objective measures in
  \code{risk_budget_objectives}.
}
\details{
  \code{neighbors} may be specified in three ways. The
  first is as a single number of neighbors. This will
  extract the \code{neighbors} closest portfolios in terms
  of the \code{out} numerical statistic. The second method
  consists of a numeric vector for \code{neighbors}. This
  will extract the \code{neighbors} with portfolio index
  numbers that correspond to the vector contents. The third
  method for specifying \code{neighbors} is to pass in a
  matrix. This matrix should look like the output of
  \code{\link{extractStats}}, and should contain properly
  named contribution and pct_contrib columns.
}
\author{
  Ross Bennett
}

