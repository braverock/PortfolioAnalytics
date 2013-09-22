\name{chart.RiskBudget}
\alias{chart.RiskBudget}
\alias{chart.RiskBudget.opt.list}
\alias{chart.RiskBudget.optimize.portfolio}
\title{Generic method to chart risk contribution}
\usage{
  chart.RiskBudget(object, ...)

  \method{chart.RiskBudget}{optimize.portfolio} (object,
    ..., neighbors = NULL, risk.type = "absolute",
    main = "Risk Contribution", ylab = "", xlab = NULL,
    cex.axis = 0.8, cex.lab = 0.8,
    element.color = "darkgray", las = 3, ylim = NULL)

  \method{chart.RiskBudget}{opt.list} (object, ...,
    match.col = "ES", risk.type = "absolute",
    main = "Risk Budget", plot.type = "line",
    cex.axis = 0.8, cex.lab = 0.8,
    element.color = "darkgray", las = 3, ylim = NULL,
    colorset = NULL, legend.loc = NULL, cex.legend = 0.8)
}
\arguments{
  \item{object}{optimal portfolio object created by
  \code{\link{optimize.portfolio}}}

  \item{\dots}{any other passthru parameters to
  \code{\link{plot}}}

  \item{neighbors}{risk contribution or pct_contrib of
  neighbor portfolios to be plotted, see Details.}

  \item{match.col}{string of risk column to match. The
  \code{opt.list} object may contain risk budgets for ES or
  StdDev and this will match the proper column names of the
  objectives list outp (e.g. ES.contribution).}

  \item{risk.type}{"absolute" or "percentage" to plot risk
  contribution in absolute terms or percentage
  contribution.}

  \item{main}{main title for the chart.}

  \item{plot.type}{"line" or "barplot".}

  \item{ylab}{label for the y-axis.}

  \item{xlab}{label for the x-axis.}

  \item{cex.axis}{the magnification to be used for axis
  annotation relative to the current setting of
  \code{cex}.}

  \item{cex.lab}{the magnification to be used for axis
  annotation relative to the current setting of
  \code{cex}.}

  \item{element.color}{provides the color for drawing
  less-important chart elements, such as the box lines,
  axis lines, etc.}

  \item{las}{numeric in \{0,1,2,3\}; the style of axis
  labels \describe{ \item{0:}{always parallel to the axis
  [\emph{default}],} \item{1:}{always horizontal,}
  \item{2:}{always perpendicular to the axis,}
  \item{3:}{always vertical.} }}

  \item{ylim}{set the y-axis limit, same as in
  \code{\link{plot}}}

  \item{colorset}{color palette or vector of colors to use}

  \item{legend.loc}{legend.loc NULL, "topright", "right",
  or "bottomright". If legend.loc is NULL, the legend will
  not be plotted}

  \item{cex.legend}{The magnification to be used for the
  legend relative to the current setting of \code{cex}}
}
\description{
  This function is the generic method to chart risk budget
  objectives for \code{optimize.portfolio} and
  \code{opt.list} objects. This function charts the
  contribution or percent contribution of the resulting
  objective measures of a \code{risk_budget_objective}.
}
\details{
  \code{neighbors} may be specified in three ways. The
  first is as a single number of neighbors. This will
  extract the \code{neighbors} closest to the portfolios in
  terms of the \code{out} numerical statistic. The second
  method consists of a numeric vector for \code{neighbors}.
  This will extract the \code{neighbors} with portfolio
  index numbers that correspond to the vector contents. The
  third method for specifying \code{neighbors} is to pass
  in a matrix. This matrix should look like the output of
  \code{\link{extractStats}}, and should contain properly
  named contribution and pct_contrib columns.
}

