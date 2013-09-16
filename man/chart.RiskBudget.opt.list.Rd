\name{chart.RiskBudget.opt.list}
\alias{chart.RiskBudget.opt.list}
\title{Chart risk contribution of an \code{opt.list} object}
\usage{
  \method{chart.RiskBudget}{opt.list} (object, ...,
    match.col = "ES", risk.type = "absolute",
    main = "Risk Budget", plot.type = "line",
    cex.axis = 0.8, cex.lab = 0.8,
    element.color = "darkgray", las = 3, ylim = NULL,
    colorset = NULL, legend.loc = NULL, cex.legend = 0.8)
}
\arguments{
  \item{object}{list of optimal portfolio objects created
  by \code{\link{optimizations.combine}}.}

  \item{\dots}{any other passthru parameter.}

  \item{match.col}{string of risk column to match. The
  \code{opt.list} object may contain risk budgets for ES or
  StdDev and this will match the proper column names of the
  objectives list outp (e.g. ES.contribution).}

  \item{risk.type}{"absolute" or "percentage" to plot risk
  contribution in absolute terms or percentage
  contribution.}

  \item{main}{main title for the chart.}

  \item{plot.type}{"line" or "barplot".}

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
  This function charts the absolute contribution or percent
  contribution of the resulting objective measures in the
  \code{opt.list} object.
}
\author{
  Ross Bennett
}

