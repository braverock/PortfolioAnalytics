\name{chart.Weights.GenSA}
\alias{chart.Weights.GenSA}
\title{boxplot of the weights in the portfolio}
\usage{
  chart.Weights.GenSA(GenSA, neighbors = NULL, ...,
    main = "Weights", las = 3, xlab = NULL, cex.lab = 1,
    element.color = "darkgray", cex.axis = 0.8)
}
\arguments{
  \item{GenSA}{object created by
  \code{\link{optimize.portfolio}}}

  \item{neighbors}{set of 'neighbor' portfolios to
  overplot}

  \item{las}{numeric in \{0,1,2,3\}; the style of axis
  labels \describe{ \item{0:}{always parallel to the axis
  [\emph{default}],} \item{1:}{always horizontal,}
  \item{2:}{always perpendicular to the axis,}
  \item{3:}{always vertical.} }}

  \item{xlab}{a title for the x axis: see
  \code{\link{title}}}

  \item{cex.lab}{The magnification to be used for x and y
  labels relative to the current setting of \code{cex}}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of \code{cex}}

  \item{element.color}{color for the default plot lines}

  \item{...}{any other passthru parameters}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}
}
\description{
  boxplot of the weights in the portfolio
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{optimize.portfolio}}
}

