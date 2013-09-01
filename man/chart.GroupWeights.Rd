\name{chart.GroupWeights}
\alias{chart.GroupWeights}
\title{Chart weights by group or category}
\usage{
  chart.GroupWeights(object, ...,
    grouping = c("groups", "category"),
    main = "Group Weights", las = 3, xlab = NULL,
    cex.lab = 1, element.color = "darkgray",
    cex.axis = 0.8)
}
\arguments{
  \item{object}{object of class \code{optimize.portfolio}}

  \item{...}{passthrough parameters to \code{\link{plot}}}

  \item{grouping}{\itemize{ \item{groups: }{group the
  weights group constraints} \item{category_labels: }{group
  the weights by category_labels in portfolio object} }}

  \item{main}{an overall title for the plot: see
  \code{\link{title}}}

  \item{las}{numeric in \{0,1,2,3\}; the style of axis
  labels \describe{ \item{0:}{always parallel to the axis
  [\emph{default}],} \item{1:}{always horizontal,}
  \item{2:}{always perpendicular to the axis,}
  \item{3:}{always vertical.} }}

  \item{xlab}{a title for the x axis: see
  \code{\link{title}}}

  \item{cex.lab}{The magnification to be used for x and y
  labels relative to the current setting of \code{cex}}

  \item{element.color}{color for the default border and
  axis}

  \item{cex.axis}{The magnification to be used for x and y
  axis relative to the current setting of \code{cex}}
}
\description{
  Chart weights by group or category
}
\author{
  Ross Bennett
}

