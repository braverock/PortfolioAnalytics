\name{Weight.level}
\alias{Weight.level}
\title{aggregates portfolio weights up to the chosen level from the hierarchy}
\usage{
  Weight.level(wp, Rp, h, level = "Sector")
}
\arguments{
  \item{Rp}{xts, data frame or matrix of portfolio returns}

  \item{wp}{vector, xts, data frame or matrix of portfolio
  weights}

  \item{h}{data.frame with portfolio hierarchy}

  \item{level}{level from the hierarchy to which returns
  and weights will be aggregated}
}
\description{
  Aggregates weights up to the chosen level from the
  hierarchy. Hierarchy can be used from the
  \code{buildHierarchy} function or defined manually in the
  same way as the \code{buildHierarchy}'s output. If for
  the selected level the values in the hierarchy are
  numeric, the aggregation of returns or weights is
  performed by quintiles.
}
\examples{
data(attrib)
Weight.level(wp = attrib.weights[1, ], Rp = attrib.returns[, 1:10], attrib.hierarchy, level = "Sector")
}
\author{
  Andrii Babii
}
\references{
  Christopherson, Jon A., Carino, David R., Ferson, Wayne
  E. \emph{Portfolio Performance Measurement and
  Benchmarking}. McGraw-Hill. 2009. Chapter 17
}
\seealso{
  \code{buildHierarchy} \cr \code{\link{Attribution}} \cr
  \code{\link{Return.level}}
}
\keyword{attribution}

