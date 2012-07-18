\name{Return.level}
\alias{Return.level}
\title{aggregates portfolio returns up to the chosen level from the hierarchy}
\usage{
  Return.level(Rp, wp, h, level = "Sector")
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
  Aggregates returns and weights up to the chosen level
  from the hierarchy. Hierarchy can be used from the
  \code{buildHierarchy} function or defined manually in the
  same way as the \code{buildHierarchy}'s output. If for
  the selected level the values in the hierarchy are
  numeric, the aggregation of returns or weights is
  performed by quintiles. \code{Weight.transform} makes
  transformation of weights to the xts object conformable
  with returns.
}
\examples{
data(attrib)
Return.level(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], h = attrib.hierarchy, level = "MarketCap")
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
  \code{\link{Weight.level}}
}
\keyword{attribution}

