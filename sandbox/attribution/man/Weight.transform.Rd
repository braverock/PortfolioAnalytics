\name{Weight.transform}
\alias{Return.level}
\alias{Weight.level}
\alias{Weight.transform}
\title{aggregate portfolio returns and weights up to the chosen level from the
hierarchy}
\usage{
  Weight.transform(wp, Rp)
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
  Aggregate returns and weights up to the chosen level from
  the hierarchy. Hierarchy can be used from the
  buildHierarchy function or defined manually in the same
  way as the buildHierarchy's output. If for the selected
  level the values in the hierarchy are numeric, the
  aggregation of returns or weights is performed by
  quintiles. \code{Weight.transform} makes transformation
  of weights to the xts object conformable with returns.
}
\examples{
data(attrib)
Weight.transform(wp, Rp)
Return.level(Rp, wp, h, level = "MarketCap")
Weight.level(wp, h, level = "Sector")
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
  \code{\link{buildHierarchy}} \cr
  \code{\link{Attribution}} \cr
  \code{\link{Return.portfolio}}
}
\keyword{attribution}

