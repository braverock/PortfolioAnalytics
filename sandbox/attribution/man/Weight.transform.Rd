\name{Weight.transform}
\alias{Weight.transform}
\title{transforms weights for the attribution functions}
\usage{
  Weight.transform(wp, Rp)
}
\arguments{
  \item{Rp}{xts, data frame or matrix of portfolio returns}

  \item{wp}{vector, xts, data frame or matrix of portfolio
  weights}
}
\description{
  Makes transformation of weights to the xts object
  conformable with returns and to be taken by the
  attribution functions
}
\examples{
data(attrib)
Weight.transform(wp = attrib.weights[1, ], Rp = attrib.returns[, 1:10])
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

