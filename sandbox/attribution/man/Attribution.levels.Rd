\name{Attribution.levels}
\alias{Attribution}
\alias{Attribution.levels}
\title{provides multi-level geometric peformance attribution}
\usage{
  Attribution.levels(Rp, wp, Rb, wb, h, ...)
}
\arguments{
  \item{Rp}{xts, data frame or matrix of portfolio returns}

  \item{wp}{vector, xts, data frame or matrix of portfolio
  weights}

  \item{Rb}{xts, data frame or matrix of benchmark returns}

  \item{wb}{vector, xts, data frame or matrix of benchmark
  weights}

  \item{h}{data.frame with the hierarchy obtained from the
  buildHierarchy function or defined manually in the the
  same style as buildHierarchy's output}
}
\description{
  Provides multi-level geometric peformance attribution.
  The Brinson model attribute excess returns at one level
  of the decision process. This function works with more
  complex decision processes. For instance, the 3-level
  decision process may have the following levels: type of
  asset - country - sector. The levels should be specified
  in the vector with elements in the particular order: from
  the highest level to the lowest. The contribution to the
  allocation in the ith category for the dth level is:
  \deqn{\left(^{d}wp_{i}-^{d}wb_{i}\right)\times\left(\frac{1+^{d}b_{i}}{1+^{d-1}b_{i}}-1\right)\times\frac{1+^{d-1}b_{i}}{1+bs^{d-1}}}
  The total attribution for each asset allocation step in
  the decision process is:
  \deqn{\frac{1+^{d}bs}{1+^{d-1}bs}-1} The final step,
  stock selection, is measured by:
  \deqn{^{d}w_{i}\times\left(\frac{1+r_{i}}{1+^{d}b_{i}}-1\right)\times\frac{1+^{d}b_{i}}{1+^{d}bs}}
}
\examples{
data(attrib)
Attribution.levels(Rp, wp, Rb, wb, h, c("type", "currency", "Sector"))
Attribution.levels(Rp, wp, Rb, wb, h, c("type", "Sector"))
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 215-220
}
\seealso{
  \code{\link{Attribution.geometric}}
}
\keyword{attribution}
\keyword{attribution,}
\keyword{geometric}
\keyword{multi-level}

