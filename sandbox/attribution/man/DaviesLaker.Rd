\name{DaviesLaker}
\alias{DaviesLaker}
\title{calculates total attribution effects using Davies and Laker smoothing}
\usage{
  DaviesLaker(Rp, wp, Rb, wb)
}
\arguments{
  \item{Rp}{xts of portfolio returns}

  \item{wp}{xts of portfolio weights}

  \item{Rb}{xts of benchmark returns}

  \item{wb}{xts of benchmark weights}
}
\value{
  This function returns the data.frame with original
  attribution effects and total attribution effects over
  multiple periods
}
\description{
  Calculates total attribution effects over multiple
  periods using Davies and Laker linking method. Used
  internally by the \code{\link{Attribution}} function.
  Arithmetic attribution effects do not naturally link over
  time. This function uses Davies and Laker linking method
  to compute total attribution effects. Arithmetic excess
  returns are decomposed as follows: \deqn{R_{p} - R_{b} =
  Allocation + Selection + Interaction}{Rp - Rb =
  Allocation + Selection + Interaction} \deqn{Allocation =
  \prod^{T}_{t=1}(1+bs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
  \deqn{Selection =
  \prod^{T}_{t=1}(1+rs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
  \deqn{Interaction =
  \prod^{T}_{t=1}(1+R_{pt})-\prod^{T}_{t=1}(1+rs_{t})-
  \prod^{T}_{t=1}(1+bs_{t})+\prod^{T}_{t=1}(1+R_{bt})}
  \eqn{R_{pi}}{Rpi} - portfolio returns at period \eqn{i},
  \eqn{R_{bi}}{Rbi} - benchmark returns at period \eqn{i},
  \eqn{rs_{i}}{rsi} - selection notional fund returns at
  period \eqn{i}, \eqn{bs_{i}}{bsi} - allocation notional
  fund returns at period \eqn{i}, \eqn{T} - number of
  periods
}
\examples{
data(attrib)
DaviesLaker(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], wb = attrib.weights[2, ])
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 201-204 \cr
  Davies, O. and Laker, D. (2001) \emph{Multiple-period
  performance attribution using the Brinson model}. Journal
  of Performance Measurement. Fall. p. 12-22 \cr
}
\seealso{
  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr
  \code{\link{Grap}} \cr \code{\link{Carino}} \cr
  \code{\link{Attribution.geometric}} \cr
  \code{\link{Frongello}}
}
\keyword{and}
\keyword{arithmetic}
\keyword{attribution,}
\keyword{Davies}
\keyword{Laker}
\keyword{linking}

