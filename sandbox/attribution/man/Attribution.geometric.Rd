\name{Attribution.geometric}
\alias{Attribution.geometric}
\title{performs sector-based geometric attribution}
\usage{
  Attribution.geometric(Rp, wp, Rb, wb, Rpl = NA, Rbl = NA,
    Rbh = NA)
}
\arguments{
  \item{Rp}{xts of portfolio returns}

  \item{wp}{xts of portfolio weights}

  \item{Rb}{xts of benchmark returns}

  \item{wb}{xts of benchmark weights}

  \item{Rpl}{xts, data frame or matrix of portfolio returns
  in local currency}

  \item{Rbl}{xts, data frame or matrix of benchmark returns
  in local currency}

  \item{Rbh}{xts, data frame or matrix of benchmark returns
  hedged into the base currency}
}
\value{
  This function returns the list with attribution effects
  (allocation or selection effect) including total
  multi-period attribution effects
}
\description{
  Performs sector-based geometric attribution of excess
  return. Calculates total geometric attribution effects
  over multiple periods. Used internally by the
  \code{\link{Attribution}} function. Geometric attribution
  effects in the contrast with arithmetic do naturally link
  over time multiplicatively:
  \deqn{\frac{(1+R_{p})}{1+R_{b}}-1=\prod^{n}_{t=1}(1+A_{t}^{G})\times
  \prod^{n}_{t=1}(1+S{}_{t}^{G})-1} Total allocation effect
  at time \eqn{t}:
  \deqn{A_{t}^{G}=\frac{1+b_{S}}{1+R_{bt}}-1} Total
  selection effect at time \eqn{t}:
  \deqn{S_{t}^{G}=\frac{1+R_{pt}}{1+b_{S}}-1} Semi-notional
  fund: \deqn{b_{S}=\sum^{n}_{i=1}w_{pi}\times R_{bi}}
  \eqn{w_{pt}}{wpt} - portfolio weights at time \eqn{t},
  \eqn{w_{bt}}{wbt} - benchmark weights at time \eqn{t},
  \eqn{r_{t}}{rt} - portfolio returns at time \eqn{t},
  \eqn{b_{t}}{bt} - benchmark returns at time \eqn{t},
  \eqn{r} - total portfolio returns \eqn{b} - total
  benchmark returns \eqn{n} - number of periods
}
\details{
  The multi-currency geometric attribution is handled
  following the Appendix A (Bacon, 2004).

  The individual selection effects are computed using:
  \deqn{w_{pi}\times\left(\frac{1+R_{pLi}}{1+R_{bLi}}-1\right)\times
  \left(\frac{1+R_{bLi}}{1+b_{SL}}\right)}

  The individual allocation effects are computed using:
  \deqn{(w_{pi}-w_{bi})\times\left(\frac{1+R_{bHi}}{1+b_{L}}-1\right)}

  Where the total semi-notional returns hedged into the
  base currency were used: \deqn{b_{SH} =
  \sum_{i}w_{pi}\times R_{bi}((w_{pi} - w_{bi})R_{bHi} +
  w_{bi}R_{bLi})} Total semi-notional returns in the local
  currency: \deqn{b_{SL} = \sum_{i}w_{pi}R_{bLi}}
  \eqn{R_{pLi}}{RpLi} - portfolio returns in the local
  currency \eqn{R_{bLi}}{RbLi} - benchmark returns in the
  local currency \eqn{R_{bHi}}{RbHi} - benchmark returns
  hedged into the base currency \eqn{b_{L}}{bL} - total
  benchmark returns in the local currency \eqn{r_{L}}{rL} -
  total portfolio returns in the local currency The total
  excess returns are decomposed into:
  \deqn{\frac{(1+R_{p})}{1+R_{b}}-1=\frac{1+r_{L}}{1+b_{SL}}\times\frac{1+
  b_{SH}}{1+b_{L}}\times\frac{1+b_{SL}}{1+b_{SH}}\times\frac{1+R_{p}}{1+r_{L}}
  \times\frac{1+b_{L}}{1+R_{b}}-1}

  where the first term corresponds to the selection, second
  to the allocation, third to the hedging cost transferred
  and the last two to the naive currency attribution
}
\examples{
data(attrib)
Attribution.geometric(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ],
Rb = attrib.returns[, 11:20], wb = attrib.weights[2, ])
}
\author{
  Andrii Babii
}
\references{
  Christopherson, Jon A., Carino, David R., Ferson, Wayne
  E. \emph{Portfolio Performance Measurement and
  Benchmarking}. McGraw-Hill. 2009. Chapter 18-19 \cr
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. Chapter 5, 8,
  Appendix A \cr
}
\seealso{
  \code{\link{Attribution}}
}
\keyword{attribution,}
\keyword{geometric}
\keyword{linking}

