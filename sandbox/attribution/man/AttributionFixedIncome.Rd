\name{AttributionFixedIncome}
\alias{AttributionFixedIncome}
\title{fixed income attribution}
\usage{
  AttributionFixedIncome(Rp, wp, Rb, wb, Rf, Dp, Db, S,
    wbf, geometric = FALSE)
}
\arguments{
  \item{Rp}{T x n xts, data frame or matrix of portfolio
  returns}

  \item{wp}{vector, xts, data frame or matrix of portfolio
  weights}

  \item{Rb}{T x n xts, data frame or matrix of benchmark
  returns}

  \item{wb}{vector, xts, data frame or matrix of benchmark
  weights}

  \item{Rf}{T x n xts, data frame or matrix with risk free
  rates}

  \item{Dp}{T x n xts, data frame or matrix with portfolio
  modified duration}

  \item{Db}{T x n xts, data frame or matrix with benchmark
  modified duration}

  \item{wbf}{vector, xts, data frame or matrix with
  benchmark weights of currency forward contracts}

  \item{S}{(T + 1) x n xts, data frame or matrix with spot
  rates. The first date should coincide with the first date
  of portfolio returns}

  \item{geometric}{- TRUE/FALSE for geometric/arithmetic
  attribution}
}
\value{
  list with total excess returns decomposed into
  allocation, selection (and currency effects)
}
\description{
  Performs fixed income attribution. The investment
  decision process for bond managers is very different from
  that of equity managers, therefore for most fixed income
  investment strategies the standard Brinson model is not
  suitable. Bonds are simply a series of defined future
  cash flows which are relatively easy to price. Fixed
  income performance is therefore driven by changes in the
  shape of the yield curve. Systematic risk in the form of
  duration is a key part of the investment process. Fixed
  income attribution is, in fact, a specialist form of
  risk-adjusted attribution. The arithmetic attribution is
  handled using weighted duration approach (Van Breukelen,
  2000). The allocation, selection and currency allocation
  effects for category \eqn{i} are: \deqn{A_{i} =
  (D_{pi}\times w_{pi}-D_{\beta}\times D_{bi}\times w_{pi})
  \times (-\Delta y_{bi} + \Delta y_{b})} \deqn{S_{i} =
  D_{i}\times w_{pi}\times (-\Delta y_{ri} + \Delta
  y_{bi})} \deqn{C_{i} = (w_{pi} - w_{bi})\times (c_{i} +
  R_{fi} - c')}{Ci = (wpi - wbi) * (ci + Rfi - c')} where
  \eqn{w_{pi}}{wpi} - portfolio weights, \eqn{w_{bi}}{wbi}
  - benchmark weights, \eqn{D_{i}}{Di} - modified duration
  in bond category \eqn{i}. Duration beta:
  \deqn{D_{\beta}=\frac{D_{r}}{D_{b}}}{Dbeta = Dr / Db}
  \eqn{D_{r}}{Dr} - portfolio duration, \eqn{D_{b}}{Db} -
  benchmark duration, \eqn{D_{bi}}{Dbi} - benchmark
  duration for category \eqn{i}, \eqn{D_{pi}}{Dpi} -
  portfolio duration for category \eqn{i}, \eqn{\Delta
  y_{ri}}{Delta yri} - change in portfolio yield for
  category \eqn{i}, \eqn{\Delta y_{bi}}{Delta ybi} - change
  in benchmark yield for category \eqn{i}, \eqn{\Delta
  y_{b}}{Delta yb} - change in benchmark yield,
  \eqn{R_{ci}}{Rci} - currency returns for category
  \eqn{i}, \eqn{R_{fi}}{Rfi} - risk-free rate in currency
  of asset \eqn{i}, \deqn{c'=
  \sum_{i}w_{bi}\times(R_{ci}+R_{fi})} The geometric
  attribution is adapted using Van Breukelen (2000)
  approach for the arithmetic attribution. The individual
  allocation and selection effects are computed as follows:
  \deqn{A_{i}=D_{i}w_{pi}-D_{\beta}D_{bi}w_{bi}}{Ai = Di *
  wpi - Dbeta * Dbi * wbi}
  \deqn{S_{i}=\frac{D_{pi}}{D_{bi}}\times (R_{bi} - R_{fi})
  + R_{fi}}{Si = Dpi / Dbi * (Rbi - Rfi) + Rfi}
}
\examples{
data(attrib)
AttributionFixedIncome(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20],
wb = attrib.weights[2, ], Rf = attrib.returns[, 23:32], Dp = attrib.returns[, 63:72], Db = attrib.returns[, 73:82],
S = attrib.currency[, 11:20], wbf = attrib.weights[4, ], geometric = FALSE)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. Chapter 7 \cr
  Van Breukelen, G. \emph{Fixed income attribution}.
  Journal of Performance Measurement. Sumer. p. 61-68. 2000
  \cr
}
\seealso{
  \code{\link{Attribution.levels}},
  \code{\link{Attribution.geometric}}
}
\keyword{attribution}

