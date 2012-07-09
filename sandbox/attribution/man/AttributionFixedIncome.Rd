\name{AttributionFixedIncome}
\alias{attribution}
\alias{AttributionFixedIncome}
\alias{fixed}
\alias{income}
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

  \item{wbf}{vector, xts, data frame or matrix with
  benchmark weights of currency forward contracts}
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
  effects for category i are: \deqn{A_{i} = (D_{pi}\times
  w_{i}-D_{\beta}\times D_{bi}\times w_{pi})\times (-\Delta
  y_{bi} + \Delta y_{b})} \deqn{S_{i} = D_{i}\times
  w_{i}\times (-\Delta y_{ri} + \Delta y_{bi})} \deqn{C_{i}
  = (w_{pi} - w_{bi})\times (c_{i} + R_{fi} - c')} where
  \deqn{w_{pi}} - portfolio weights \deqn{w_{bi}} -
  benchmark weights \deqn{D_{i}} - modified duration in
  bond category i \deqn{D_{\beta}=\frac{D_{r}}{D_{b}}} -
  duration beta \deqn{D_{r}} - portfolio duration
  \deqn{D_{b}} - benchmark duration \deqn{D_{bi}} -
  benchmark duration for category i \deqn{D_{pi}} -
  portfolio duration for category i \deqn{\Delta y_{ri}} -
  change in portfolio yield for category i \deqn{\Delta
  y_{bi}} - change in benchmark yield for category i
  \deqn{\Delta y_{b}} - change in benchmark yield
  \deqn{R_{ci} - currency returns for category i
  \deqn{R_{fi}} - risk-free rate in currency of asset i
  \deqn{c'= \underset{i}{\sum}w_{bi}\times(R_{ci}+R_{fi})}
  The geometric attribution is adapted using Van Breukelen
  (2000) approach for the arithmetic attribution. The
  individual allocation and selection effects are computed
  as follows:
  \deqn{A_{i}=D_{i}w_{pi}-D_{\beta}D_{bi}w_{bi}}
  \deqn{S_{i}=\frac{D_{pi}}{D_{bi}}\times (R_{bi} - R_{fi})
  + R_{fi}}
}
\examples{
data(attrib)
AttributionFixedIncome(Rp, wp, Rb, wb, Rf, Dp, Db, S, wbf, geometric = FALSE)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. Chapter 7

  Van Breukelen, G. \emph{Fixed income attribution}.
  Journal of Performance
}
\seealso{
  \code{\link{Attribution.levels}},
  \code{\link{Attribution.geometric}}
}
\keyword{attribution}

