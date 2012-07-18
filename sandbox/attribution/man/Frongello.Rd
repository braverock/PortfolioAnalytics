\name{Frongello}
\alias{Frongello}
\title{calculates total attribution effects using Frongello smoothing}
\usage{
  Frongello(rp, rb, attributions, adjusted)
}
\arguments{
  \item{rp}{xts of portfolio returns}

  \item{rb}{xts of benchmark returns}

  \item{attributions}{xts with attribution effects}

  \item{adjusted}{TRUE/FALSE, whether to show original or
  smoothed attribution effects for each period}
}
\value{
  returns a data frame with original attribution effects
  and total attribution effects over multiple periods
}
\description{
  Calculates total attribution effects over multiple
  periods using Frongello linking method. Used internally
  by the \code{\link{Attribution}} function. Arithmetic
  attribution effects do not naturally link over time. This
  function uses Frongello smoothing algorithm to adjust
  attribution effects so that they can be summed up over
  multiple periods Adjusted attribution effect at period t
  are: \deqn{A_{t}' =
  A_{t}\times\prod^{t-1}_{i=1}(1+r_{pi})+R_{bt}
  \times\sum^{t-1}_{i=1}A_{i}'} \eqn{A_{t}'}{At'} -
  adjusted attribution effects at period \eqn{t},
  \eqn{A_{t}}{At} - unadjusted attribution effects at
  period \eqn{t}, \eqn{R_{pi}}{Rpi} - portfolio returns at
  period \eqn{i}, \eqn{R_{bi}}{Rbi} - benchmark returns at
  period, \eqn{R_{p}}{Rp} - total portfolio returns,
  \eqn{R_{b}}{Rb} - total benchmark returns, \eqn{n} -
  number of periods
}
\examples{
data(attrib)
Frongello(rp = attrib.returns[, 21], rb = attrib.returns[, 22], attributions = attrib.allocation, adjusted = FALSE)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 199-201 \cr
  Frongello, A. (2002) \emph{Linking single period
  attribution results}. Journal of Performance Measurement.
  Spring, p. 10-22. \cr
}
\seealso{
  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr
  \code{\link{Grap}} \cr \code{\link{Carino}} \cr
  \code{\link{Attribution.geometric}}
}
\keyword{arithmetic}
\keyword{attribution,}
\keyword{Frongello}
\keyword{linking}

