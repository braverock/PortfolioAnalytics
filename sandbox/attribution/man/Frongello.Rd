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
  A_{t}\times\prod^{t-1}_{i=1}(1+r_{pi})+R_{bt}\times\sum^{t-1}_{i=1}A_{i}'}
  A_t' - adjusted attribution effects at period t, A_t -
  unadjusted attribution effects at period t, R_pi -
  portfolio returns at period i, R_bi - benchmark returns
  at period , Rp - total portfolio returns, Rb - total
  benchmark returns, n - number of periods
}
\examples{
data(attrib)
Frongello(rp, rb, allocation, adjusted = FALSE)
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

