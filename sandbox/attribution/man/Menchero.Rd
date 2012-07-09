\name{Menchero}
\alias{Menchero}
\title{calculates total attribution effects using Menchero smoothing}
\usage{
  Menchero(rp, rb, attributions, adjusted)
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
  periods using Menchero linking method. Used internally by
  the \code{\link{Attribution}} function. Arithmetic
  attribution effects do not naturally link over time. This
  function uses Menchero smoothing algorithm to adjust
  attribution effects so that they can be summed up over
  multiple periods Attribution effect are multiplied by the
  adjustment factor \deqn{A_{t}' = A_{t} \times (M +a_{t})}
  where
  \deqn{M=\frac{\frac{1}{n}(R_{p}-R_{b})}{(1+R_{p})^{\frac{1}{n}}-(1+R_{b})^{\frac{1}{n}}}}
  \deqn{a_{t} =
  \left(\frac{R_{p}-R_{b}-M\times\sum^{n}_{t=1}(R_{pt}-R_{bt})}{\sum^{n}_{t=1}(R_{pt}-R_{bt})^{2}}\right)\times(R_{pt}-R_{bt})}
  In case if portfolio and benchmark returns are equal the
  limit of the above value is used: \deqn{M = (1 +
  r_{p})^\frac{n-1}{n}} A_t' - adjusted attribution effects
  at period t, A_t - unadjusted attribution effects at
  period t, R_pt - portfolio returns at period t, R_bt -
  benchmark returns at period t, Rp - total portfolio
  returns, Rb - total benchmark returns, n - number of
  periods
}
\details{
  The total arithmetic excess returns can be explained in
  terms of the sum of adjusted attribution effects:
  \deqn{R_{p} - R_{b} =
  \sum^{n}_{t=1}\left(Allocation_{t}+Selection_{t}+Interaction_{t}\right)}
}
\examples{
data(attrib)
Menchero(rp, rb, allocation, adjusted = FALSE)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 194-196 \cr
  Menchero, J. (2000) \emph{An optimized approach to
  linking attribution effects over time}. Journal of
  Performance Measurement. Fall. p. 36-42 \cr
}
\seealso{
  \code{\link{Attribution}} \cr \code{\link{Carino}} \cr
  \code{\link{Grap}} \cr \code{\link{Frongello}} \cr
  \code{\link{Attribution.geometric}}
}
\keyword{arithmetic}
\keyword{attribution,}
\keyword{linking}
\keyword{Menchero}

