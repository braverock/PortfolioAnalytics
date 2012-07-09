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
  A_{t}\times\overset{t-1}{\underset{i=1}{\prod}}(1+r_{i})+b_{t}\times\overset{t-1}{\underset{i=1}{\sum}}A_{i}'}
  \deqn{A_{t}}' - adjusted attribution effects at period
  \deqn{t} \deqn{A_{t}} - unadjusted attribution effects at
  period \deqn{t} \deqn{r_{i}} - portfolio returns at
  period \deqn{i} \deqn{b_{i}} - benchmark returns at
  period \deqn{i} \deqn{r} - total portfolio returns
  \deqn{b} - total benchmark returns \deqn{n} - number of
  periods
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
  Measurement and Attribution}. Wiley. 2004. p. 199-201

  Frongello, A. (2002) \emph{Linking single period
  attribution results}.
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

