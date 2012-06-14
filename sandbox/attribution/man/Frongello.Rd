\name{Frongello}
\alias{Frongello}
\title{calculates total attribution effects using Frongello linking}
\usage{
  Frongello(Rp, wp, Rb, wb, attributions)
}
\arguments{
  \item{Rp}{xts of portfolio returns}

  \item{wp}{xts of portfolio weights}

  \item{Rb}{xts of benchmark returns}

  \item{wb}{xts of benchmark weights}

  \item{attributions}{xts with attribution effects}
}
\description{
  Calculates total attribution effects over multiple
  periods using logarithmic linking method. Used internally
  by the \code{\link{Attribution}} function. Arithmetic
  attribution effects do not naturally link over time. This
  function uses logarithmic smoothing algorithms to adjust
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
Frongello(Rp, wp, Rb, wb, allocation)
}
\author{
  Andrii Babii
}
\references{
  Christopherson, Jon A., Carino, David R., Ferson, Wayne
  E. \emph{Portfolio Performance Measurement and
  Benchmarking}. McGraw-Hill. 2009. Chapter 19

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
\keyword{attribution,}
\keyword{Frongello}
\keyword{linking}

