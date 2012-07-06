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
  returns are decomposed as follows: \deqn{r - b =
  Allocation + Selection + Interaction} \deqn{Allocation =
  \overset{T}{\underset{t=1}{\prod}}(1+bs_{t})-\overset{T}{\underset{t=1}{\prod}}(1+rb_{t})}
  \deqn{Selection =
  \overset{T}{\underset{t=1}{\prod}}(1+rs_{t})-\overset{T}{\underset{t=1}{\prod}}(1+rb_{t})}
  \deqn{Interaction =
  \overset{T}{\underset{t=1}{\prod}}(1+rp_{t})-\overset{T}{\underset{t=1}{\prod}}(1+rs_{t})-\overset{T}{\underset{t=1}{\prod}}(1+bs_{t})+\overset{T}{\underset{t=1}{\prod}}(1+rb_{t})}
  \deqn{rp_{i}} - portfolio returns at period \deqn{i}
  \deqn{rb_{i}} - benchmark returns at period \deqn{i}
  \deqn{rs_{i}} - selection notional fund returns at period
  \deqn{i} \deqn{bs_{i}} - allocation notional fund returns
  at period \deqn{i} \deqn{T} - number of periods
}
\examples{
data(attrib)
DaviesLaker(Rp, wp, Rb, wb)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 201-204

  Davies, O. and Laker, D. (2001) Multiple-period
  performance attribution using the brinson model.Journal
  of Performance MeasurementFall, 12–22.
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

