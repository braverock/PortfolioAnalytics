\name{Modigliani}
\alias{Modigliani}
\title{Modigliani-Modigliani measure}
\usage{
  Modigliani(Ra, Rb, Rf = 0, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{\dots}{any other passthrough parameters}
}
\description{
  The Modigliani-Modigliani measure is the portfolio return
  adjusted upward or downward to match the benchmark's
  standard deviation.  This puts the portfolio return and
  the benchmark return on 'equal footing' from a standard
  deviation perspective.
}
\details{
  This is also analogous to some approaches to 'risk
  parity' portfolios, which use (presumably costless)
  leverage to increase the portfolio standard deviation to
  some target.
}
\examples{
data(managers)
round(Modigliani(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12),4)
round(Modigliani(managers[,1:6], managers[,8,drop=FALSE], Rf=.035/12),4)
}
\author{
  Andrii Babii, Brian G. Peterson
}
\references{
  J. Christopherson, D. Carino, W. Ferson. Portfolio
  Performance Measurement and Benchmarking. 2009.
  McGraw-Hill, 97-99.
}
\seealso{
  \code{\link{SharpeRatio}}, \code{\link{TreynorRatio}}
}

