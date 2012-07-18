\name{AppraisalRatio}
\alias{AppraisalRatio}
\title{Apraisal ratio = alpha / residual return}
\usage{
  AppraisalRatio(Ra, Rb, Rf = 0, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of the asset returns}

  \item{Rb}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of the benchmark asset return}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{\dots}{any other passthrough parameters}
}
\description{
  The alpha from CAPM divided by the standard deviation of
  residuals
}
\details{
  \deqn{A_{p}=\frac{\alpha_{p}}{\sigma(\varepsilon_{p})}}{Ap
  = alphap / sigmap(epsilonp)}

  Is appropriate for evaluating security selection ability.
  It captures the idea that an active portfolio manager has
  t odepart from the benchmark, that is, take on residual
  risk, in order to produce alpha. The more alpha the
  investor produces for a given amout of residual risk, the
  higher the ratio.
}
\examples{
data(managers)
AppraisalRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12)
AppraisalRatio(managers[80:120,1:6], managers[80:120,7,drop=FALSE], managers[80:120,10,drop=FALSE])
AppraisalRatio(managers[80:120,1:6], managers[80:120,8:7], managers[80:120,10,drop=FALSE])
}
\author{
  Andrii Babii
}
\references{
  J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio
  Performance Measurement and Benchmarking}. 2009.
  McGraw-Hill, p. 101. \cr Jack Treynor and Fischer Black,
  "How t oUse Security Analysis to Improve Portfolio
  Selection," \emph{Journal of Business}, vol.46, no.1,
  January 1973, pp. 66-86. \cr
}
\seealso{
  \code{\link{InformationRatio}}
}

