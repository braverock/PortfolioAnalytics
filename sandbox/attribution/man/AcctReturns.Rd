\name{AcctReturns}
\alias{AcctReturns}
\title{Calculate account returns}
\usage{
  AcctReturns(Account, Dates = NULL, Portfolios = NULL,
    method = c("timeweighted", "dietz"), ...)
}
\arguments{
  \item{Account}{string name of the account to generate
  returns for}

  \item{\dots}{any other passthru parameters (like
  \code{native} for \code{.getBySymbol}}

  \item{Dates}{xts style ISO 8601 date subset to retrieve,
  default NULL (all dates)}

  \item{Portfolios}{concatenated string vector for
  portfolio names to retrieve returns on, default NULL (all
  portfolios)}

  \item{method}{Used to select between time-weighted and
  linked modified Dietz returns. May be any of:
  \itemize{\item timeweighted \item dietz} By default
  time-weigthed is selected}
}
\value{
  returns xts with account returns
}
\description{
  Similar to the \code{PortfReturns}, but gives returns for
  the entire account and takes into account cashflows.
  Allows selecting between time-weighted returns and linked
  modified Dietz approach. If time-weighted method is
  selected, the returns are computed using:
  \deqn{r_{t}=\frac{V_{t}}{V_{t-1}+C_{t}}-1} where
  \eqn{V_{t}} - account value at time \eqn{t}, \eqn{C_{t}}
  - cashflow at time \eqn{t}.These returns then can be
  linked geometrically (for instance using
  \code{Return.cumulative} function from the
  \code{PerformanceAnalytics} package) to yield cumulative
  multiperiod returns:
  \deqn{1+r=\prod_{t=1}^{T}(1+r_{t})=\prod_{t=1}^{T}\frac{V_{t}}{V_{t-1}+C_{t}}}
  In case if there were no cashflows, the result reduces to
  simple one-period returns. If Modified Dietz method is
  selected, monthly returns are computed taking into
  account cashflows within each month: \deqn{r =
  \frac{V_{t}-V_{t-1}-C}{V_{t-1}+\sum_{t}C_{t}\times
  W_{t}}} where \eqn{C} - total external cash flows within
  a month, \eqn{C_{t}} - external cashflow on day \eqn{t},
  \eqn{W_{t}=\frac{TD-D_{t}}{TD}} - weighting ratio to be
  applied to external cashflow on day \eqn{t}, \eqn{TD} -
  total number of days wihting the month, \eqn{D_{t}} -
  number of days since the beginning of the month including
  weekends and public holidays. Finally monthly Modified
  Dietz returns can also be linked geometrically.
}
\note{
  TODO handle portfolio and account in different currencies
  (not hard, just not done)

  TODO explicitly handle portfolio weights

  TODO provide additional methods of calculating returns

  TODO support additions and withdrawals to available
  capital
}
\author{
  Brian Peterson, Andrii Babii
}
\references{
  Christopherson, Jon A., Carino, David R., Ferson, Wayne
  E. \emph{Portfolio Performance Measurement and
  Benchmarking}. McGraw-Hill. 2009. Chapter 5 \cr Bacon, C.
  \emph{Practical Portfolio Performance Measurement and
  Attribution}. Wiley. 2004. Chapter 2 \cr
}
\seealso{
  PortfReturns
}
\keyword{portfolio}
\keyword{returns}

