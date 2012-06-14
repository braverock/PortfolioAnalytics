\name{logLinking}
\alias{logLinking}
\title{Logarithmic Linking}
\usage{
  logLinking(weighted.portfolio.returns,
    weighted.benchmark.returns, component)
}
\arguments{
  \item{weighted.portfolio.returns}{\code{n x p} data frame
  containing asset ID (as rownames) and weighted portfolio
  returns for \code{p} dates}

  \item{weighted.benchmark.returns}{\code{m x p} containing
  benchmark ID (as rownames) and weighted benchmark returns
  for \code{p} dates}

  \item{component}{data frame containing ID and attribution
  component for \code{p} dates (e.g., \code{n x p} for
  manager selection or \code{m x p} strategy allocation)}
}
\value{
  Returns a list of two objects of class "\code{numeric}"
  after applying the linking coefficient from the
  logarithmic method: \item{linked}{ combined
  \code{component} over time } \item{linked.total}{ sum of
  combined \code{component} over time }
}
\description{
  Aggregates performance attribution effects over time to
  produce a multiperiod summary.
}
\details{
  Transforms to a multiperiod arithmetic decomposition by
  distributing the residual proportionately. Creates
  linking coefficients \code{k_t / k} and applies to given
  \code{component} to combine arithmetic attribution
  effects over time. If \code{weighted.benchmark.returns}
  is zero for each \code{p} date, then the function returns
  absolute arithmetic attribution effect, e.g., factor
  returns or idiosyncratic returns from factor model.
}
\author{
  Eric Zivot
}
\references{
  Christopherson, J., Carino, D., and Ferson, W. (2009)
  \emph{Portfolio Performance Measurement and
  Benchmarking}, McGrall-Hill.
}
\seealso{
  \code{\link{relativeAttribution}}
}

