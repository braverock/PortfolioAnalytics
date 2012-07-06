\name{Conv.option}
\alias{attribution}
\alias{Conv.option}
\alias{options}
\title{convert information about options, warrants or convertible bonds to the
equivalent of returns}
\usage{
  Conv.option(option)
}
\arguments{
  \item{\code{n}{x 8} matrix containing option ID (as
  rownames), and columns corresponding to (in particular
  order): strike price, number of options, current option
  price, end option price, option's delta, returns on the
  underlying}
}
\value{
  This function returns the equivalent of returns for
  options, warrants or convertible bonds
}
\description{
  The performance of option contracts are measured in
  exactly the same way as any other asset but they also
  generate economic exposures. The key to attribution
  analysis with options is to include the appropriate
  economic exposure for which we can use the option’s
  delta. The option itself will provide part of the
  valuation; the remainder of the option’s economic value
  must be achieved again by using notional assets:
  \deqn{option economic exposure = \delta \times number of
  options \times strike price = option valuation + notional
  exposure}
}
\examples{
option = matrix(c(1000, 1000, 1000, 300, 400, 10, 20, 30, 40, 50, 10, 11, 12, 13, 14, 12, 13, 14, 15, 16, 0.1, 0.2, 0.3, 0.4, 0.5, 0.1, 0.1, 0.2, 0.2, 0.3), 5, 6)
colnames(option) = c("Strike", "Number", "Current option", "End option", "delta", "returns")
rownames(option) = c("CVX", "XOM", "GE", "WMT", "FB")
Conv.option(option)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. p. 237-241
}
\seealso{
  \code{\link{Attribution}}
}
\keyword{attribution}
\keyword{attribution,}
\keyword{derivatives}
\keyword{option}

