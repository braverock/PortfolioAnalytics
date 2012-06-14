\name{Attribution}
\alias{Attribution}
\title{performs arithmetic attribution}
\usage{
  Attribution(Rp, wp, Rb, wb,
    method = c("none", "top.down", "bottom.up"),
    linking = c("carino", "menchero", "grap", "frongello"),
    geometric = FALSE)
}
\arguments{
  \item{Rp}{xts, data frame or matrix of portfolio returns}

  \item{wp}{vector, xts, data frame or matrix of portfolio
  weights}

  \item{Rb}{xts, data frame or matrix of benchmark returns}

  \item{wb}{vector, xts, data frame or matrix of benchmark
  weights}

  \item{method}{Used to select the priority between
  allocation and selection effects in arithmetic
  attribution. May be any of: \itemize{ \item none -
  present allocation, selection and interaction effects
  independently, \item top.down - the priority is given to
  the sector allocation. Interaction term is combined with
  the security selection effect, \item bottom.up - the
  priority is given to the security selection. Interection
  term is combined with the sector allocation effect}}

  \item{linking}{Used to select the linking method to
  present the multi-period summary of arithmetic
  attribution effects. It is also used to select the
  geometric attribution. May be any of: \itemize{ \item
  carino - logarithmic linking coefficient method, \item
  menchero - Menchero's smoothing algorithm, \item grap -
  linking approach developed by GRAP, \item frongello -
  Frongello's linking method}

  \item{geometric}{TRUE/FALSE, whether to use geometric or
  arithmetic excess returns for the attribution analysis}
}
\description{
  Performance attribution analysis. Portfolio performance
  measured relative to a benchmark gives an indication of
  the value-added by the portfolio. Equipped with weights
  and returns of portfolio segments, we can dissect the
  value-added into useful components. This function is
  based on the sector-based approach to the attribution.
  The workhorse is the Brinson model that explains the
  arithmetic difference between portfolio and benchmark
  returns. That is it breaks down the arithmetic excess
  returns at one level. The attribution effects can be
  computed for several periods. The multi-period summary is
  obtained using one of linking methods: Carino, Menchero,
  GRAP, Frongello. It also allows to break down the
  geometric excess returns, which link naturally over time.
  Finally, it annualizes arithmetic and geometric excess
  returns similarly to the portfolio and/or benchmark
  returns annualization.
}
\details{
  The arithmetic exess returns are decomposed into the sum
  of allocation, selection and interaction effects across
  \deqn{n} sectors:
  \deqn{r-b=\overset{n}{\underset{i=1}{\sum}}\left(A_{i}+S_{i}+I_{i}\right)}
  The arithmetic attribtion effects for the category
  \deqn{i} are computed as suggested in the Brinson, Hood
  and Beebower (1986): \deqn{A_{i}=(wp_{i}-wb_{i})\times
  b_{i}} - allocation effect
  \deqn{S_{i}=wp_{i}\times(r_{i}-b_{i})} - selection effect
  \deqn{I_{i}=(wp_{i}-wb_{i})\times(r_{i}-b_{i})} -
  interaction effect \deqn{r} - total portfolio returns
  \deqn{b} - total benchmark returns \deqn{wp_{i}} -
  weights of the category \deqn{i} in the portfolio
  \deqn{wb_{i}} - weigths of the category \deqn{i} in the
  benchmark \deqn{r_{i}} - returns of the portfolio
  category \deqn{i} \deqn{b_{i}} - returns of the benchmark
  category \deqn{i} Depending on goals we can give priority
  to the allocation or to the selection effects. If the
  priority is given to the sector allocation the
  interaction term will be combined with the security
  selection effect (top-down approach). If the priority is
  given to the security selection, the interaction term
  will be combined with the asset-allocation effect
  (bottom-up approach). Usually we have more then one
  period. In that case individual arithmetic attribution
  effects should be adjusted using linking methods.
  Adjusted arithmetic attribution effects can be summed up
  over time to provide the multi-period summary:
  \deqn{r-b=\overset{T}{\underset{t=1}{\sum}}\left(A_{t}'+S_{t}'+I_{t}'\right)}
  , where \deqn{T} - number of periods; prime stands for
  the adjustment. The geometric attribution effects do not
  suffer from the linking problem. Moreover we don't have
  the interaction term. For more details about the
  geometric attribution see the documentation to
  \code{link{Attribution.geometric}} Finally, arithmetic
  annualized excess returns are computed as the arithmetic
  difference between annualised portfolio and benchmark
  returns: \deqn{AAER=r_{a}-b_{a}}; the geometric
  annualized excess returns are computed as the geometric
  difference between annualized portfolio and benchmark
  returns: \deqn{GAER=\frac{1+r_{a}}{1+b_{a}}-1}
}
\examples{
data(attrib)
Attribution(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
Attribution(Rp, wp, Rb, wb, geometric = TRUE)
}
\author{
  Andrii Babii
}
\references{
  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. Chapter 5, 8

  Christopherson, Jon A., Carino, David R., Ferson, Wayne
  E. \emph{Portfolio Performance Measurement and
  Benchmarking}. McGraw-Hill. 2009. Chapter 18-19

  Gary P. Brinson, L. Randolph Hood, and Gilbert L.
  Beebower, \emph{Determinants of Portfolio Performance},
  Financial Analysts Journal, vol. 42, no. 4, July/August
}
\seealso{
  \code{\link{Attribution.levels}}
}
\keyword{attribution}

