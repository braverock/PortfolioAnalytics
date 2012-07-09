\name{Attribution}
\alias{Attribution}
\title{performs arithmetic attribution}
\usage{
  Attribution(Rp, wp, Rb, wb, wpf = NA, wbf = NA, S = NA,
    F = NA, Rpl = NA, Rbl = NA, Rbh = NA, bf = FALSE,
    method = c("none", "top.down", "bottom.up"),
    linking = c("carino", "menchero", "grap", "frongello", "davies.laker"),
    geometric = FALSE, adjusted = FALSE)
}
\arguments{
  \item{Rp}{T x n xts, data frame or matrix of portfolio
  returns}

  \item{wp}{vector, xts, data frame or matrix of portfolio
  weights}

  \item{Rb}{T x n xts, data frame or matrix of benchmark
  returns}

  \item{wb}{vector, xts, data frame or matrix of benchmark
  weights}

  \item{method}{Used to select the priority between
  allocation and selection effects in arithmetic
  attribution. May be any of: \itemize{ \item none -
  present allocation, selection and interaction effects
  independently, \item top.down - the priority is given to
  the sector allocation. Interaction term is combined with
  the security selection effect, \item bottom.up - the
  priority is given to the security selection. Interaction
  term is combined with the sector allocation effect}. By
  default "none" is selected}

  \item{wpf}{vector, xts, data frame or matrix with
  portfolio weights of currency forward contracts}

  \item{wbf}{vector, xts, data frame or matrix with
  benchmark weights of currency forward contracts}

  \item{S}{(T+1) x n xts, data frame or matrix with spot
  rates. The first date should coincide with the first date
  of portfolio returns}

  \item{F}{(T+1) x n xts, data frame or matrix with forward
  rates. The first date should coincide with the first date
  of portfolio returns}

  \item{Rpl}{xts, data frame or matrix of portfolio returns
  in local currency}

  \item{Rbl}{xts, data frame or matrix of benchmark returns
  in local currency}

  \item{Rbh}{xts, data frame or matrix of benchmark returns
  hedged into the base currency}

  \item{bf}{TRUE for Brinson and Fachler and FALSE for
  Brinson, Hood and Beebower arithmetic attribution}

  \item{linking}{Used to select the linking method to
  present the multi-period summary of arithmetic
  attribution effects. May be any of: \itemize{ \item
  carino - logarithmic linking coefficient method, \item
  menchero - Menchero's smoothing algorithm, \item grap -
  linking approach developed by GRAP, \item frongello -
  Frongello's linking method \item davies.laker - Davies
  and Laker's linking method By default Carino linking is
  selected}

  \item{geometric}{TRUE/FALSE, whether to use geometric or
  arithmetic excess returns for the attribution analysis}

  \item{adjusted}{TRUE/FALSE, whether to show original or
  smoothed attribution effects for each period}
}
\value{
  returns a list with the following components: excess
  returns with annualized excess returns over all periods,
  attribution effects (allocation, selection and
  interaction)
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
  returns at one level. If returns and weights are
  available at the lowest level (e.g. for individual
  instruments), the aggregation up to the chosen level from
  the hierarchy can be done using Return.level function.
  The attribution effects can be computed for several
  periods. The multi-period summary is obtained using one
  of linking methods: Carino, Menchero, GRAP, Frongello. It
  also allows to break down the geometric excess returns,
  which link naturally over time. Finally, it annualizes
  arithmetic and geometric excess returns similarly to the
  portfolio and/or benchmark returns annualization.
}
\details{
  The arithmetic excess returns are decomposed into the sum
  of allocation, selection and interaction effects across
  \deqn{n} sectors:
  \deqn{r-b=\overset{n}{\underset{i=1}{\sum}}\left(A_{i}+S_{i}+I_{i}\right)}
  The arithmetic attribution effects for the category
  \deqn{i} are computed as suggested in the Brinson, Hood
  and Beebower (1986): \deqn{A_{i}=(w_{pi}-w_{bi})\times
  R_{bi}} - allocation effect
  \deqn{S_{i}=w_{pi}\times(R_{pi}-R_{bi})} - selection
  effect \deqn{I_{i}=(w_{pi}-w_{bi})\times(r_{i}-b_{i})} -
  interaction effect \deqn{r} - total portfolio returns
  \deqn{b} - total benchmark returns \deqn{w_{pi}} -
  weights of the category \deqn{i} in the portfolio
  \deqn{w_{bi}} - weigths of the category \deqn{i} in the
  benchmark \deqn{R_{pi}} - returns of the portfolio
  category \deqn{i} \deqn{R_{bi}} - returns of the
  benchmark category \deqn{i} If Brinson and Fachler (1985)
  is selected the allocation effect differs:
  \deqn{A_{i}=(w_{pi}-w_{bi})\times (R_{bi} - b)} Depending
  on goals we can give priority to the allocation or to the
  selection effects. If the priority is given to the sector
  allocation the interaction term will be combined with the
  security selection effect (top-down approach). If the
  priority is given to the security selection, the
  interaction term will be combined with the
  asset-allocation effect (bottom-up approach). Usually we
  have more than one period. In that case individual
  arithmetic attribution effects should be adjusted using
  linking methods. Adjusted arithmetic attribution effects
  can be summed up over time to provide the multi-period
  summary:
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
  returns: \deqn{GAER=\frac{1+r_{a}}{1+b_{a}}-1} In the
  case of multi-currency portfolio, the currency return,
  currency surprise and forward premium should be
  specified. The multi-currency arithmetic attribution is
  handled following Ankrim and Hensel (1992). Currency
  returns are decomposed into the sum of the currency
  surprise and the forward premium: \deqn{R_{ci} = R_{cei}
  + R_{fpi}}, where \deqn{R_{cei} = \frac{S_{i}^{t+1} -
  F_{i}^{t+1}}{S_{i}^{t}} \deqn{R_{fpi} =
  \frac{F_{i}^{t+1}}{S_{i}^{t}} - 1} \deqn{S_{i}^{t}} -
  stop rate for asset i at time t \deqn{F_{i}^{t}} -
  forward rate for asset i at time t Excess returns are
  decomposed into the sum of allocation, selection and
  interaction effects as in the standard Brinson model:
  \deqn{r-b=\overset{n}{\underset{i=1}{\sum}}\left(A_{i}+S_{i}+I_{i}\right)}
  However the allocation effect is computed taking into
  account currency effects:
  \deqn{A_{i}=(w_{pi}-w_{bi})\times (R_{bi} - R_{ci} -
  R_{l})} - allocation \deqn{R_{l} =
  \overset{n}{\underset{i=1}{\sum}}w_{bi}\times(R_{bi}-R_{ci})}
  - benchmark return adjusted for currecy. The contribution
  from currency is analogous to asset allocation:
  \deqn{C_{i} = (w_{pi} - w_{bi}) \times (R_{cei} - e) +
  (w_{pfi} - w_{bfi}) \times (R_{fi} - e)} where \deqn{e =
  \overset{n}{\underset{i=1}{\sum}}w_{bi}\times R_{cei}}
  The final term, forward premium, is also analogous to the
  asset allocation: \deqn{R_{fi} = (w_{pi} - w_{bi}) \times
  (R_{fpi} - d)} where \deqn{d =
  \overset{n}{\underset{i=1}{\sum}}w_{bi}\times R_{fpi}}
  \deqn{R_{fpi}} - forward premium
}
\examples{
data(attrib)
Attribution(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
}
\author{
  Andrii Babii
}
\references{
  Ankrim, E. and Hensel, C. \emph{Multi-currency
  performance attribution}.Russell Research Commentary.
  November 2002

  Bacon, C. \emph{Practical Portfolio Performance
  Measurement and Attribution}. Wiley. 2004. Chapter 5, 6,
  8

  Christopherson, Jon A., Carino, David R., Ferson, Wayne
  E. \emph{Portfolio Performance Measurement and
  Benchmarking}. McGraw-Hill. 2009. Chapter 18-19

  Brinson, G. and Fachler, N. (1985) \emph{Measuring non-US
  equity portfolio

  Gary P. Brinson, L. Randolph Hood, and Gilbert L.
  Beebower, \emph{Determinants of Portfolio Performance},
  Financial Analysts Journal,

  Karnosky, D. and Singer, B. \emph{Global asset management
  and performance attribution. The Research Foundation of
  the Institute of Chartered Financial Analysts}. February
  1994.
}
\seealso{
  \code{\link{Attribution.levels}},
  \code{\link{Attribution.geometric}}
}
\keyword{attribution}

