\name{relativeAttributionWithoutFactors}
\alias{relativeAttributionWithoutFactors}
\title{Relative Performance Attribution}
\usage{
  relativeAttributionWithoutFactors(portfolio.df,
    benchmark.df)
}
\arguments{
  \item{portfolio.df}{\code{n x 3} data frame containing
  asset ID (as rownames), Strategy \code{n x 1}, Weight
  \code{n x 1}, Return \code{n x 1},}

  \item{benchmark.df}{\code{m x 3} data frame containing
  benchmark ID (as rownames), Strategy \code{n x 1},
  Weights \code{n x 1}, Returns \code{n x 1},}
}
\value{
  Returns a list with the following components:
  \item{portfolio.ret}{ \code{1 x 1} matrix containing
  portfolio return} \item{benchmark.ret}{ \code{1 x 1}
  matrix containing benchmark return} \item{relative.ret}{
  \code{1 x 1} matrix containing relative return}
  \item{manager}{ \code{n x 1} data frame containing asset
  ID (as rownames) and \code{ManagerSelection} }
  \item{manager.total}{ numeric value of total
  \code{ManagerSelection} component} \item{strategy}{
  \code{n x 2} data frame containing benchmark ID (as
  rownames), \code{Strategy}, and \code{StrategyAllocation}
  } \item{strategy.total}{ numeric value of total
  StrategyAllocation component}
}
\description{
  Decompose relative returns into manager selection and
  strategy allocation components.
}
\details{
  The Brinson model reflects a sector-based investment
  process and attributes portfolio return to manager
  selection and strategy allocation effects. Positive
  contributions are earned by overweighting outperforming
  managers/sectors and underweighting underperformers.
}
\author{
  Eric Zivot
}
\references{
  Davis, B. and Menchero, J. (2009) \emph{Beyond Brinson:
  Establishing the Link Between Sector and Factor Models},
  MSCI Barra Research Insights.
}
\seealso{
  \code{\link{logLinking}}
}

