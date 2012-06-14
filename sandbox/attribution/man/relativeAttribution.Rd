\name{relativeAttribution}
\alias{relativeAttribution}
\title{Relative Performance Attribution}
\usage{
  relativeAttribution(portfolio.df, benchmark.df)
}
\arguments{
  \item{portfolio.df}{\code{n x (3 + f)} data frame
  containing asset ID (as rownames), Strategy \code{n x 1},
  Weight \code{n x 1}, Return \code{n x 1},
  exposure-weighted factor returns \code{n x f} whose
  column names are the factor IDs}

  \item{benchmark.df}{\code{m x (3 + f)} data frame
  containing benchmark ID (as rownames), Strategy \code{n x
  1}, Weights \code{n x 1}, Returns \code{n x 1},
  exposure-weighted factor returns \code{n x f} whose
  column names are factor IDs.}
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
  \code{ManagerSelection} component} \item{manager.factor}{
  \code{n x (1 + f)} data frame containing asset ID (as
  rownames) and ManagerSelection by \code{Idiosyncratic}
  \code{n x 1} and factor IDs \code{n x f}}
  \item{manager.factor.total}{ \code{n x 1} numeric object
  with asset ID (as rownames) and ManagerSelection
  component summed over all factor IDs}
  \item{manager.idiosyncratic.total}{ numeric value of
  ManagerSelection component summed over all Idiosyncratic
  values} \item{strategy}{ \code{n x 2} data frame
  containing benchmark ID (as rownames), \code{Strategy},
  and \code{StrategyAllocation} } \item{strategy.total}{
  numeric value of total StrategyAllocation component}
  \item{strategy.factor}{ \code{m x (1 + f)} data frame
  containing benchmark ID (as rownames) and
  StrategyAllocation by \code{Idiosyncratic} \code{m x 1}
  and factor IDs \code{m x f}}
  \item{strategy.factor.total}{ \code{m x 1} numeric object
  with benchmark ID (as rownames) and StrategyAllocation
  component summed over all factor IDs}
  \item{strategy.idiosyncratic.total}{ numeric value of
  StrategyAllocation component summed over all
  Idiosyncratic values}
}
\description{
  Decompose relative returns into manager selection and
  strategy allocation components, and then into factor and
  idiosyncratic return contributions.
}
\details{
  The Brinson model reflects a sector-based investment
  process and attributes portfolio return to manager
  selection and strategy allocation effects. Positive
  contributions are earned by overweighting outperforming
  managers/sectors and underweighting underperformers. In
  addition, the manager selection and strategy allocation
  effects are decomposed into factor and idiosyncratic
  return contributions as defined by a factor model.
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

