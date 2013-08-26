\name{create.EfficientFrontier}
\alias{create.EfficientFrontier}
\title{create an efficient frontier}
\usage{
  create.EfficientFrontier(R, portfolio, type,
    n.portfolios = 25, match.col = "ES",
    search_size = 2000, ...)
}
\arguments{
  \item{R}{xts of asset returns}

  \item{portfolio}{object of class 'portfolio' specifying
  the constraints and objectives, see
  \code{\link{portfolio.spec}}}

  \item{type}{type of efficient frontier, see details}

  \item{n.portfolios}{number of portfolios to calculate
  along the efficient frontier}

  \item{match.col}{column to match when extracting the
  efficient frontier from an objected created by
  optimize.portfolio}

  \item{seach_size}{passed to optimize.portfolio for
  type="DEoptim" or type="random"}

  \item{...}{passthrough parameters to
  \code{\link{optimize.portfolio}}}
}
\value{
  an object of class 'efficient.frontier' with the
  objective measures and weights of portfolios along the
  efficient frontier
}
\description{
  create an efficient frontier
}
\details{
  currently there are 4 'types' supported to create an
  efficient frontier \itemize{ \item{"mean-var", "mean-sd",
  or "mean-StdDev":}{ This is a special case for an
  efficient frontier that can be created by a QP solver.
  The \code{portfolio} object should have two objectives:
  1) mean and 2) var. The efficient frontier will be
  created via \code{\link{meanvar.efficient.frontier}}.}
  \item{"mean-ETL", "mean-ES", "mean-CVaR", "mean-etl"}{
  This is a special case for an efficient frontier that can
  be created by an LP solver.  The \code{portfolio} object
  should have two objectives: 1) mean and 2) ETL/ES/CVaR.
  The efficient frontier will be created via
  \code{\link{meanetl.efficient.frontier}}.}
  \item{"DEoptim"}{ This can handle more complex
  constraints and objectives than the simple mean-var and
  mean-ETL cases. For this type, we actually call
  \code{\link{optimize.portfolio}} with
  \code{optimize_method="DEoptim"} and then extract the
  efficient frontier with
  \code{\link{extract.efficient.frontier}}.}
  \item{"random"}{ This can handle more complex constraints
  and objectives than the simple mean-var and mean-ETL
  cases. For this type, we actually call
  \code{\link{optimize.portfolio}} with
  \code{optimize_method="random"} and then extract the
  efficient frontier with
  \code{\link{extract.efficient.frontier}}.} }
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{optimize.portfolio}},
  \code{\link{portfolio.spec}},
  \code{\link{meanvar.efficient.frontier}},
  \code{\link{meanetl.efficient.frontier}},
  \code{\link{extract.efficient.frontier}}
}

