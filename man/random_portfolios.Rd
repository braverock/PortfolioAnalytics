\name{random_portfolios_v2}
\alias{random_portfolios}
\alias{random_portfolios_v2}
\title{version 2 generate an arbitary number of constrained random portfolios}
\usage{
  random_portfolios_v2(portfolio, permutations = 100,
    rp_method = "sample", eliminate = TRUE, ...)
}
\arguments{
  \item{portfolio}{an object of type "portfolio" specifying
  the constraints for the optimization, see
  \code{\link{constraint}}}

  \item{permutations}{integer: number of unique constrained
  random portfolios to generate}

  \item{\dots}{any other passthru parameters}

  \item{rp_method}{method to generate random portfolios}

  \item{eliminate}{TRUE/FALSE, eliminate portfolios that do
  not satisfy constraints}
}
\value{
  matrix of random portfolio weights
}
\description{
  repeatedly calls \code{\link{randomize_portfolio}} to
  generate an arbitrary number of constrained random
  portfolios.
}
\details{
  Random portfolios can be generate using one of three
  methods. \itemize{ \item{sample: }{The 'sample' method to
  generate random portfolios is based on an idea pioneerd
  by Pat Burns. This is the most flexible method and can
  generate portfolios to satisfy leverage, box, group, and
  position limit constraints.} \item{simplex: }{The
  'simplex' method to generate random portfolios is based
  on a paper by W. T. Shaw. The simplex method is useful to
  generate random portfolios with the full investment
  constraint, where the sum of the weights is equal to 1,
  and min box constraints. Values for \code{min_sum} and
  \code{max_sum} of the leverage constraint will be
  ignored, the sum of weights will equal 1. All other
  constraints such as group and position limit constraints
  will be handled by elimination. If the constraints are
  very restrictive, this may result in very few feasible
  portfolios remaining.} \item{grid: }{The 'grid' method to
  generate random portfolios is based on the
  \code(gridSearch} function in package 'NMOF'. The grid
  search method only satisfies the \code{min} and
  \code{max} box constraints. The \code{min_sum} and
  \code{max_sum} leverage constraints will likely be
  violated and the weights in the random portfolios should
  be normalized.  Normalization may cause the box
  constraints to be violated and will be penalized in
  \code{constrained_objective}.} }

  The constraint types checked are leverage, box, group,
  and position limit. Any portfolio that does not satisfy
  all these constraints will be eliminated.
}
\author{
  Peter Carl, Brian G. Peterson, (based on an idea by Pat
  Burns)
}
\seealso{
  \code{\link{portfolio.spec}}, \code{\link{objective}},
  \code{\link{randomize_portfolio_v2}}
}

