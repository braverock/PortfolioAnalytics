\name{rp_grid}
\alias{rp_grid}
\title{Generate random portfolios based on grid search method}
\usage{
  rp_grid(portfolio, permutations = 2000, normalize = TRUE,
    ...)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio'
  specifying the constraints for the optimization, see
  \code{\link{portfolio.spec}}}

  \item{permutations}{integer: number of unique constrained
  random portfolios to generate}

  \item{normalize}{TRUE/FALSE to normalize the weghts to
  satisfy min_sum or max_sum}

  \item{\dots}{any passthru parameters. Currently ignored}
}
\value{
  matrix of random portfolio weights
}
\description{
  This function generates random portfolios based on the
  \code{gridSearch} function from the 'NMOF' package.
}
\details{
  The number of levels is calculated based on permutations
  and number of assets. The number of levels must be an
  integer and may not result in the exact number of
  permutations. We round up to the nearest integer for the
  levels so the number of portfolios generated will be
  greater than or equal to permutations.

  The grid search method only satisfies the \code{min} and
  \code{max} box constraints. The \code{min_sum} and
  \code{max_sum} leverage constraints will likely be
  violated and the weights in the random portfolios should
  be normalized. Normalization may cause the box
  constraints to be violated and will be penalized in
  \code{constrained_objective}.
}

