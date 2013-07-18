\name{rp_transform}
\alias{rp_transform}
\title{Transform a weights vector to satisfy leverage, box, group, and position_limit constraints using logic from \code{randomize_portfolio}}
\usage{
  rp_transform(w, min_sum = 0.99, max_sum = 1.01, min, max,
    groups, cLO, cUP, max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    max_permutations = 200)
}
\arguments{
  \item{w}{weights vector to be transformed}

  \item{min_sum}{minimum sum of all asset weights, default
  0.99}

  \item{max_sum}{maximum sum of all asset weights, default
  1.01}

  \item{min}{numeric or named vector specifying minimum
  weight box constraints}

  \item{max}{numeric or named vector specifying maximum
  weight box constraints}

  \item{groups}{vector specifying the groups of the assets}

  \item{cLO}{numeric or vector specifying minimum weight
  group constraints}

  \item{cUP}{numeric or vector specifying minimum weight
  group constraints}

  \item{max_pos}{maximum assets with non-zero weights}

  \item{group_pos}{vector specifying maximum number assets
  with non-zero weights per group}

  \item{max_pos_long}{maximum number of assets with long
  (i.e. buy) positions}

  \item{max_pos_short}{maximum number of assets with short
  (i.e. sell) positions}

  \item{max_permutations}{integer: maximum number of
  iterations to try for a valid portfolio, default 200}
}
\value{
  named weighting vector
}
\description{
  This function uses a block of code from
  \code{\link{randomize_portfolio}} to transform the weight
  vector if either the weight_sum (leverage) constraints,
  box constraints, group constraints, or position_limit
  constraints are violated. The resulting weights vector
  might be quite different from the original weights
  vector.
}
\author{
  Peter Carl, Brian G. Peterson, Ross Bennett (based on an
  idea by Pat Burns)
}

