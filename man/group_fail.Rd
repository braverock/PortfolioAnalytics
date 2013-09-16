\name{group_fail}
\alias{group_fail}
\title{Test if group constraints have been violated}
\usage{
  group_fail(weights, groups, cLO, cUP, group_pos = NULL)
}
\arguments{
  \item{weights}{weights vector to test}

  \item{groups}{list of vectors specifying the groups of
  the assets}

  \item{cLO}{numeric or vector specifying minimum weight
  group constraints}

  \item{cUP}{numeric or vector specifying minimum weight
  group constraints}

  \item{group_pos}{vector specifying the number of non-zero
  weights per group}
}
\value{
  logical vector: TRUE if group constraints are violated
  for a given group
}
\description{
  The function loops through each group and tests if cLO or
  cUP have been violated for the given group. This is a
  helper function for \code{\link{rp_transform}}.
}
\author{
  Ross Bennett
}

