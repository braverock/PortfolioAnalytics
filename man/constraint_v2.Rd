\name{constraint_v2}
\alias{constraint_v2}
\title{constructor for class v2_constraint}
\usage{
  constraint_v2(type, enabled = TRUE, ...,
    constrclass = "v2_constraint")
}
\arguments{
  \item{type}{character type of the constraint to add or
  update}

  \item{assets}{number of assets, or optionally a named
  vector of assets specifying seed weights}

  \item{...}{any other passthru parameters}

  \item{constrclass}{character to name the constraint
  class}
}
\description{
  This function is called by the constructor for the
  specific constraint.
}
\author{
  Ross Bennett
}

