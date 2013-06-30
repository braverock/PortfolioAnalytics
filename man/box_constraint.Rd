\name{box_constraint}
\alias{box_constraint}
\title{constructor for box_constraint.}
\usage{
  box_constraint(type, assets, min, max, min_mult,
    max_mult, enabled = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{assets}{number of assets, or optionally a named
  vector of assets specifying seed weights}

  \item{min}{numeric or named vector specifying minimum
  weight box constraints}

  \item{max}{numeric or named vector specifying minimum
  weight box constraints}

  \item{min_mult}{numeric or named vector specifying
  minimum multiplier box constraint from seed weight in
  \code{assets}}

  \item{max_mult}{numeric or named vector specifying
  maximum multiplier box constraint from seed weight in
  \code{assets}}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  constraints}
}
\description{
  This function is called by add.constraint when type="box"
  is specified. see \code{\link{add.constraint}}
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

