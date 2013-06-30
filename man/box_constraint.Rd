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
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

# defaults to min=0 and max=1
pspec <- add.constraint(pspec, type="box")

# specify box constraints as a scalar
pspec <- add.constraint(pspec, type="box", min=0.05, max=0.45)

# specify box constraints per asset
pspec <- add.constraint(pspec, type="box", min=c(0.05, 0.10, 0.08, 0.06), max=c(0.45, 0.55, 0.35, 0.65))
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

