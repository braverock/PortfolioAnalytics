\name{group_constraint}
\alias{group_constraint}
\title{constructor for group_constraint}
\usage{
  group_constraint(type, assets, groups,
    group_labels = NULL, group_min, group_max,
    enabled = TRUE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{assets}{number of assets, or optionally a named
  vector of assets specifying seed weights}

  \item{groups}{vector specifying the groups of the assets}

  \item{group_labels}{character vector to label the groups
  (e.g. size, asset class, style, etc.)}

  \item{group_min}{numeric or vector specifying minimum
  weight group constraints}

  \item{group_max}{numeric or vector specifying minimum
  weight group constraints}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify
  group constraints}
}
\description{
  This function is called by add.constraint when
  type="group" is specified. see
  \code{\link{add.constraint}}
}
\examples{
data(edhec)
ret <- edhec[, 1:4]

pspec <- portfolio.spec(assets=colnames(ret))

pspec <- add.constraint(portfolio=pspec,
                        type="group",
                        groups=c(2, 2),
                        group_labels=c("Style A", "Style B"),
                        group_min=c(0.15, 0.25),
                        group_max=c(0.65, 0.55))
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

