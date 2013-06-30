\name{group_constraint}
\alias{group_constraint}
\title{constructor for group_constraint}
\usage{
  group_constraint(type, assets, groups,
    group_labels = NULL, group_min, group_max,
    enabled = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{assets}{number of assets, or optionally a named
  vector of assets specifying seed weights}

  \item{groups}{vector specifying the groups of the assets}

  \item{group_labels}{character vector to label the groups
  (i.e. size, asset class, style, etc.)}

  \item{group_min}{numeric or vector specifying minimum
  weight group constraints}

  \item{group_max}{numeric or vector specifying minimum
  weight group constraints}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="group" is specified. see
  \code{\link{add.constraint}}
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{add.constraint}}
}

