\name{portfolio.spec}
\alias{portfolio.spec}
\title{constructor for class portfolio}
\usage{
  portfolio.spec(assets = NULL, category_labels = NULL,
    weight_seq = NULL, message = FALSE)
}
\arguments{
  \item{assets}{number of assets, or optionally a named
  vector of assets specifying seed weights. If seed weights
  are not specified, an equal weight portfolio will be
  assumed.}

  \item{category_labels}{character vector to categorize
  assets by sector, industry, geography, market-cap,
  currency, etc.}

  \item{weight_seq}{seed sequence of weights, see
  \code{\link{generatesequence}}}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}
}
\description{
  constructor for class portfolio
}
\examples{
pspec <- portfolio.spec(assets=10, weight_seq=generatesequence())
}
\author{
  Ross Bennett
}

