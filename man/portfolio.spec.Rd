\name{portfolio.spec}
\alias{portfolio}
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
  currency, etc. Default NULL}

  \item{weight_seq}{seed sequence of weights, see
  \code{\link{generatesequence}} Default NULL}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}
}
\value{
  an object of class \code{portfolio}
}
\description{
  The portfolio object is created with
  \code{portfolio.spec}. The portfolio object is an S3
  object of class 'portfolio' used to hold the initial
  asset weights, constraints, objectives, and other
  information about the portfolio. The only required
  argument to \code{portfolio.spec} is \code{assets}.
}
\details{
  The portfolio object contains the following elements:
  \itemize{ \item{\code{assets}}{ named vector of the seed
  weights} \item{\code{category_labels}}{ character vector
  to categorize the assets by sector, geography, etc.}
  \item{\code{weight_seq}}{ sequence of weights used by
  \code{\link{random_portfolios}}. See
  \code{\link{generatesequence}}}
  \item{\code{constraints}}{ a list of constraints added to
  the portfolio object with \code{\link{add.constraint}}}
  \item{\code{objectives}}{ a list of objectives added to
  the portfolio object with \code{\link{add.objective}}}
  \item{\code{call}}{ the call to \code{portfolio.spec}
  with all of the specified arguments} }
}
\examples{
data(edhec)
pspec <- portfolio.spec(assets=colnames(edhec))
pspec <- portfolio.spec(assets=10, weight_seq=generatesequence())
}
\author{
  Ross Bennett, Brian G. Peterson
}
\seealso{
  \code{\link{add.constraint}},
  \code{\link{add.objective}},
  \code{\link{optimize.portfolio}}
}

