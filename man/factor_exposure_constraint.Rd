\name{factor_exposure_constraint}
\alias{factor_exposure_constraint}
\title{Constructor for factor exposure constraint}
\usage{
  factor_exposure_constraint(type = "factor_exposure",
    assets, B, lower, upper, enabled = TRUE,
    message = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{assets}{named vector of assets specifying seed
  weights}

  \item{B}{vector or matrix of risk factor exposures}

  \item{lower}{vector of lower bounds of constraints for
  risk factor exposures}

  \item{upper}{vector of upper bounds of constraints for
  risk factor exposures}

  \item{enabled}{TRUE/FALSE}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters to specify
  risk factor exposure constraints}
}
\description{
  This function is called by add.constraint when
  type="factor_exposure" is specified. see
  \code{\link{add.constraint}}
}
\details{
  \code{B} can be either a vector or matrix of risk factor
  exposures (i.e. betas). If \code{B} is a vector, the
  length of \code{B} must be equal to the number of assets
  and lower and upper must be scalars. If \code{B} is
  passed in as a vector, it will be converted to a matrix
  with one column.

  If \code{B} is a matrix, the number of rows must be equal
  to the number of assets and the number of columns
  represent the number of factors. The length of lower and
  upper must be equal to the number of factors. The
  \code{B} matrix should have column names specifying the
  factors and row names specifying the assets. Default
  column names and row names will be assigned if the user
  passes in a \code{B} matrix without column names or row
  names.
}
\author{
  Ross Bennett
}

