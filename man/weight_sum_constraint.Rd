\name{weight_sum_constraint}
\alias{weight_sum_constraint}
\title{constructor for weight_sum_constraint}
\usage{
  weight_sum_constraint(type, min_sum = 0.99,
    max_sum = 1.01, enabled = FALSE, ...)
}
\arguments{
  \item{type}{character type of the constraint}

  \item{min_sum}{minimum sum of all asset weights, default
  0.99}

  \item{max_sum}{maximum sum of all asset weights, default
  1.01}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}
}
\description{
  This function is called by add.constraint when
  type="weight_sum" is specified. see
  \code{\link{add.constraint}} This function allows the
  user to specify the minimum and maximum that the weights
  sum to
}
\author{
  Ross Bennett
}

