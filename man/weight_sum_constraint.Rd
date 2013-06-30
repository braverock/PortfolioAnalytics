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

  \item{\dots}{any other passthru parameters to specify
  weight_sum constraints}
}
\description{
  This function is called by add.constraint when
  "weight_sum", "leverage", "full_investment",
  "dollar_neutral", or "active" is specified as the type.
  see \code{\link{add.constraint}} This function allows the
  user to specify the minimum and maximum that the weights
  sum to
}
\details{
  Special cases for the weight_sum constraint are
  "full_investment" and "dollar_nuetral" or "active"

  If type="full_investment", min_sum=1 and max_sum=1

  If type="dollar_neutral" or type="active", min_sum=0, and
  max_sum=0
}
\author{
  Ross Bennett
}

