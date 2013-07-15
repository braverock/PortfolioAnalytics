\name{minmax_objective}
\alias{minmax_objective}
\title{constructor for class tmp_minmax_objective}
\usage{
  minmax_objective(name, target = NULL, arguments = NULL,
    multiplier = 1, enabled = TRUE, ..., min, max)
}
\arguments{
  \item{name}{name of the objective, should correspond to a
  function, though we will try to make allowances}

  \item{target}{univariate target for the objective}

  \item{min}{minimum value}

  \item{max}{maximum value}

  \item{arguments}{default arguments to be passed to an
  objective function when executed}

  \item{multiplier}{multiplier to apply to the objective,
  usually 1 or -1}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters}
}
\description{
  I am add this as a temporary objective allowing for a min
  and max to be specified. Testing to understand how the
  objective function responds to a range of allowable
  values. I will likely add this to the turnover,
  diversification, and volatility constraints allowing the
  user to specify a range of values.
}
\details{
  if target is null, we'll try to minimize the metric

  if target is set, we'll try to meet the metric

  If max is violated to the upside, penalize the metric If
  min is violated to the downside, penalize the metric Try
  to meet the range between min and max
}
\author{
  Ross Bennett
}

