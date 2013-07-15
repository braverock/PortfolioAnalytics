\name{portfolio_risk_objective}
\alias{portfolio_risk_objective}
\title{constructor for class portfolio_risk_objective}
\usage{
  portfolio_risk_objective(name, target = NULL,
    arguments = NULL, multiplier = 1, enabled = TRUE, ...)
}
\arguments{
  \item{name}{name of the objective, should correspond to a
  function, though we will try to make allowances}

  \item{target}{univariate target for the objective}

  \item{arguments}{default arguments to be passed to an
  objective function when executed}

  \item{multiplier}{multiplier to apply to the objective,
  usually 1 or -1}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters}
}
\description{
  if target is null, we'll try to minimize the risk metric
}
\author{
  Brian G. Peterson
}

